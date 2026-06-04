skip_if_not_installed("processx")
skip_on_cran()
# Deliberately runs under R CMD check (no _R_CHECK_PACKAGE_NAME_ skip): this is
# the regression guard for the Windows stdio async-delivery hang. A daemon
# (mirai) result is posted onto `later`'s queue from a background thread; the
# stdio serve loop must block in `later::run_now(positive)` so that completion
# is delivered to stdout. We exercise three daemon-backed paths over real
# stdio: a successful tools/call, an erroring tools/call, and several
# concurrent tools/call. Shared spawn/JSON-RPC/retry helpers live in
# helper-stdio.R; stdio_with_retry respawns on a transient no-response so a
# slow CI runner can't redden the build, while a real hang still fails (it
# times out on every attempt).

# A minimal stdio server with three non-bidirectional (=> mirai-daemon) tools.
# Here serve_io() OWNS the daemon pool (see test-stdio-extpool.R for the
# caller-created-pool case).
async_runner_lines <- function() {
  c(
    "suppressPackageStartupMessages(library(mcpserver))",
    "srv <- new_server('stdio-async', version = '0.1.0')",
    "add_capability(srv, new_tool(",
    "  name = 'echo2', description = 'Echo back text',",
    "  input_schema = schema(list(text = property_string('text'))),",
    "  handler = function(args, ctx) response_text(paste0('echo2: ', args$text))))",
    "add_capability(srv, new_tool(",
    "  name = 'boom', description = 'Always errors',",
    "  input_schema = schema(list()),",
    "  handler = function(args, ctx) stop('boom from handler')))",
    "add_capability(srv, new_tool(",
    "  name = 'slow', description = 'Sleeps then returns',",
    "  input_schema = schema(list()),",
    "  handler = function(args, ctx) { Sys.sleep(0.5); response_text('slept') }))",
    "serve_io(srv)"
  )
}

test_that("daemon-backed tools/call over stdio delivers a successful result", {
  resp <- stdio_with_retry(async_runner_lines(), function(srv, buf) {
    stdio_send(srv$process, list(jsonrpc = "2.0", id = 10, method = "tools/call",
                                 params = list(name = "echo2",
                                               arguments = list(text = "hi"))))
    # Generous timeout for mirai daemon cold-start on a loaded runner; a real
    # hang times out on every attempt, a transient slow start gets a respawn.
    line <- stdio_readline(buf, timeout_ms = 60000)
    if (is.na(line)) return(NULL)
    jsonlite::fromJSON(line, simplifyVector = FALSE)
  })
  expect_equal(resp$id, 10)
  expect_equal(resp$result$content[[1L]]$text, "echo2: hi")
})

test_that("an erroring daemon-backed tools/call delivers a JSON-RPC error", {
  resp <- stdio_with_retry(async_runner_lines(), function(srv, buf) {
    # A tools/call whose handler fails in the daemon. The failure must be
    # *delivered* as an error envelope, not lost into a hang.
    stdio_send(srv$process, list(jsonrpc = "2.0", id = 20, method = "tools/call",
                                 params = list(name = "boom",
                                               arguments = list())))
    line <- stdio_readline(buf, timeout_ms = 60000)
    if (is.na(line)) return(NULL)
    jsonlite::fromJSON(line, simplifyVector = FALSE)
  })
  expect_equal(resp$id, 20)
  expect_false(is.null(resp$error))
  expect_true(is.numeric(resp$error$code))
  expect_match(resp$error$message, "boom", fixed = TRUE)
})

test_that("concurrent daemon-backed tools/call all return over stdio", {
  ids <- 201:208
  out <- stdio_with_retry(async_runner_lines(), function(srv, buf) {
    # Warm the daemon pool first so cold-start doesn't pollute the timing.
    stdio_send(srv$process, list(jsonrpc = "2.0", id = 100,
                                 method = "tools/call",
                                 params = list(name = "slow",
                                               arguments = list())))
    if (is.na(stdio_readline(buf, timeout_ms = 60000))) return(NULL)

    t0 <- Sys.time()
    for (id in ids) {
      stdio_send(srv$process, list(jsonrpc = "2.0", id = id,
                                   method = "tools/call",
                                   params = list(name = "slow",
                                                 arguments = list())))
    }
    got <- stdio_collect_by_id(buf, ids, timeout_ms = 30000)
    if (is.null(got)) return(NULL)
    list(got = got,
         elapsed = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  })

  expect_equal(length(out$got), length(ids))
  for (id in ids) {
    expect_equal(out$got[[as.character(id)]]$result$content[[1L]]$text, "slept")
  }
  # Eight 0.5s sleeps over four daemons = two rounds (~1s); serial would be
  # ~4s. A generous 3s bound is decisive evidence the pool runs in parallel.
  expect_lt(out$elapsed, 3)
})
