skip_if_not_installed("processx")
skip_on_cran()
# Deliberately runs under R CMD check (no _R_CHECK_PACKAGE_NAME_ skip): this is
# the regression guard for the Windows stdio async-delivery hang. A daemon
# (mirai) result is posted onto `later`'s queue from a background thread; the
# stdio serve loop must block in `later::run_now(positive)` so that completion
# is delivered to stdout. We exercise three daemon-backed paths over real
# stdio: a successful tools/call, an erroring tools/call, and several
# concurrent tools/call.

# A minimal stdio server with three non-bidirectional (=> mirai-daemon) tools.
async_runner_script <- function() {
  script <- tempfile(fileext = ".R")
  writeLines(c(
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
  ), script)
  script
}

spawn_async_stdio <- function() {
  script <- async_runner_script()
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(), collapse = .Platform$path.sep)
  p <- processx::process$new(
    "Rscript", c(script),
    stdin = "|", stdout = "|", stderr = "|",
    env = child_env)
  list(process = p, script = script)
}

send_msg <- function(p, msg) {
  p$write_input(paste0(jsonlite::toJSON(msg, auto_unbox = TRUE), "\n"))
}

# Persistent line buffer so multiple replies flushed together aren't lost.
new_reader <- function(p) {
  buf <- new.env(parent = emptyenv())
  buf$lines <- character(0L)
  buf$partial <- ""
  buf$p <- p
  buf
}

read_line <- function(buf, timeout_ms = 30000) {
  if (length(buf$lines) > 0L) {
    out <- buf$lines[[1L]]; buf$lines <- buf$lines[-1L]; return(out)
  }
  t0 <- Sys.time()
  while (difftime(Sys.time(), t0, units = "secs") < timeout_ms / 1000) {
    buf$p$poll_io(200)
    chunk <- buf$p$read_output()
    if (nchar(chunk) > 0L) {
      buf$partial <- paste0(buf$partial, chunk)
      if (grepl("\n", buf$partial, fixed = TRUE)) {
        parts <- strsplit(buf$partial, "\n", fixed = TRUE)[[1L]]
        if (endsWith(buf$partial, "\n")) {
          buf$lines <- c(buf$lines, parts); buf$partial <- ""
        } else {
          buf$lines <- c(buf$lines, parts[-length(parts)])
          buf$partial <- parts[[length(parts)]]
        }
        buf$lines <- buf$lines[nzchar(buf$lines)]
        if (length(buf$lines) > 0L) {
          out <- buf$lines[[1L]]; buf$lines <- buf$lines[-1L]; return(out)
        }
      }
    }
    if (!buf$p$is_alive()) break
  }
  NA_character_
}

# Read responses until every id in `ids` is seen (responses can interleave /
# arrive out of order). Returns a list keyed by id, or NULL on timeout.
collect_by_id <- function(buf, ids, timeout_ms = 30000) {
  got <- list()
  t0 <- Sys.time()
  while (length(got) < length(ids) &&
         difftime(Sys.time(), t0, units = "secs") < timeout_ms / 1000) {
    line <- read_line(buf, timeout_ms = timeout_ms)
    if (is.na(line)) break
    parsed <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
                       error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$id)) next
    key <- as.character(parsed$id)
    if (key %in% as.character(ids)) got[[key]] <- parsed
  }
  if (length(got) < length(ids)) return(NULL)
  got
}

initialize_server <- function(srv) {
  buf <- new_reader(srv$process)
  send_msg(srv$process, list(jsonrpc = "2.0", id = 1, method = "initialize",
                             params = list(protocolVersion = "2025-06-18",
                                           capabilities = list())))
  init_line <- read_line(buf, timeout_ms = 30000)
  expect_false(is.na(init_line),
               info = paste("stderr:",
                            paste(srv$process$read_error_lines(),
                                  collapse = " | ")))
  send_msg(srv$process, list(jsonrpc = "2.0",
                             method = "notifications/initialized"))
  buf
}

test_that("daemon-backed tools/call over stdio delivers a successful result", {
  srv <- spawn_async_stdio()
  withr::defer({ srv$process$kill(); unlink(srv$script) })
  buf <- initialize_server(srv)

  send_msg(srv$process, list(jsonrpc = "2.0", id = 10, method = "tools/call",
                             params = list(name = "echo2",
                                           arguments = list(text = "hi"))))
  # Generous timeout for mirai daemon cold-start on a loaded Windows runner;
  # the bug was a permanent hang, so any finite timeout fails fast pre-fix.
  line <- read_line(buf, timeout_ms = 60000)
  expect_false(is.na(line),
               info = paste("stderr:",
                            paste(srv$process$read_error_lines(),
                                  collapse = " | ")))
  resp <- jsonlite::fromJSON(line, simplifyVector = FALSE)
  expect_equal(resp$id, 10)
  expect_equal(resp$result$content[[1L]]$text, "echo2: hi")
})

test_that("an erroring daemon-backed tools/call delivers a JSON-RPC error", {
  srv <- spawn_async_stdio()
  withr::defer({ srv$process$kill(); unlink(srv$script) })
  buf <- initialize_server(srv)

  # The exact shape that hung on Windows: a tools/call whose handler fails in
  # the daemon. The failure must be *delivered* as an error envelope, not lost.
  send_msg(srv$process, list(jsonrpc = "2.0", id = 20, method = "tools/call",
                             params = list(name = "boom",
                                           arguments = list())))
  line <- read_line(buf, timeout_ms = 60000)
  expect_false(is.na(line),
               info = paste("stderr:",
                            paste(srv$process$read_error_lines(),
                                  collapse = " | ")))
  resp <- jsonlite::fromJSON(line, simplifyVector = FALSE)
  expect_equal(resp$id, 20)
  expect_false(is.null(resp$error))
  expect_true(is.numeric(resp$error$code))
  expect_match(resp$error$message, "boom", fixed = TRUE)
})

test_that("concurrent daemon-backed tools/call all return over stdio", {
  srv <- spawn_async_stdio()
  withr::defer({ srv$process$kill(); unlink(srv$script) })
  buf <- initialize_server(srv)

  # Warm the daemon pool first so cold-start doesn't pollute the timing.
  send_msg(srv$process, list(jsonrpc = "2.0", id = 100, method = "tools/call",
                             params = list(name = "slow", arguments = list())))
  warm <- read_line(buf, timeout_ms = 60000)
  expect_false(is.na(warm))

  ids <- 201:208
  t0 <- Sys.time()
  for (id in ids) {
    send_msg(srv$process, list(jsonrpc = "2.0", id = id, method = "tools/call",
                               params = list(name = "slow",
                                             arguments = list())))
  }
  got <- collect_by_id(buf, ids, timeout_ms = 30000)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  # Every concurrent call must come back (the delivery that hung on Windows).
  expect_false(is.null(got),
               info = paste("stderr:",
                            paste(srv$process$read_error_lines(),
                                  collapse = " | ")))
  expect_equal(length(got), length(ids))
  for (id in ids) {
    expect_equal(got[[as.character(id)]]$result$content[[1L]]$text, "slept")
  }
  # Eight 0.5s sleeps over four daemons = two rounds (~1s); serial would be
  # ~4s. A generous 3s bound is decisive evidence the pool runs in parallel.
  expect_lt(elapsed, 3)
})
