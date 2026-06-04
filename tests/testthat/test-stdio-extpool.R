skip_if_not_installed("processx")
skip_on_cran()
# Regression guard for the SECOND Windows stdio hang: when the CALLER creates
# the mirai daemon pool itself (via mirai::daemons()) BEFORE serve_io() — as
# the downstream rcpa.mcpserver does, to load extra packages into the daemons —
# serve_io()'s ensure_daemons() must ADOPT that pool, not re-initialise it.
# Re-calling mirai::daemons(n) on a live pool resets it onto fresh sockets,
# which breaks mirai->later delivery on Windows and hangs every daemon-backed
# tools/call. This mirrors rcpa-mcpserver/R/stdio_server.R's setup. Shared
# spawn/JSON-RPC helpers live in helper-stdio.R.

# Caller pre-creates and pre-loads the daemon pool, THEN calls serve_io().
extpool_runner_lines <- function() {
  c(
    "suppressPackageStartupMessages(library(mcpserver))",
    "mirai::daemons(4L)",                                  # caller owns the pool
    "mirai::everywhere(suppressPackageStartupMessages(library(mcpserver)))",
    "srv <- new_server('stdio-extpool', version = '0.1.0')",
    "add_capability(srv, new_tool(",
    "  name = 'echo2', description = 'Echo back text',",
    "  input_schema = schema(list(text = property_string('text'))),",
    "  handler = function(args, ctx) response_text(paste0('echo2: ', args$text))))",
    "add_capability(srv, new_tool(",
    "  name = 'boom', description = 'Always errors',",
    "  input_schema = schema(list()),",
    "  handler = function(args, ctx) stop('boom from handler')))",
    "serve_io(srv, daemons = 4L)",                         # must adopt, not reset
    "mirai::daemons(0L)"
  )
}

test_that("daemon-backed tools/call returns when the caller pre-created the pool", {
  srv <- stdio_spawn(extpool_runner_lines())
  withr::defer({ srv$process$kill(); unlink(srv$script) })
  buf <- stdio_init(srv)

  stdio_send(srv$process, list(jsonrpc = "2.0", id = 10, method = "tools/call",
                               params = list(name = "echo2",
                                             arguments = list(text = "ext"))))
  line <- stdio_readline(buf, timeout_ms = 60000)
  expect_false(is.na(line),
               info = paste("stderr:",
                            paste(srv$process$read_error_lines(),
                                  collapse = " | ")))
  resp <- jsonlite::fromJSON(line, simplifyVector = FALSE)
  expect_equal(resp$id, 10)
  expect_equal(resp$result$content[[1L]]$text, "echo2: ext")
})

test_that("an erroring tools/call is delivered (not hung) with a pre-created pool", {
  # Mirrors rcpa's empty-arg validate_input_file case that hung on Windows.
  srv <- stdio_spawn(extpool_runner_lines())
  withr::defer({ srv$process$kill(); unlink(srv$script) })
  buf <- stdio_init(srv)

  stdio_send(srv$process, list(jsonrpc = "2.0", id = 20, method = "tools/call",
                               params = list(name = "boom",
                                             arguments = list())))
  line <- stdio_readline(buf, timeout_ms = 60000)
  expect_false(is.na(line),
               info = paste("stderr:",
                            paste(srv$process$read_error_lines(),
                                  collapse = " | ")))
  resp <- jsonlite::fromJSON(line, simplifyVector = FALSE)
  expect_equal(resp$id, 20)
  expect_false(is.null(resp$error))
  expect_match(resp$error$message, "boom", fixed = TRUE)
})
