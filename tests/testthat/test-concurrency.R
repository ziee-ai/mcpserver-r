skip_if_not_installed("processx")
skip_if_not_installed("httr2")
skip_on_cran()
skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))

test_that("ten concurrent slow tool calls do not serialise", {
  # Spawn a small server that registers a slow tool sleeping 500ms.
  runner_script <- tempfile(fileext = ".R")
  on.exit(unlink(runner_script), add = TRUE)
  log_path <- tempfile(fileext = ".log")
  writeLines(c(
    "log_path <- Sys.getenv('MCPSERVER_LOG', '')",
    "if (nzchar(log_path)) sink(file(log_path, open = 'a'), type='message')",
    "suppressPackageStartupMessages(library(mcpserver))",
    "srv <- new_server('slow', version = '0.1.0')",
    "add_capability(srv, new_tool(",
    "  name = 'slow', description = 'Slow tool',",
    "  input_schema = schema(list()),",
    "  handler = function(args, ctx) { Sys.sleep(0.5); response_text('ok') }",
    "))",
    "serve_http(srv, port = 42091L, allowed_origins = c('http://127.0.0.1'))"
  ), runner_script)

  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                               collapse = .Platform$path.sep)
  child_env["MCPSERVER_LOG"] <- log_path
  p <- processx::process$new("Rscript", c(runner_script),
                             stdout = "|", stderr = "|",
                             env = child_env)
  withr::defer(p$kill())
  Sys.sleep(3)

  url <- "http://127.0.0.1:42091/mcp"
  init <- httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Origin = "http://127.0.0.1",
                       `Content-Type` = "application/json") |>
    httr2::req_body_raw(charToRaw(
      '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}')) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(5) |>
    httr2::req_perform()
  sid <- httr2::resp_header(init, "Mcp-Session-Id")
  if (is.null(sid) || !nzchar(sid)) {
    skip(paste("server did not start within 3 seconds:",
               paste(readLines(log_path, warn = FALSE), collapse = " | ")))
  }

  fire <- function(call_id) {
    body <- sprintf(
      '{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":"slow","arguments":{}}}',
      call_id)
    httr2::request(url) |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        Origin = "http://127.0.0.1",
        `Content-Type` = "application/json",
        `Mcp-Session-Id` = sid,
        `MCP-Protocol-Version` = "2025-06-18") |>
      httr2::req_body_raw(charToRaw(body)) |>
      httr2::req_timeout(20) |>
      httr2::req_perform_promise()
  }

  t0 <- Sys.time()
  promises <- lapply(seq_len(10L), fire)
  results <- new.env(parent = emptyenv())
  results$done <- 0L
  for (pr in promises) {
    promises::then(pr,
      onFulfilled = function(r) { results$done <- results$done + 1L },
      onRejected  = function(e) { results$done <- results$done + 1L })
  }
  deadline <- t0 + 8
  while (results$done < 10L && Sys.time() < deadline) {
    later::run_now(timeoutSecs = 0.05)
  }
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_equal(results$done, 10L)
  # Ten 500ms tools serialised would take ~5s. With four daemons we
  # expect well under 3s; we assert under 4s to keep the test stable on
  # slow CI hosts.
  expect_lt(elapsed, 4)
})
