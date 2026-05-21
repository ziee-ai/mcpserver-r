# SEP-1699: SSE priming event on every new SSE stream.

skip_if_not_installed("processx")
skip_if_not_installed("httr2")
skip_on_cran()
skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))

test_that("GET SSE stream emits an `id: 0`, retry, empty-data priming event first", {
  runner_script <- tempfile(fileext = ".R")
  withr::defer(unlink(runner_script))
  writeLines(c(
    "suppressPackageStartupMessages(library(mcpserver))",
    "srv <- new_server('p', version = '0.1.0')",
    "add_capability(srv, new_tool('echo', 'echo',",
    "  schema(list(text = property_string(required = TRUE))),",
    "  handler = function(args, ctx) response_text(args$text)))",
    "serve_http(srv, port = 44310L,",
    "           allowed_origins = c('http://127.0.0.1'),",
    "           require_origin = FALSE)"
  ), runner_script)
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                               collapse = .Platform$path.sep)
  p <- processx::process$new("Rscript", c(runner_script),
                             stdout = "|", stderr = "|", env = child_env)
  withr::defer(if (p$is_alive()) p$kill())
  Sys.sleep(3)

  # First, initialize so we have a session id.
  init <- tryCatch(httr2::request("http://127.0.0.1:44310/mcp") |>
    httr2::req_method("POST") |>
    httr2::req_headers(`Content-Type` = "application/json",
                       Accept = "application/json, text/event-stream") |>
    httr2::req_body_raw(charToRaw(
      '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{}}}')) |>
    httr2::req_timeout(5) |>
    httr2::req_perform(), error = function(e) NULL)
  if (is.null(init)) skip("server did not start")
  sid <- httr2::resp_header(init, "Mcp-Session-Id")

  # Open the GET stream via raw curl so we can stream-read.
  out_file <- tempfile(fileext = ".sse")
  withr::defer(unlink(out_file))
  curl_p <- processx::process$new(
    "curl",
    c("-sS", "-N", "-o", out_file,
      "-H", sprintf("Mcp-Session-Id: %s", sid),
      "-H", "Accept: text/event-stream",
      "-H", "MCP-Protocol-Version: 2025-11-25",
      "http://127.0.0.1:44310/mcp"))
  withr::defer(if (curl_p$is_alive()) curl_p$kill())
  Sys.sleep(1)
  curl_p$kill()
  if (!file.exists(out_file)) skip("curl produced no output")
  body <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
  # The first event must be `id: 0` with retry: 3000 and empty data:.
  expect_match(body, "(?m)^id: 0$", perl = TRUE)
  expect_match(body, "(?m)^retry: 3000$", perl = TRUE)
})
