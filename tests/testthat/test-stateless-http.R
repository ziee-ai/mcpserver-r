# Ports test/integration/test/stateManagementStreamableHttp.test.ts
# "Stateless Mode" suite.

skip_if_not_installed("processx")
skip_if_not_installed("httr2")
skip_on_cran()
skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))

spawn_stateless <- function(port) {
  runner_script <- tempfile(fileext = ".R")
  writeLines(c(
    "suppressPackageStartupMessages(library(mcpserver))",
    "srv <- new_server('stateless', version = '0.1.0')",
    "add_capability(srv, new_tool('echo', 'echo',",
    "  schema(list(text = property_string(required = TRUE))),",
    "  handler = function(args, ctx) response_text(args$text)))",
    sprintf("serve_http(srv, port = %dL, stateless = TRUE,", port),
    "             allowed_origins = c('http://127.0.0.1'))"
  ), runner_script)
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                               collapse = .Platform$path.sep)
  p <- processx::process$new("Rscript", c(runner_script),
                             stdout = "|", stderr = "|", env = child_env)
  Sys.sleep(3)
  list(process = p, url = sprintf("http://127.0.0.1:%d/mcp", port),
       runner_script = runner_script)
}

post <- function(srv, body, headers = c()) {
  default <- c(Origin = "http://127.0.0.1",
               `Content-Type` = "application/json",
               Accept = "application/json, text/event-stream")
  hdr <- c(default, headers)
  hdr_args <- as.list(hdr); names(hdr_args) <- names(hdr)
  do.call(httr2::req_headers,
          c(list(httr2::req_method(httr2::request(srv$url), "POST")),
            hdr_args)) |>
    httr2::req_body_raw(charToRaw(body)) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(5) |>
    httr2::req_perform()
}

test_that("stateless mode handles initialize without allocating a session id", {
  srv <- spawn_stateless(43900L)
  withr::defer({ srv$process$kill(); unlink(srv$runner_script) })
  resp <- post(srv,
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}')
  expect_equal(httr2::resp_status(resp), 200L)
  # No Mcp-Session-Id header in stateless mode.
  sid <- httr2::resp_header(resp, "Mcp-Session-Id")
  expect_true(is.null(sid) || identical(sid, ""))
  body <- jsonlite::fromJSON(httr2::resp_body_string(resp),
                             simplifyVector = FALSE)
  expect_equal(body$result$protocolVersion, "2025-06-18")
})

test_that("stateless mode dispatches subsequent requests without session id", {
  srv <- spawn_stateless(43901L)
  withr::defer({ srv$process$kill(); unlink(srv$runner_script) })
  # Initialize (also stateless)
  post(srv,
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}')
  # tools/list without a session id should still work
  resp <- post(srv, '{"jsonrpc":"2.0","id":2,"method":"tools/list"}')
  expect_equal(httr2::resp_status(resp), 200L)
  body <- jsonlite::fromJSON(httr2::resp_body_string(resp),
                             simplifyVector = FALSE)
  names <- vapply(body$result$tools, function(t) t$name, character(1L))
  expect_true("echo" %in% names)
})

test_that("stateless mode dispatches tool calls with their own ephemeral session", {
  srv <- spawn_stateless(43902L)
  withr::defer({ srv$process$kill(); unlink(srv$runner_script) })
  post(srv,
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}')
  resp <- post(srv,
    '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"echo","arguments":{"text":"hi"}}}')
  expect_equal(httr2::resp_status(resp), 200L)
  body <- jsonlite::fromJSON(httr2::resp_body_string(resp),
                             simplifyVector = FALSE)
  expect_equal(body$result$content[[1L]]$text, "hi")
})
