skip_if_not_installed("processx")
skip_if_not_installed("httr2")
skip_on_cran()
skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))

spawn_minimal_http <- function(port) {
  runner_script <- tempfile(fileext = ".R")
  log_path <- tempfile(fileext = ".log")
  writeLines(c(
    "log_path <- Sys.getenv('MCPSERVER_LOG', '')",
    "if (nzchar(log_path)) sink(file(log_path, open='a'), type='message')",
    "suppressPackageStartupMessages(library(mcpserver))",
    "srv <- new_server('h', version = '0.1.0')",
    "add_capability(srv, new_tool(",
    "  'echo', 'echo', schema(list(",
    "    message = property_string(required = TRUE))),",
    "  handler = function(args, ctx) response_text(args$message)))",
    sprintf("serve_http(srv, port = %dL, allowed_origins = c('http://127.0.0.1'))",
            port)
  ), runner_script)
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                               collapse = .Platform$path.sep)
  child_env["MCPSERVER_LOG"] <- log_path
  p <- processx::process$new("Rscript", c(runner_script),
                             stdout = "|", stderr = "|", env = child_env)
  Sys.sleep(3)
  url <- sprintf("http://127.0.0.1:%d/mcp", port)
  init <- tryCatch(
    httr2::request(url) |>
      httr2::req_method("POST") |>
      httr2::req_headers(Origin = "http://127.0.0.1",
                         `Content-Type` = "application/json") |>
      httr2::req_body_raw(charToRaw(
        '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}')) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_timeout(5) |>
      httr2::req_perform(),
    error = function(e) NULL)
  list(process = p, url = url,
       sid = if (!is.null(init)) httr2::resp_header(init, "Mcp-Session-Id"),
       runner_script = runner_script, log_path = log_path)
}

test_that("bad MCP-Protocol-Version returns 400", {
  srv <- spawn_minimal_http(42501L)
  withr::defer({ srv$process$kill(); unlink(srv$runner_script) })
  if (is.null(srv$sid) || !nzchar(srv$sid)) skip("server did not start")
  resp <- httr2::request(srv$url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Origin = "http://127.0.0.1",
                       `Content-Type` = "application/json",
                       `Mcp-Session-Id` = srv$sid,
                       `MCP-Protocol-Version` = "2099-01-01") |>
    httr2::req_body_raw(charToRaw(
      '{"jsonrpc":"2.0","id":2,"method":"tools/list"}')) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(5) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 400L)
})

test_that("missing Origin with require_origin=TRUE is rejected with 403", {
  srv <- spawn_minimal_http(42502L)
  withr::defer({ srv$process$kill(); unlink(srv$runner_script) })
  if (is.null(srv$sid) || !nzchar(srv$sid)) skip("server did not start")
  resp <- httr2::request(srv$url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_raw(charToRaw("{}")) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(5) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 403L)
})

test_that("DELETE without MCP-Protocol-Version is accepted; bad version rejected", {
  srv <- spawn_minimal_http(42503L)
  withr::defer({ srv$process$kill(); unlink(srv$runner_script) })
  if (is.null(srv$sid) || !nzchar(srv$sid)) skip("server did not start")
  bad <- httr2::request(srv$url) |>
    httr2::req_method("DELETE") |>
    httr2::req_headers(Origin = "http://127.0.0.1",
                       `Mcp-Session-Id` = srv$sid,
                       `MCP-Protocol-Version` = "wrong") |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(5) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(bad), 400L)
})
