skip_if_not_installed("processx")
skip_if_not_installed("httr2")
skip_on_cran()
skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))

spawn_minimal_http <- function(port, auth_args = "") {
  runner_script <- tempfile(fileext = ".R")
  writeLines(c(
    "suppressPackageStartupMessages(library(mcpserver))",
    "srv <- new_server('h', version = '0.1.0')",
    "add_capability(srv, new_tool(",
    "  'echo', 'echo', schema(list(",
    "    text = property_string(required = TRUE))),",
    "  handler = function(args, ctx) response_text(args$text)))",
    sprintf("serve_http(srv, port = %dL, allowed_origins = c('http://127.0.0.1')%s)",
            port, auth_args)
  ), runner_script)
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                               collapse = .Platform$path.sep)
  p <- processx::process$new("Rscript", c(runner_script),
                             stdout = "|", stderr = "|", env = child_env)
  Sys.sleep(3)
  url <- sprintf("http://127.0.0.1:%d/mcp", port)
  init <- tryCatch(
    httr2::request(url) |>
      httr2::req_method("POST") |>
      httr2::req_headers(Origin = "http://127.0.0.1",
                         `Content-Type` = "application/json",
                         Accept = "application/json, text/event-stream") |>
      httr2::req_body_raw(charToRaw(
        '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}')) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_timeout(5) |>
      httr2::req_perform(),
    error = function(e) NULL)
  list(process = p, url = url, runner_script = runner_script,
       sid = if (!is.null(init)) httr2::resp_header(init, "Mcp-Session-Id"))
}

test_that("PUT / PATCH / HEAD on /mcp return 405 with Allow header", {
  srv <- spawn_minimal_http(43700L)
  withr::defer({ srv$process$kill(); unlink(srv$runner_script) })
  if (is.null(srv$sid)) skip("server did not start")
  for (m in c("PUT", "PATCH", "HEAD")) {
    resp <- httr2::request(srv$url) |>
      httr2::req_method(m) |>
      httr2::req_headers(Origin = "http://127.0.0.1") |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_timeout(5) |>
      httr2::req_perform()
    expect_equal(httr2::resp_status(resp), 405L,
                 info = paste("method:", m))
    allow <- httr2::resp_header(resp, "Allow")
    expect_true(!is.null(allow) && nzchar(allow),
                info = paste("Allow header missing for method:", m))
    expect_true(grepl("POST", allow) && grepl("GET", allow) &&
                grepl("DELETE", allow))
  }
})

test_that("/.well-known/oauth-protected-resource returns AS metadata when auth is configured", {
  # The metadata endpoint must be public; we don't initialize an MCP
  # session, we just hit the well-known URL directly.
  runner_script <- tempfile(fileext = ".R")
  withr::defer(unlink(runner_script))
  writeLines(c(
    "suppressPackageStartupMessages(library(mcpserver))",
    "srv <- new_server('h', version='0.1.0')",
    "add_capability(srv, new_tool('echo','e',schema(list()),",
    "  handler=function(args,ctx) response_text('ok')))",
    "serve_http(srv, port = 43701L,",
    "  allowed_origins = c('http://127.0.0.1'),",
    "  auth = oauth_config(",
    "    issuer = 'https://issuer.example',",
    "    audience = 'https://api.example',",
    "    jwks_uri = 'https://issuer.example/jwks',",
    "    required_scopes = c('mcp:tools')))"
  ), runner_script)
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                               collapse = .Platform$path.sep)
  p <- processx::process$new("Rscript", c(runner_script),
                             stdout = "|", stderr = "|", env = child_env)
  withr::defer(p$kill())
  Sys.sleep(3)
  url <- "http://127.0.0.1:43701/.well-known/oauth-protected-resource"
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_timeout(5) |>
      httr2::req_perform(),
    error = function(e) NULL)
  if (is.null(resp)) skip("server did not start")
  expect_equal(httr2::resp_status(resp), 200L)
  body <- jsonlite::fromJSON(httr2::resp_body_string(resp),
                             simplifyVector = FALSE)
  expect_equal(body$resource, "https://api.example")
  expect_true("https://issuer.example" %in%
                unlist(body$authorization_servers))
  expect_true("mcp:tools" %in% unlist(body$scopes_supported))
})

test_that("HTTP POST accepts a JSON-RPC batch and replies with a JSON array", {
  srv <- spawn_minimal_http(43702L)
  withr::defer({ srv$process$kill(); unlink(srv$runner_script) })
  if (is.null(srv$sid)) skip("server did not start")
  body <- '[
    {"jsonrpc":"2.0","id":10,"method":"tools/list"},
    {"jsonrpc":"2.0","id":11,"method":"ping"}
  ]'
  resp <- httr2::request(srv$url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Origin = "http://127.0.0.1",
                       `Mcp-Session-Id` = srv$sid,
                       `Content-Type` = "application/json",
                       `MCP-Protocol-Version` = "2025-06-18",
                       Accept = "application/json, text/event-stream") |>
    httr2::req_body_raw(charToRaw(body)) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(10) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 200L)
  arr <- jsonlite::fromJSON(httr2::resp_body_string(resp),
                            simplifyVector = FALSE)
  expect_true(is.list(arr) && is.null(names(arr)))
  expect_equal(length(arr), 2L)
  ids <- vapply(arr, function(x) as.integer(x$id), integer(1L))
  expect_setequal(ids, c(10L, 11L))
})
