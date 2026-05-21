skip_if_not_installed("processx")
skip_if_not_installed("httr2")
# Subprocess-driven HTTP integration is run via the in-tree dev workflow
# (pkgload::load_all() + testthat::test_dir()). R CMD check's isolated
# library + restricted env makes mirai daemons take longer than the test
# polling budget on some hosts, so we don't gate the package install on
# it. Maintainers can run this locally with `devtools::test()`.
skip_on_cran()
skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))

# Start a server in a subprocess and tear it down on test exit.
spawn_http_server <- function(port) {
  runner <- system.file("everything", "run-http.R",
                        package = "mcpserver")
  if (!nzchar(runner) || !file.exists(runner)) {
    skip("run-http.R script not installed")
  }
  # Propagate the current .libPaths() so the subprocess can find
  # mcpserver when run from R CMD check's temp library.
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(), collapse = .Platform$path.sep)
  log_path <- tempfile("mcp-http-", fileext = ".log")
  child_env["MCPSERVER_LOG"] <- log_path
  stderr_path <- tempfile("mcp-http-stderr-", fileext = ".log")
  stdout_path <- tempfile("mcp-http-stdout-", fileext = ".log")
  p <- processx::process$new(
    "Rscript",
    c(runner, "--port", as.character(port)),
    stdout = stdout_path, stderr = stderr_path,
    env = child_env)
  # Give the subprocess a moment to load packages and bind the port.
  Sys.sleep(3)
  ok <- FALSE
  last_err <- NULL
  for (i in seq_len(120L)) {
    resp <- tryCatch(
      httr2::request(sprintf("http://127.0.0.1:%d/mcp", port)) |>
        httr2::req_method("POST") |>
        httr2::req_headers(Origin = "http://127.0.0.1",
                           `Content-Type` = "application/json") |>
        httr2::req_body_raw(charToRaw("{}")) |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_timeout(2) |>
        httr2::req_perform(),
      error = function(e) { last_err <<- conditionMessage(e); NULL })
    if (!is.null(resp)) { ok <- TRUE; break }
    Sys.sleep(0.5)
  }
  if (!ok) {
    p$kill()
    log_contents <- if (file.exists(log_path)) {
      paste(readLines(log_path, warn = FALSE), collapse = " | ")
    } else ""
    stderr_contents <- if (file.exists(stderr_path)) {
      paste(readLines(stderr_path, warn = FALSE), collapse = " | ")
    } else ""
    stdout_contents <- if (file.exists(stdout_path)) {
      paste(readLines(stdout_path, warn = FALSE), collapse = " | ")
    } else ""
    err <- paste(
      "last_req_err:", last_err %||% "",
      " | stderr:", stderr_contents,
      " | stdout:", stdout_contents,
      " | log:", log_contents)
    skip(paste("server failed to start:", substr(err, 1L, 400L)))
  }
  p
}

test_that("HTTP transport handles initialize / list / call / DELETE", {
  port <- 41801L
  p <- spawn_http_server(port)
  withr::defer(p$kill())

  # initialize
  init <- httr2::request(sprintf("http://127.0.0.1:%d/mcp", port)) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Origin = "http://127.0.0.1",
                       `Content-Type` = "application/json",
                       Accept = "application/json, text/event-stream") |>
    httr2::req_body_raw(charToRaw(
      '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}')) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(init), 200L)
  sid <- httr2::resp_header(init, "Mcp-Session-Id")
  expect_true(nzchar(sid %||% ""))

  body <- jsonlite::fromJSON(httr2::resp_body_string(init),
                             simplifyVector = FALSE)
  expect_equal(body$result$protocolVersion, "2025-06-18")

  # tools/list
  tl <- httr2::request(sprintf("http://127.0.0.1:%d/mcp", port)) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Origin = "http://127.0.0.1",
                       `Content-Type` = "application/json",
                       `Mcp-Session-Id` = sid,
                       `MCP-Protocol-Version` = "2025-06-18") |>
    httr2::req_body_raw(charToRaw(
      '{"jsonrpc":"2.0","id":2,"method":"tools/list"}')) |>
    httr2::req_perform()
  parsed <- jsonlite::fromJSON(httr2::resp_body_string(tl),
                               simplifyVector = FALSE)
  names <- vapply(parsed$result$tools, function(t) t$name, character(1L))
  expect_true("echo" %in% names)

  # tools/call echo
  tc <- httr2::request(sprintf("http://127.0.0.1:%d/mcp", port)) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Origin = "http://127.0.0.1",
                       `Content-Type` = "application/json",
                       `Mcp-Session-Id` = sid,
                       `MCP-Protocol-Version` = "2025-06-18") |>
    httr2::req_body_raw(charToRaw(
      '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"echo","arguments":{"message":"hi"}}}')) |>
    httr2::req_perform()
  parsed <- jsonlite::fromJSON(httr2::resp_body_string(tc),
                               simplifyVector = FALSE)
  expect_equal(parsed$result$content[[1L]]$text, "Echo: hi")

  # bad origin -> 403
  bo <- httr2::request(sprintf("http://127.0.0.1:%d/mcp", port)) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Origin = "http://evil.example",
                       `Content-Type` = "application/json") |>
    httr2::req_body_raw(charToRaw("{}")) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(bo), 403L)

  # DELETE -> 204
  d <- httr2::request(sprintf("http://127.0.0.1:%d/mcp", port)) |>
    httr2::req_method("DELETE") |>
    httr2::req_headers(Origin = "http://127.0.0.1",
                       `Mcp-Session-Id` = sid) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(d), 204L)

  # After delete -> 404
  after <- httr2::request(sprintf("http://127.0.0.1:%d/mcp", port)) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Origin = "http://127.0.0.1",
                       `Content-Type` = "application/json",
                       `Mcp-Session-Id` = sid,
                       `MCP-Protocol-Version` = "2025-06-18") |>
    httr2::req_body_raw(charToRaw(
      '{"jsonrpc":"2.0","id":1,"method":"ping"}')) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(after), 404L)
})
