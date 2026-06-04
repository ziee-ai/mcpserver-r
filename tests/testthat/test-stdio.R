skip_if_not_installed("processx")
skip_on_cran()
# NB: this test deliberately runs under R CMD check (no _R_CHECK_PACKAGE_NAME_
# skip). It exercises the stdio daemon-backed tools/call path, which hung on
# Windows; keeping it under check is the regression guard. Shared
# spawn/JSON-RPC/retry helpers live in helper-stdio.R; stdio_with_retry
# respawns on a transient no-response so a slow CI runner can't redden the
# build, while a real delivery hang still fails (it times out every attempt).

test_that("stdio transport replies to initialize, tools/list and tools/call", {
  runner <- system.file("everything", "run-stdio.R",
                        package = "mcpserver")
  skip_if(!nzchar(runner) || !file.exists(runner),
          "run-stdio.R not installed")

  # Drive the shipped stdio entrypoint (run-stdio.R -> serve_io).
  runner_lines <- sprintf("source(%s)", deparse(runner))

  out <- stdio_with_retry(runner_lines, function(srv, buf) {
    stdio_send(srv$process, list(jsonrpc = "2.0", id = 2, method = "tools/list"))
    tl_line <- stdio_readline(buf, timeout_ms = 30000)
    if (is.na(tl_line)) return(NULL)
    tl <- jsonlite::fromJSON(tl_line, simplifyVector = FALSE)

    stdio_send(srv$process, list(jsonrpc = "2.0", id = 3, method = "tools/call",
                                 params = list(name = "echo",
                                               arguments = list(message = "stdio works"))))
    tc_line <- stdio_readline(buf, timeout_ms = 60000)
    if (is.na(tc_line)) return(NULL)
    list(tl = tl, tc = jsonlite::fromJSON(tc_line, simplifyVector = FALSE))
  })

  names <- vapply(out$tl$result$tools, function(t) t$name, character(1L))
  expect_true("echo" %in% names)
  expect_equal(out$tc$result$content[[1L]]$text, "Echo: stdio works")
})
