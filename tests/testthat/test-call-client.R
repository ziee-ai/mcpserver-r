# Unit tests for call_client_blocking and the pending-request cap.

test_that("call_client_blocking enforces the per-session pending cap", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  s$pending_cap <- 2L
  for (i in 1:2) {
    assign(as.character(-i),
           list(cv = NULL,
                env = new.env(parent = emptyenv()),
                deadline = NA_real_),
           envir = s$pending)
  }
  err <- tryCatch(
    mcpserver:::call_client_blocking(s, "x/y", NULL, timeout = 1),
    error = function(e) conditionMessage(e))
  expect_match(err, "pending server->client request cap")
})

test_that("call_client_blocking times out cleanly when no response arrives", {
  srv <- new_server("t")
  sent <- new.env(parent = emptyenv()); sent$messages <- list()
  s <- mcpserver:::Session$new("t", srv, function(e) {
    sent$messages <- c(sent$messages, list(e))
  })
  err <- tryCatch(
    mcpserver:::call_client_blocking(s, "test/method",
                                     params = NULL, timeout = 0.3),
    error = function(e) conditionMessage(e))
  expect_match(err, "timed out")
  expect_equal(length(sent$messages), 1L)
  expect_equal(sent$messages[[1L]]$method, "test/method")
})
