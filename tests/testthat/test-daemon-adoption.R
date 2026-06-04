skip_on_cran()
skip_if_not_installed("mirai")

# Fast, cross-platform unit tests for the daemon-pool adoption logic in
# ensure_daemons()/stop_daemons(). The Windows hang is a downstream symptom of
# the pool *reset* these tests detect directly: re-calling mirai::daemons(n) on
# a live pool changes its dispatcher URL, which breaks mirai->later delivery on
# Windows. Asserting the URL is unchanged (adopt, no reset) is decisive and
# observable on every platform.

reset_daemon_state <- function() {
  mcpserver:::stop_daemons()
  suppressWarnings(try(mirai::daemons(0L), silent = TRUE))
}

test_that("ensure_daemons adopts a caller-created pool instead of resetting it", {
  reset_daemon_state()
  mirai::daemons(2L)
  withr::defer(reset_daemon_state())
  url_before <- mirai::status()$daemons
  expect_true(is.character(url_before) && nzchar(url_before))

  # serve_io(daemons = 4) reaches here with the caller's pool already live.
  mcpserver:::ensure_daemons(4L)
  url_after <- mirai::status()$daemons

  # Same pool — a re-init would change the dispatcher URL.
  expect_identical(url_after, url_before)
  expect_true(isTRUE(mcpserver:::.mcp_state$daemons_external))

  # Async work still resolves on the adopted pool.
  val <- NULL
  promises::then(mcpserver:::with_mirai(quote(40L + 2L)),
                 onFulfilled = function(v) val <<- v)
  deadline <- Sys.time() + 10
  while (is.null(val) && Sys.time() < deadline) later::run_now(0.05)
  expect_equal(val, 42L)
})

test_that("a subsequent ensure_daemons never re-inits an adopted pool", {
  reset_daemon_state()
  mirai::daemons(2L)
  withr::defer(reset_daemon_state())
  mcpserver:::ensure_daemons(2L)
  url1 <- mirai::status()$daemons

  # Even a larger request must not reset a pool we do not own.
  mcpserver:::ensure_daemons(8L)
  expect_identical(mirai::status()$daemons, url1)
  expect_true(isTRUE(mcpserver:::.mcp_state$daemons_external))
})

test_that("stop_daemons leaves a caller-created pool running", {
  reset_daemon_state()
  mirai::daemons(2L)
  withr::defer(suppressWarnings(try(mirai::daemons(0L), silent = TRUE)))
  mcpserver:::ensure_daemons(2L)
  expect_true(isTRUE(mcpserver:::.mcp_state$daemons_external))

  mcpserver:::stop_daemons()
  expect_false(isTRUE(mcpserver:::.mcp_state$daemons_started))
  expect_true(mirai::daemons_set())   # external pool untouched
})

test_that("ensure_daemons creates and owns a pool when none exists", {
  reset_daemon_state()
  withr::defer(reset_daemon_state())
  expect_false(mirai::daemons_set())

  mcpserver:::ensure_daemons(2L)
  expect_true(mirai::daemons_set())
  expect_false(isTRUE(mcpserver:::.mcp_state$daemons_external))   # we own it

  mcpserver:::stop_daemons()
  expect_false(mirai::daemons_set())   # we stopped our own pool
})
