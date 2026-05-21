# Cancellation channel: in-process flag + cross-process flag file +
# on_cancel(fn) callback hooks. Tests cover both the transport-thread
# path (bidirectional tools share the live env) and the daemon path
# (simulated by file.exists on a stand-alone flag file).

new_session <- function() {
  srv <- new_server("t")
  mcpserver:::Session$new("t", srv, function(e) NULL)
}

test_that("cancel_entry_open creates an env with cancelled FALSE and a flag_path", {
  s <- new_session()
  entry <- mcpserver:::cancel_entry_open(s, 1L)
  expect_false(isTRUE(entry$cancelled))
  expect_type(entry$flag_path, "character")
  expect_length(entry$flag_path, 1L)
  expect_equal(length(entry$on_cancel_fns), 0L)
  expect_true(exists("1", envir = s$cancel, inherits = FALSE))
  mcpserver:::cancel_entry_close(s, 1L)
})

test_that("cancel_entry_signal flips flag, touches file, and fires hooks", {
  s <- new_session()
  entry <- mcpserver:::cancel_entry_open(s, 2L)
  fired <- new.env(parent = emptyenv()); fired$n <- 0L
  entry$on_cancel_fns <- c(entry$on_cancel_fns,
                           list(function() fired$n <- fired$n + 1L))
  mcpserver:::cancel_entry_signal(entry)
  expect_true(isTRUE(entry$cancelled))
  expect_true(file.exists(entry$flag_path))
  expect_equal(fired$n, 1L)
  mcpserver:::cancel_entry_close(s, 2L)
})

test_that("cancel_entry_close unlinks file and removes the entry", {
  s <- new_session()
  entry <- mcpserver:::cancel_entry_open(s, 3L)
  mcpserver:::cancel_entry_signal(entry)
  path <- entry$flag_path
  expect_true(file.exists(path))
  mcpserver:::cancel_entry_close(s, 3L)
  expect_false(file.exists(path))
  expect_false(exists("3", envir = s$cancel, inherits = FALSE))
})

test_that("cancel_entry_close is idempotent on missing ids", {
  s <- new_session()
  expect_silent(mcpserver:::cancel_entry_close(s, 999L))
})

test_that("handle_cancelled signals the entry by requestId", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  entry <- mcpserver:::cancel_entry_open(s, 4L)
  mcpserver:::handle_cancelled(srv, s, list(requestId = 4L))
  expect_true(isTRUE(entry$cancelled))
  expect_true(file.exists(entry$flag_path))
  mcpserver:::cancel_entry_close(s, 4L)
})

test_that("Session$close sweeps all cancel entries and removes flag files", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  e1 <- mcpserver:::cancel_entry_open(s, 10L)
  e2 <- mcpserver:::cancel_entry_open(s, 11L)
  mcpserver:::cancel_entry_signal(e1) # touch one flag file
  expect_true(file.exists(e1$flag_path))
  s$close()
  expect_false(file.exists(e1$flag_path))
  expect_false(file.exists(e2$flag_path))
  expect_equal(length(ls(s$cancel, all.names = TRUE)), 0L)
})

# ctx$cancelled() behaviour ----------------------------------------------

make_test_ctx <- function(s, msg_id) {
  msg <- list(id = msg_id)
  mcpserver:::make_ctx(s, msg)
}

test_that("ctx$cancelled() is FALSE before cancel arrives", {
  s <- new_session()
  mcpserver:::cancel_entry_open(s, 20L)
  ctx <- make_test_ctx(s, 20L)
  expect_false(ctx$cancelled())
  mcpserver:::cancel_entry_close(s, 20L)
})

test_that("ctx$cancelled() reflects the in-process flag (bidirectional path)", {
  s <- new_session()
  entry <- mcpserver:::cancel_entry_open(s, 21L)
  ctx <- make_test_ctx(s, 21L)
  expect_false(ctx$cancelled())
  mcpserver:::cancel_entry_signal(entry)
  expect_true(ctx$cancelled())
  mcpserver:::cancel_entry_close(s, 21L)
})

test_that("ctx$cancelled() reflects the flag file (simulated daemon path)", {
  # Simulate the daemon view: no live entry in session$cancel (the
  # in-process flag is a stale FALSE), but `.cancel_path` is set on
  # ctx and the file has been touched by the transport thread.
  s <- new_session()
  ctx <- make_test_ctx(s, 22L)
  path <- file.path(tempdir(),
                    sprintf("mcpserver-cancel-daemon-test-%d.flag",
                            Sys.getpid()))
  ctx$.cancel_path <- path
  expect_false(ctx$cancelled())
  file.create(path)
  expect_true(ctx$cancelled())
  unlink(path)
})

test_that("ctx$on_cancel(fn) appends and runs on signal", {
  s <- new_session()
  entry <- mcpserver:::cancel_entry_open(s, 23L)
  ctx <- make_test_ctx(s, 23L)
  fired <- new.env(parent = emptyenv()); fired$n <- 0L
  ctx$on_cancel(function() fired$n <- fired$n + 1L)
  ctx$on_cancel(function() fired$n <- fired$n + 10L)
  expect_equal(length(entry$on_cancel_fns), 2L)
  mcpserver:::cancel_entry_signal(entry)
  expect_equal(fired$n, 11L)
  mcpserver:::cancel_entry_close(s, 23L)
})

test_that("ctx$on_cancel(fn) refuses non-function arguments", {
  s <- new_session()
  mcpserver:::cancel_entry_open(s, 24L)
  ctx <- make_test_ctx(s, 24L)
  expect_error(ctx$on_cancel("not a function"))
  mcpserver:::cancel_entry_close(s, 24L)
})

test_that("ctx$on_cancel(fn) errors when no cancel entry exists for the id", {
  s <- new_session()
  ctx <- make_test_ctx(s, 25L)
  expect_error(ctx$on_cancel(function() NULL),
               "outside a cancellable request")
})

# Cross-process daemon integration ----------------------------------------

test_that("a mirai-daemon handler observes cancellation via the flag file", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  s <- new_session()
  entry <- mcpserver:::cancel_entry_open(s, 30L)
  ctx <- make_test_ctx(s, 30L)
  ctx$.cancel_path <- entry$flag_path

  mirai::daemons(1)
  withr::defer(mirai::daemons(0))

  # Daemon polls a stand-in for ctx$cancelled() — the file path — and
  # returns the elapsed time at the point it observed cancel. We pass
  # the path explicitly rather than the full ctx to keep the test
  # focused on the cross-process signal, not on McpCtx serialisation
  # (which involves its own quirks around environment classes).
  m <- mirai::mirai({
    started <- Sys.time()
    repeat {
      if (file.exists(p)) {
        return(list(
          observed = TRUE,
          elapsed = as.numeric(difftime(Sys.time(), started,
                                        units = "secs"))))
      }
      if (as.numeric(difftime(Sys.time(), started,
                              units = "secs")) > 2) {
        return(list(observed = FALSE, elapsed = -1))
      }
      Sys.sleep(0.02)
    }
  }, p = entry$flag_path)

  # Wait for the daemon to start, then fire the cancel.
  Sys.sleep(0.2)
  s$cancel_request(30L)

  res <- mirai::collect_mirai(m)
  expect_true(isTRUE(res$observed))
  expect_lt(res$elapsed, 1.5)
  mcpserver:::cancel_entry_close(s, 30L)
})

test_that("cancel signal survives a real ctx serialisation through with_mirai", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  # This test exercises the daemon path of `ctx$cancelled()` end-to-end:
  # we ship a real ctx into a mirai daemon and assert that the daemon's
  # `ctx$cancelled()` (which falls through to `file.exists(.cancel_path)`)
  # flips from FALSE to TRUE when the parent thread calls
  # `session$cancel_request()`. The daemon needs the in-development
  # mcpserver code (the new `.cancel_path` branch), not the installed
  # one, so we pkgload::load_all() inside the daemon. Skip if pkgload
  # isn't available or the package source root cannot be located.
  skip_if_not_installed("pkgload")
  pkg_root <- tryCatch(pkgload::pkg_path(), error = function(e) NULL)
  if (is.null(pkg_root) || !file.exists(file.path(pkg_root, "DESCRIPTION"))) {
    skip("could not locate package root for daemon load_all()")
  }

  s <- new_session()
  entry <- mcpserver:::cancel_entry_open(s, 31L)
  ctx <- make_test_ctx(s, 31L)
  ctx$.cancel_path <- entry$flag_path

  mirai::daemons(1)
  withr::defer(mirai::daemons(0))
  mirai::everywhere(suppressPackageStartupMessages(
    pkgload::load_all(pkg_root_, quiet = TRUE)), pkg_root_ = pkg_root)

  m <- mirai::mirai({
    started <- Sys.time()
    repeat {
      if (call_ctx$cancelled()) {
        return(list(
          cancelled = TRUE,
          elapsed = as.numeric(difftime(Sys.time(), started,
                                        units = "secs"))))
      }
      if (as.numeric(difftime(Sys.time(), started,
                              units = "secs")) > 2) {
        return(list(cancelled = FALSE, elapsed = -1))
      }
      Sys.sleep(0.02)
    }
  }, call_ctx = ctx)

  Sys.sleep(0.2)
  s$cancel_request(31L)

  res <- mirai::collect_mirai(m)
  expect_true(isTRUE(res$cancelled))
  expect_lt(res$elapsed, 1.5)
  mcpserver:::cancel_entry_close(s, 31L)
})
