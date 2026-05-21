# Progress-driven timeout reset semantics for outbound (server→client)
# requests. Mirrors `test/shared/protocol.test.ts > progress notification
# timeout behavior` from TS SDK v1.29.0.

new_pending_session <- function() {
  srv <- new_server("t")
  bag <- new.env(parent = emptyenv())
  bag$out <- list()
  s <- mcpserver:::Session$new("t", srv, function(env) {
    bag$out <- c(bag$out, list(env))
  })
  list(session = s, outgoing = bag)
}

# Helper: schedule an inbound progress notification (or a final
# response) after `delay` seconds.
schedule <- function(fn, delay) later::later(fn, delay)

test_that("outbound request times out at the base deadline when no progress arrives", {
  stub <- new_pending_session()
  t0 <- Sys.time()
  expect_error(
    mcpserver:::call_client_blocking(
      stub$session, "test/method", list(), timeout = 0.3),
    "timed out")
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_gte(elapsed, 0.25)
  expect_lt(elapsed, 1.0)
})

test_that("matching-token progress resets the deadline before it expires", {
  stub <- new_pending_session()
  t0 <- Sys.time()
  # The request will use a generated token surfaced via _meta.
  # We schedule three progress notifications that arrive every 0.2s,
  # extending the deadline from a base 0.4s to roughly 1.0s. Then a
  # final response wins at 0.9s.
  schedule(function() {
    # Look up the token of the most recent outgoing request.
    out <- stub$outgoing$out
    last <- out[[length(out)]]
    token <- last$params$`_meta`$progressToken
    mcpserver:::handle_progress_in(NULL, stub$session,
      list(progressToken = token, progress = 1, total = 3))
  }, 0.2)
  schedule(function() {
    out <- stub$outgoing$out
    token <- out[[1L]]$params$`_meta`$progressToken
    mcpserver:::handle_progress_in(NULL, stub$session,
      list(progressToken = token, progress = 2, total = 3))
  }, 0.4)
  schedule(function() {
    out <- stub$outgoing$out
    rid <- out[[1L]]$id
    stub$session$resolve_pending(rid, list(ok = TRUE),
                                 is_error = FALSE)
  }, 0.7)
  res <- mcpserver:::call_client_blocking(
    stub$session, "test/method", list(),
    timeout = 0.3,
    reset_timeout_on_progress = TRUE)
  expect_true(isTRUE(res$ok))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_gte(elapsed, 0.6)
})

test_that("max_total_timeout caps the cumulative wait regardless of progress", {
  stub <- new_pending_session()
  # Even with progress, total can't exceed max_total_timeout = 0.5s.
  schedule(function() {
    out <- stub$outgoing$out
    token <- out[[1L]]$params$`_meta`$progressToken
    mcpserver:::handle_progress_in(NULL, stub$session,
      list(progressToken = token, progress = 1))
  }, 0.1)
  schedule(function() {
    out <- stub$outgoing$out
    token <- out[[1L]]$params$`_meta`$progressToken
    mcpserver:::handle_progress_in(NULL, stub$session,
      list(progressToken = token, progress = 2))
  }, 0.3)
  t0 <- Sys.time()
  expect_error(
    mcpserver:::call_client_blocking(
      stub$session, "test/method", list(),
      timeout = 1.0,
      reset_timeout_on_progress = TRUE,
      max_total_timeout = 0.5),
    "timed out")
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_lt(elapsed, 1.0)
})

test_that("progress with a different token does not reset the deadline", {
  stub <- new_pending_session()
  schedule(function() {
    mcpserver:::handle_progress_in(NULL, stub$session,
      list(progressToken = "wrong-token", progress = 1))
  }, 0.1)
  t0 <- Sys.time()
  expect_error(
    mcpserver:::call_client_blocking(
      stub$session, "test/method", list(),
      timeout = 0.3,
      reset_timeout_on_progress = TRUE),
    "timed out")
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_lt(elapsed, 0.7)
})

test_that("reset_timeout_on_progress = TRUE injects a progressToken in _meta", {
  stub <- new_pending_session()
  schedule(function() {
    out <- stub$outgoing$out
    rid <- out[[1L]]$id
    stub$session$resolve_pending(rid, list(), is_error = FALSE)
  }, 0.05)
  mcpserver:::call_client_blocking(
    stub$session, "test/method", list(some = "param"),
    timeout = 1,
    reset_timeout_on_progress = TRUE)
  out <- stub$outgoing$out
  token <- out[[1L]]$params$`_meta`$progressToken
  expect_true(is.character(token) && nzchar(token))
})

test_that("reset_timeout_on_progress = FALSE (default) does not inject a token", {
  stub <- new_pending_session()
  schedule(function() {
    out <- stub$outgoing$out
    rid <- out[[1L]]$id
    stub$session$resolve_pending(rid, list(), is_error = FALSE)
  }, 0.05)
  mcpserver:::call_client_blocking(
    stub$session, "test/method", list(some = "param"),
    timeout = 1)
  out <- stub$outgoing$out
  expect_null(out[[1L]]$params$`_meta`$progressToken)
})
