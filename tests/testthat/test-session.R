test_that("session subscriptions can be added and removed", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("x", srv, function(e) NULL)
  mcpserver:::handle_resources_subscribe(srv, s,
                                         list(uri = "demo://a"), NULL)
  expect_true(exists("demo://a", envir = s$subs, inherits = FALSE))
  mcpserver:::handle_resources_unsubscribe(srv, s,
                                           list(uri = "demo://a"), NULL)
  expect_false(exists("demo://a", envir = s$subs, inherits = FALSE))
})

test_that("session id namespace for outgoing requests is negative", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("x", srv, function(e) NULL)
  a <- s$new_request_id()
  b <- s$new_request_id()
  expect_lt(a, 0L)
  expect_lt(b, a)
})

test_that("event log replay returns truncation sentinel for missing id", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("x", srv, function(e) NULL)
  s$record_event("a")
  s$record_event("b")
  out <- s$replay_after("does-not-exist")
  expect_equal(out[[1L]]$id, "replay-truncated")
})
