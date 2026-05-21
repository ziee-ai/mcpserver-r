# SSE event-log replay + ring-buffer cap.

test_that("event log is capped at max_event_log and old entries are dropped", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL,
                               max_event_log = 3L)
  for (i in 1:5) s$record_event(sprintf("e%d", i))
  expect_equal(length(s$event_log), 3L)
  # Oldest two should have been evicted; check the last id is "e5".
  expect_equal(s$event_log[[length(s$event_log)]]$payload, "e5")
})

test_that("replay_after returns events after a known id", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL,
                               max_event_log = 10L)
  ids <- vapply(seq_len(5), function(i) s$record_event(
    sprintf("e%d", i)), character(1L))
  replay <- s$replay_after(ids[[2]])
  payloads <- vapply(replay, function(e) e$payload, character(1L))
  expect_equal(payloads, c("e3", "e4", "e5"))
})

test_that("replay_after returns truncation sentinel when id has been evicted", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL,
                               max_event_log = 2L)
  for (i in 1:5) s$record_event(sprintf("e%d", i))
  replay <- s$replay_after("evicted-id-xyz")
  expect_equal(replay[[1L]]$id, "replay-truncated")
})
