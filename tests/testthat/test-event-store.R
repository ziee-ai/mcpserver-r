# Pluggable event store tests.

test_that("new_event_store appends and replays events in order", {
  es <- new_event_store(max_per_stream = 10L)
  ids <- vapply(seq_len(5L), function(i)
    es$append("s1", sprintf("payload-%d", i)), character(1L))
  out <- es$replay("s1")
  payloads <- vapply(out, function(e) e$payload, character(1L))
  expect_equal(payloads, sprintf("payload-%d", 1:5))
})

test_that("replay returns events strictly after the cursor", {
  es <- new_event_store(max_per_stream = 10L)
  ids <- vapply(seq_len(5L), function(i)
    es$append("s1", sprintf("p%d", i)), character(1L))
  out <- es$replay("s1", ids[[2L]])
  payloads <- vapply(out, function(e) e$payload, character(1L))
  expect_equal(payloads, c("p3", "p4", "p5"))
})

test_that("replay with last_event_id = '0' returns the entire stream", {
  es <- new_event_store(max_per_stream = 10L)
  for (i in 1:3) es$append("s1", sprintf("p%d", i))
  out <- es$replay("s1", last_event_id = "0")
  expect_equal(length(out), 3L)
})

test_that("ring buffer caps and evicts oldest", {
  es <- new_event_store(max_per_stream = 3L)
  for (i in 1:5) es$append("s1", sprintf("p%d", i))
  out <- es$replay("s1")
  payloads <- vapply(out, function(e) e$payload, character(1L))
  expect_equal(payloads, c("p3", "p4", "p5"))
})

test_that("replay-truncated sentinel on missing cursor", {
  es <- new_event_store(max_per_stream = 3L)
  for (i in 1:5) es$append("s1", sprintf("p%d", i))
  out <- es$replay("s1", last_event_id = "missing")
  expect_equal(out[[1L]]$id, "replay-truncated")
})

test_that("streams are isolated per stream_id", {
  es <- new_event_store(max_per_stream = 10L)
  es$append("s1", "a")
  es$append("s2", "b")
  expect_equal(length(es$replay("s1")), 1L)
  expect_equal(length(es$replay("s2")), 1L)
  expect_equal(es$replay("s1")[[1L]]$payload, "a")
})

test_that("drop_stream removes everything for a stream", {
  es <- new_event_store()
  es$append("s1", "x")
  expect_equal(es$size("s1"), 1L)
  es$drop_stream("s1")
  expect_equal(es$size("s1"), 0L)
})

test_that("max argument trims replay output", {
  es <- new_event_store(max_per_stream = 100L)
  for (i in 1:10) es$append("s1", sprintf("p%d", i))
  out <- es$replay("s1", max = 3L)
  expect_equal(length(out), 3L)
})

test_that("size() counts streams when no stream_id is given", {
  es <- new_event_store()
  es$append("a", "1"); es$append("b", "1"); es$append("c", "1")
  expect_equal(es$size(), 3L)
})

test_that("size() returns 0 for unknown stream", {
  es <- new_event_store()
  expect_equal(es$size("missing"), 0L)
})
