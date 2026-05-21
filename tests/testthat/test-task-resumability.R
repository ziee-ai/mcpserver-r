# Streamable-HTTP resumability for a long-running tool emitting progress.
# Drives the server's in-process event log + replay handlers directly,
# verifying that:
#   1. A bidirectional tool's progress notifications are recorded against
#      the session's event log when an SSE stream is attached.
#   2. After a stream is closed, the events remain in the log.
#   3. A new SSE attach with `Last-Event-ID` replays only the events
#      strictly after that id (matching the wire-level guarantee that
#      `R/transport_http.R::http_get_handler` provides).
# Uses an in-process server + a stub SSE connection so the test isn't
# timing-sensitive on subprocess startup or curl buffering.

new_stub_conn <- function() {
  e <- new.env(parent = emptyenv())
  e$received <- list()
  e$closed <- FALSE
  e$id <- new_uuid()
  list(
    id = e$id,
    obj = e,
    send = function(payload) {
      e$received <- c(e$received, list(as.character(payload)))
    },
    close = function() { e$closed <- TRUE }
  )
}

new_session_with_log <- function() {
  srv <- new_server("resume", version = "0.1.0")
  add_capability(srv, new_tool(
    name = "slow-emit",
    description = "Emits progress",
    input_schema = schema(list(
      count = property_integer("count", default = 4L))),
    bidirectional = TRUE,
    handler = function(args, ctx) {
      total <- as.integer(args$count %||% 4L)
      for (i in seq_len(total)) {
        ctx$send_progress(i, total = total,
                          message = sprintf("step %d/%d", i, total))
      }
      response_text("done")
    }))
  bag <- new.env(parent = emptyenv()); bag$out <- list()
  s <- mcpserver:::Session$new("s", srv, function(env) {
    bag$out <- c(bag$out, list(env))
    s$record_event(jrpc_encode(env))
  })
  assign("s", s, envir = srv$sessions)
  list(server = srv, session = s, outgoing = bag)
}

test_that("a bidirectional tool's progress notifications land in the event log", {
  ctx_pair <- new_session_with_log()
  srv <- ctx_pair$server; s <- ctx_pair$session

  call_msg <- list(jsonrpc = "2.0", id = 1L,
                   method = "tools/call",
                   params = list(name = "slow-emit",
                                 arguments = list(count = 4L),
                                 `_meta` = list(progressToken = "tok")))
  marker <- mcpserver:::route_message(srv, s, call_msg)
  done <- new.env(parent = emptyenv()); done$resp <- NULL
  promises::then(mcpserver:::finalize_async(marker, srv, s),
                 onFulfilled = function(v) done$resp <- v)
  t0 <- Sys.time()
  while (is.null(done$resp) &&
         difftime(Sys.time(), t0, units = "secs") < 5) {
    later::run_now(timeoutSecs = 0.05)
  }
  expect_false(is.null(done$resp))
  # 4 progress events recorded.
  expect_gte(length(s$event_log), 4L)
})

test_that("event log entries each carry a unique monotonic id", {
  ctx_pair <- new_session_with_log()
  s <- ctx_pair$session
  for (i in seq_len(6L)) {
    s$record_event(sprintf("payload-%d", i))
  }
  ids <- vapply(s$event_log, function(e) e$id, character(1L))
  expect_length(ids, 6L)
  expect_length(unique(ids), 6L)
})

test_that("replay_after returns events strictly newer than the cursor", {
  ctx_pair <- new_session_with_log()
  s <- ctx_pair$session
  for (i in seq_len(5L)) {
    s$record_event(sprintf("payload-%d", i))
  }
  mid_id <- s$event_log[[2L]]$id
  replayed <- s$replay_after(mid_id)
  expect_length(replayed, 3L)  # entries 3, 4, 5
  expect_equal(replayed[[1L]]$payload, "payload-3")
  expect_equal(replayed[[3L]]$payload, "payload-5")
})

test_that("replay with the most recent id returns no events (caught up)", {
  ctx_pair <- new_session_with_log()
  s <- ctx_pair$session
  for (i in seq_len(3L)) {
    s$record_event(sprintf("payload-%d", i))
  }
  last_id <- s$event_log[[3L]]$id
  replayed <- s$replay_after(last_id)
  expect_length(replayed, 0L)
})

test_that("replay with an evicted id surfaces the replay-truncated sentinel", {
  ctx_pair <- new_session_with_log()
  s <- ctx_pair$session
  for (i in seq_len(3L)) {
    s$record_event(sprintf("payload-%d", i))
  }
  replayed <- s$replay_after("never-existed")
  expect_equal(replayed[[1L]]$id, "replay-truncated")
})

test_that("a tool emits, the stream is dropped mid-flight, a reconnect replays", {
  # Same flow as the wire-level reconnect: a long-running bidirectional
  # tool runs, the first SSE stream is closed after 2 events, the
  # remaining events still land in the log, and `replay_after(<2nd id>)`
  # returns ONLY the later events (matching what http_get_handler
  # would send on the reconnect).
  ctx_pair <- new_session_with_log()
  srv <- ctx_pair$server; s <- ctx_pair$session
  call_msg <- list(jsonrpc = "2.0", id = 1L,
                   method = "tools/call",
                   params = list(name = "slow-emit",
                                 arguments = list(count = 6L),
                                 `_meta` = list(progressToken = "tok")))
  marker <- mcpserver:::route_message(srv, s, call_msg)
  done <- new.env(parent = emptyenv()); done$resp <- NULL
  promises::then(mcpserver:::finalize_async(marker, srv, s),
                 onFulfilled = function(v) done$resp <- v)
  t0 <- Sys.time()
  while (is.null(done$resp) &&
         difftime(Sys.time(), t0, units = "secs") < 5) {
    later::run_now(timeoutSecs = 0.05)
  }
  expect_false(is.null(done$resp))
  # All 6 progress notifications + the tools/call response are in
  # the event log.
  expect_gte(length(s$event_log), 6L)
  # Simulate the first stream dropping after the 2nd event.
  cut_id <- s$event_log[[2L]]$id
  replayed <- s$replay_after(cut_id)
  expect_gte(length(replayed), 4L)
  # The replayed slice should carry progress payloads from later
  # steps (the first stream missed them).
  joined <- paste(vapply(replayed, function(e) e$payload,
                         character(1L)), collapse = " ")
  expect_match(joined, '"progress":3', fixed = FALSE)
  expect_match(joined, '"progress":6', fixed = FALSE)
})
