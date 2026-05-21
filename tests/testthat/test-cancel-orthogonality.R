# Request cancellation (notifications/cancelled) and task cancellation
# (tasks/cancel) are separate concerns per SEP-1686. Cancelling one MUST
# NOT auto-cancel the other.

build_tooled_session <- function() {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    name = "k", description = "k", input_schema = schema(list()),
    tasks = TRUE, bidirectional = TRUE,
    handler = function(args, ctx) response_text("ok")))
  s <- mcpserver:::Session$new("s", srv, function(env) NULL)
  assign("s", s, envir = srv$sessions)
  s$client_capabilities <- list()
  list(server = srv, session = s)
}

start_task_call <- function(srv, s, msg_id = 42L) {
  marker <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = msg_id, method = "tools/call",
    params = list(name = "k", arguments = list())))
  list(marker = marker, task_id = marker$ctx$.task$id)
}

test_that("notifications/cancelled marks the request but NOT the task", {
  c <- build_tooled_session()
  state <- start_task_call(c$server, c$session)
  # Initial status is "running" (set by handle_tools_call).
  store <- mcpserver:::ensure_task_store(c$server)
  expect_equal(mcpserver:::task_get(store, state$task_id)$status,
               "running")
  # Fire notifications/cancelled for the request id.
  mcpserver:::route_message(c$server, c$session, list(
    jsonrpc = "2.0", method = "notifications/cancelled",
    params = list(requestId = 42L)))
  # The request's cancellation entry was signalled.
  key <- "42"
  entry <- get(key, envir = c$session$cancel, inherits = FALSE)
  expect_true(isTRUE(entry$cancelled))
  # But the task status MUST still be "running" — cancellation of
  # the surrounding request does not auto-cancel the task.
  expect_equal(mcpserver:::task_get(store, state$task_id)$status,
               "running")
})

test_that("tasks/cancel marks the task but NOT the underlying request", {
  c <- build_tooled_session()
  state <- start_task_call(c$server, c$session)
  # The request id 42 has a cancellation entry that should NOT be
  # flipped by tasks/cancel.
  key <- "42"
  expect_true(exists(key, envir = c$session$cancel, inherits = FALSE))
  # Fire tasks/cancel for the task id.
  resp <- mcpserver:::route_message(c$server, c$session, list(
    jsonrpc = "2.0", id = 99L, method = "tasks/cancel",
    params = list(taskId = state$task_id)))
  expect_equal(length(resp$result), 0L)
  store <- mcpserver:::ensure_task_store(c$server)
  expect_equal(mcpserver:::task_get(store, state$task_id)$status,
               "cancelled")
  # Request's cancel entry must NOT have flipped.
  entry <- get(key, envir = c$session$cancel, inherits = FALSE)
  expect_false(isTRUE(entry$cancelled))
})

test_that("ctx$cancelled() reflects request-cancel only", {
  c <- build_tooled_session()
  state <- start_task_call(c$server, c$session)
  ctx <- state$marker$ctx
  expect_false(ctx$cancelled())
  expect_false(ctx$task$cancelled())
  mcpserver:::route_message(c$server, c$session, list(
    jsonrpc = "2.0", method = "notifications/cancelled",
    params = list(requestId = 42L)))
  expect_true(ctx$cancelled())
  # task$cancelled is still FALSE.
  expect_false(ctx$task$cancelled())
})

test_that("ctx$task$cancelled() reflects task-cancel only", {
  c <- build_tooled_session()
  state <- start_task_call(c$server, c$session)
  ctx <- state$marker$ctx
  expect_false(ctx$cancelled())
  expect_false(ctx$task$cancelled())
  mcpserver:::route_message(c$server, c$session, list(
    jsonrpc = "2.0", id = 99L, method = "tasks/cancel",
    params = list(taskId = state$task_id)))
  expect_false(ctx$cancelled())
  expect_true(ctx$task$cancelled())
})
