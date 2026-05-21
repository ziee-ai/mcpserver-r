# Full tasks lifecycle: tasks/get, tasks/result, tasks/cancel (using
# the spec param name `taskId`), and TaskStatusNotification emission.

test_that("tasks/cancel accepts taskId per the spec", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    name = "k", description = "k",
    input_schema = schema(list()),
    tasks = TRUE,
    handler = function(args, ctx) response_text("ok")))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  marker <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 2, method = "tools/call",
    params = list(name = "k", arguments = list())))
  tid <- marker$ctx$.task$id

  cancelled <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 3, method = "tasks/cancel",
    params = list(taskId = tid)))
  # Cancellation status update produces an empty-object result.
  expect_equal(length(cancelled$result), 0L)
  expect_equal(get(tid, envir = srv$task_store)$status, "cancelled")
})

test_that("tasks/cancel still accepts legacy `id` param for backwards compat", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    "k", "k", schema(list()), tasks = TRUE,
    handler = function(args, ctx) response_text("ok")))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  marker <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 2, method = "tools/call",
    params = list(name = "k", arguments = list())))
  tid <- marker$ctx$.task$id
  cancelled <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 3, method = "tasks/cancel",
    params = list(id = tid)))
  expect_equal(length(cancelled$result), 0L)
})

test_that("tasks/cancel rejects cancellation on a terminal task", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    "k", "k", schema(list()), tasks = TRUE,
    handler = function(args, ctx) response_text("ok")))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  marker <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 2, method = "tools/call",
    params = list(name = "k", arguments = list())))
  tid <- marker$ctx$.task$id
  mcpserver:::task_update_status(srv$task_store, tid, "completed")
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 3, method = "tasks/cancel",
    params = list(taskId = tid)))
  expect_equal(resp$error$code, -32600)
})

test_that("tasks/get returns current state without blocking", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    "k", "k", schema(list()), tasks = TRUE,
    handler = function(args, ctx) response_text("ok")))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  marker <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 2, method = "tools/call",
    params = list(name = "k", arguments = list())))
  tid <- marker$ctx$.task$id
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 3, method = "tasks/get",
    params = list(taskId = tid)))
  expect_equal(resp$result$taskId, tid)
  expect_equal(resp$result$status, "running")
})

test_that("tasks/get returns -32602 on unknown taskId", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  # ensure store
  mcpserver:::ensure_task_store(srv)
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 3, method = "tasks/get",
    params = list(taskId = "missing")))
  expect_equal(resp$error$code, -32602)
})

test_that("tasks/cancel emits notifications/tasks/status to the session", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    "k", "k", schema(list()), tasks = TRUE,
    handler = function(args, ctx) response_text("ok")))
  out <- new.env(parent = emptyenv()); out$msgs <- list()
  s <- mcpserver:::Session$new("t", srv, function(e) {
    out$msgs <- c(out$msgs, list(e))
  })
  marker <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 2, method = "tools/call",
    params = list(name = "k", arguments = list())))
  tid <- marker$ctx$.task$id
  mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 3, method = "tasks/cancel",
    params = list(taskId = tid)))
  methods <- vapply(out$msgs, function(m) m$method %||% "",
                    character(1L))
  expect_true("notifications/tasks/status" %in% methods)
})

test_that("tasks/result returns task in terminal state when reached", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    "k", "k", schema(list()), tasks = TRUE,
    handler = function(args, ctx) response_text("ok")))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  marker <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 2, method = "tools/call",
    params = list(name = "k", arguments = list())))
  tid <- marker$ctx$.task$id
  # Force terminal state synchronously, then poll.
  mcpserver:::task_update_status(srv$task_store, tid, "completed")
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 3, method = "tasks/result",
    params = list(taskId = tid)))
  expect_equal(resp$result$status, "completed")
})

test_that("task_set_result stores the final result on the task", {
  store <- mcpserver:::new_task_store()
  task <- mcpserver:::task_create(store, "t", session_id = "s")
  mcpserver:::task_set_result(store, task$id,
    list(content = list(list(type = "text", text = "answer"))))
  t <- mcpserver:::task_get(store, task$id)
  expect_equal(t$result$content[[1L]]$text, "answer")
  expect_false(is.null(t$last_updated))
})

test_that("task_clear_messages empties the queue", {
  store <- mcpserver:::new_task_store()
  task <- mcpserver:::task_create(store, "t", session_id = "s")
  mcpserver:::task_append_message(store, task$id, list(method = "a"))
  mcpserver:::task_append_message(store, task$id, list(method = "b"))
  expect_length(mcpserver:::task_get(store, task$id)$messages, 2L)
  mcpserver:::task_clear_messages(store, task$id)
  expect_length(mcpserver:::task_get(store, task$id)$messages, 0L)
})
