test_that("tools opting into tasks lifecycle receive a ctx$task handle", {
  srv <- new_server("t")
  observed <- new.env(parent = emptyenv())
  add_capability(srv, new_tool(
    name = "task-tool",
    description = "Task-driven tool",
    input_schema = schema(list()),
    tasks = TRUE,
    handler = function(args, ctx) {
      observed$has_task <- !is.null(ctx$task)
      observed$task_id <- ctx$task$id
      ctx$task$append_message(list(type = "text", text = "running"))
      ctx$task$update_status("completed")
      response_text("done")
    }
  ))

  session <- mcpserver:::Session$new("test", srv, function(e) NULL)
  mcpserver:::route_message(srv, session, list(
    jsonrpc = "2.0", id = 1, method = "initialize",
    params = list(protocolVersion = "2025-06-18", capabilities = list())))

  marker <- mcpserver:::route_message(srv, session, list(
    jsonrpc = "2.0", id = 2, method = "tools/call",
    params = list(name = "task-tool", arguments = list())))
  expect_true(isTRUE(marker$.async))
  expect_true(!is.null(marker$ctx$.task))
  task_id <- marker$ctx$.task$id
  expect_true(nzchar(task_id))

  store <- srv$task_store
  expect_true(!is.null(store))
  expect_true(exists(task_id, envir = store, inherits = FALSE))
  # Pre-execution status is "running" (set by handle_tools_call).
  entry <- get(task_id, envir = store, inherits = FALSE)
  expect_equal(entry$status, "running")
})

test_that("tasks/list reflects created tasks; tasks/cancel updates status", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    name = "k",
    description = "k",
    input_schema = schema(list()),
    tasks = TRUE,
    handler = function(args, ctx) response_text("ok")
  ))
  session <- mcpserver:::Session$new("t", srv, function(e) NULL)
  mcpserver:::route_message(srv, session, list(
    jsonrpc = "2.0", id = 1, method = "initialize", params = list()))
  marker <- mcpserver:::route_message(srv, session, list(
    jsonrpc = "2.0", id = 2, method = "tools/call",
    params = list(name = "k", arguments = list())))
  tid <- marker$ctx$.task$id

  listed <- mcpserver:::route_message(srv, session, list(
    jsonrpc = "2.0", id = 3, method = "tasks/list"))
  ids <- vapply(listed$result$tasks, function(t) t$id, character(1L))
  expect_true(tid %in% ids)

  cancelled <- mcpserver:::route_message(srv, session, list(
    jsonrpc = "2.0", id = 4, method = "tasks/cancel",
    params = list(id = tid)))
  # Empty-object result encoded via j_empty_obj.
  expect_equal(length(cancelled$result), 0L)
  expect_equal(get(tid, envir = srv$task_store)$status, "cancelled")
})
