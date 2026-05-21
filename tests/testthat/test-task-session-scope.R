# tasks/get, tasks/cancel and tasks/result must refuse to operate on a
# task owned by a different session. Mirrors the TS SDK's per-session
# task ownership rule and prevents one client from polling another's
# in-flight work.

build_task_server <- function() {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    name = "k", description = "k", input_schema = schema(list()),
    tasks = TRUE,
    handler = function(args, ctx) response_text("ok")))
  srv
}

test_that("tasks/get refuses to disclose a task owned by another session", {
  srv <- build_task_server()
  s1 <- mcpserver:::Session$new("alice", srv, function(e) NULL)
  s2 <- mcpserver:::Session$new("bob", srv, function(e) NULL)
  marker <- mcpserver:::route_message(srv, s1, list(
    jsonrpc = "2.0", id = 1, method = "tools/call",
    params = list(name = "k", arguments = list())))
  tid <- marker$ctx$.task$id
  # Bob asks about Alice's task — should be refused.
  resp <- mcpserver:::route_message(srv, s2, list(
    jsonrpc = "2.0", id = 2, method = "tasks/get",
    params = list(taskId = tid)))
  expect_equal(resp$error$code, -32602)
  expect_match(resp$error$message, "session")
})

test_that("tasks/cancel refuses to cancel another session's task", {
  srv <- build_task_server()
  s1 <- mcpserver:::Session$new("alice", srv, function(e) NULL)
  s2 <- mcpserver:::Session$new("bob", srv, function(e) NULL)
  marker <- mcpserver:::route_message(srv, s1, list(
    jsonrpc = "2.0", id = 1, method = "tools/call",
    params = list(name = "k", arguments = list())))
  tid <- marker$ctx$.task$id
  resp <- mcpserver:::route_message(srv, s2, list(
    jsonrpc = "2.0", id = 2, method = "tasks/cancel",
    params = list(taskId = tid)))
  expect_equal(resp$error$code, -32602)
})

test_that("tasks/get returns lastUpdatedAt reflecting later task updates", {
  srv <- build_task_server()
  s <- mcpserver:::Session$new("alice", srv, function(e) NULL)
  marker <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "tools/call",
    params = list(name = "k", arguments = list())))
  tid <- marker$ctx$.task$id
  Sys.sleep(0.01)
  mcpserver:::task_update_status(srv$task_store, tid, "completed")
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 2, method = "tasks/get",
    params = list(taskId = tid)))
  expect_equal(resp$result$status, "completed")
  expect_true(!identical(resp$result$createdAt,
                         resp$result$lastUpdatedAt))
})
