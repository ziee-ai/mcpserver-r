# SEP-1686 invariant: notifications must not flow after a task reaches
# a terminal status. Mirrors TS SDK's protocol.test.ts > "Progress
# notification support for tasks" cluster.

new_task_ctx <- function() {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    name = "k", description = "k", input_schema = schema(list()),
    tasks = TRUE, bidirectional = TRUE,
    handler = function(args, ctx) response_text("ok")))
  out <- new.env(parent = emptyenv()); out$msgs <- list()
  s <- mcpserver:::Session$new("s", srv,
    function(env) out$msgs <- c(out$msgs, list(env)))
  s$log_level <- "debug"
  assign("s", s, envir = srv$sessions)
  s$client_capabilities <- list()
  # Manually allocate a task entry (skip the full tools/call path so
  # the test isolates the lifecycle invariant).
  store <- mcpserver:::ensure_task_store(srv)
  task <- mcpserver:::task_create(store, "k", session_id = "s")
  mcpserver:::task_update_status(store, task$id, "running")
  ctx <- mcpserver:::make_ctx(s,
    list(id = 1L, params = list(`_meta` = list(progressToken = "tk"))))
  ctx$.task <- mcpserver:::make_task_handle(store, task$id)
  list(server = srv, session = s, ctx = ctx, task = task,
       store = store, sent = out)
}

# progress + log emission while running ----------------------------------

test_that("ctx$send_progress emits while task is running", {
  c <- new_task_ctx()
  c$ctx$send_progress(1, total = 3, message = "step 1")
  expect_length(c$sent$msgs, 1L)
  expect_equal(c$sent$msgs[[1L]]$method, "notifications/progress")
})

test_that("ctx$send_log emits while task is running", {
  c <- new_task_ctx()
  c$ctx$send_log("info", "hello")
  expect_length(c$sent$msgs, 1L)
  expect_equal(c$sent$msgs[[1L]]$method, "notifications/message")
})

# Terminal status drops further progress + log notifications -------------

test_that("progress is silently dropped after task reaches 'completed'", {
  c <- new_task_ctx()
  c$ctx$send_progress(1, total = 2, message = "step 1")
  mcpserver:::task_update_status(c$store, c$task$id, "completed")
  c$ctx$send_progress(2, total = 2, message = "step 2 — after terminal")
  expect_length(c$sent$msgs, 1L)  # only the pre-terminal one
})

test_that("progress is silently dropped after task reaches 'failed'", {
  c <- new_task_ctx()
  mcpserver:::task_update_status(c$store, c$task$id, "failed")
  c$ctx$send_progress(1, total = 1)
  expect_length(c$sent$msgs, 0L)
})

test_that("progress is silently dropped after task reaches 'cancelled'", {
  c <- new_task_ctx()
  mcpserver:::task_update_status(c$store, c$task$id, "cancelled")
  c$ctx$send_progress(1, total = 1)
  expect_length(c$sent$msgs, 0L)
})

test_that("logs are silently dropped after task is completed/failed/cancelled", {
  for (term in c("completed", "failed", "cancelled")) {
    c <- new_task_ctx()
    mcpserver:::task_update_status(c$store, c$task$id, term)
    c$ctx$send_log("warning", "after-terminal log")
    expect_length(c$sent$msgs, 0L)
  }
})

test_that("input_required is NOT terminal — progress still flows", {
  c <- new_task_ctx()
  mcpserver:::task_update_status(c$store, c$task$id, "input_required")
  c$ctx$send_progress(1, total = 1, message = "waiting for elicit")
  expect_length(c$sent$msgs, 1L)
})
