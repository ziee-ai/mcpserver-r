# Task message queueing: when a task-mode bidirectional tool emits
# progress / log notifications via ctx, those envelopes are queued
# against the task and delivered alongside the final result via
# `tasks/result`.

new_tool_server <- function(handler) {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    name = "qtool", description = "task queue test",
    input_schema = schema(list()),
    tasks = TRUE, bidirectional = TRUE,
    handler = handler))
  srv
}

call_tool_then_result <- function(srv, request_id = 1L,
                                  log_level = "debug") {
  sent <- new.env(parent = emptyenv()); sent$msgs <- list()
  s <- mcpserver:::Session$new("s", srv, function(env) {
    sent$msgs <- c(sent$msgs, list(env))
  })
  assign("s", s, envir = srv$sessions)
  s$client_capabilities <- list()
  s$log_level <- log_level

  call_msg <- list(jsonrpc = "2.0", id = request_id,
                   method = "tools/call",
                   params = list(name = "qtool",
                                 arguments = list(),
                                 `_meta` = list(progressToken = "tkn")))
  marker <- mcpserver:::route_message(srv, s, call_msg)
  expect_true(isTRUE(marker$.async))
  # Bidirectional path: finalize_async returns an inline_promise.
  done <- new.env(parent = emptyenv()); done$resp <- NULL
  promises::then(mcpserver:::finalize_async(marker, srv, s),
                 onFulfilled = function(v) done$resp <- v)
  t0 <- Sys.time()
  while (is.null(done$resp) &&
         difftime(Sys.time(), t0, units = "secs") < 5) {
    later::run_now(timeoutSecs = 0.05)
  }
  expect_false(is.null(done$resp))

  # Find the task id by listing tasks.
  store <- mcpserver:::ensure_task_store(srv)
  ids <- ls(store, all.names = TRUE)
  expect_length(ids, 1L)
  task_id <- ids[[1L]]

  result_msg <- list(jsonrpc = "2.0", id = 99L,
                     method = "tasks/result",
                     params = list(taskId = task_id))
  result_resp <- mcpserver:::route_message(srv, s, result_msg)
  list(tool_response = done$resp,
       task_id = task_id,
       sent = sent$msgs,
       result_response = result_resp)
}

test_that("progress events emitted mid-task land in the task's message queue", {
  out <- call_tool_then_result(new_tool_server(function(args, ctx) {
    ctx$send_progress(1, total = 3, message = "step 1")
    ctx$send_progress(2, total = 3, message = "step 2")
    ctx$send_progress(3, total = 3, message = "step 3")
    response_text("done")
  }))
  msgs <- out$result_response$result$messages
  expect_length(msgs, 3L)
  expect_equal(msgs[[1L]]$method, "notifications/progress")
  expect_equal(msgs[[1L]]$params$progress, 1)
  expect_equal(msgs[[3L]]$params$progress, 3)
})

test_that("log notifications mid-task are also queued", {
  out <- call_tool_then_result(new_tool_server(function(args, ctx) {
    ctx$send_log("info", "hello")
    ctx$send_log("warning", "watch out")
    response_text("done")
  }))
  msgs <- out$result_response$result$messages
  expect_length(msgs, 2L)
  expect_equal(msgs[[1L]]$method, "notifications/message")
  expect_equal(msgs[[1L]]$params$level, "info")
  expect_equal(msgs[[2L]]$params$level, "warning")
})

test_that("tasks/result returns the final tool result alongside queued messages", {
  out <- call_tool_then_result(new_tool_server(function(args, ctx) {
    ctx$send_progress(1, total = 1, message = "only")
    response_text("the answer is 42")
  }))
  expect_equal(out$result_response$result$status, "completed")
  expect_false(is.null(out$result_response$result$result))
  expect_length(out$result_response$result$messages, 1L)
  expect_equal(out$result_response$result$result$content[[1L]]$text,
               "the answer is 42")
})

test_that("isError result transitions the task to failed status", {
  out <- call_tool_then_result(new_tool_server(function(args, ctx) {
    response_error("intentional failure")
  }))
  expect_equal(out$result_response$result$status, "failed")
})

test_that("tools without tasks = TRUE do not append anything to any task", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    name = "plain", description = "no task",
    input_schema = schema(list()),
    bidirectional = TRUE,
    handler = function(args, ctx) {
      ctx$send_progress(1, total = 1)
      response_text("ok")
    }))
  sent <- new.env(parent = emptyenv()); sent$msgs <- list()
  s <- mcpserver:::Session$new("s", srv, function(env) {
    sent$msgs <- c(sent$msgs, list(env))
  })
  assign("s", s, envir = srv$sessions)
  s$client_capabilities <- list()

  call_msg <- list(jsonrpc = "2.0", id = 1L,
                   method = "tools/call",
                   params = list(name = "plain",
                                 arguments = list(),
                                 `_meta` = list(progressToken = "tkn")))
  marker <- mcpserver:::route_message(srv, s, call_msg)
  done <- new.env(parent = emptyenv()); done$resp <- NULL
  promises::then(mcpserver:::finalize_async(marker, srv, s),
                 onFulfilled = function(v) done$resp <- v)
  t0 <- Sys.time()
  while (is.null(done$resp) &&
         difftime(Sys.time(), t0, units = "secs") < 5) {
    later::run_now(timeoutSecs = 0.05)
  }
  store <- mcpserver:::ensure_task_store(srv)
  expect_equal(length(ls(store, all.names = TRUE)), 0L)
})

test_that("tasks/cancel clears the queued messages", {
  srv <- new_server("t")
  store <- mcpserver:::ensure_task_store(srv)
  task <- mcpserver:::task_create(store, "x", session_id = "s")
  mcpserver:::task_append_message(store, task$id,
    list(method = "notifications/progress",
         params = list(progress = 1)))
  mcpserver:::task_append_message(store, task$id,
    list(method = "notifications/progress",
         params = list(progress = 2)))
  s <- mcpserver:::Session$new("s", srv, function(env) NULL)
  assign("s", s, envir = srv$sessions)
  msg <- list(jsonrpc = "2.0", id = 1L, method = "tasks/cancel",
              params = list(taskId = task$id))
  out <- mcpserver:::route_message(srv, s, msg)
  expect_false(is.null(out$result))
  t <- mcpserver:::task_get(store, task$id)
  expect_equal(t$status, "cancelled")
  expect_length(t$messages, 0L)
})

test_that("tasks/result on a cancelled task returns no messages", {
  srv <- new_server("t")
  store <- mcpserver:::ensure_task_store(srv)
  task <- mcpserver:::task_create(store, "x", session_id = "s")
  mcpserver:::task_append_message(store, task$id,
    list(method = "notifications/progress",
         params = list(progress = 1)))
  mcpserver:::task_update_status(store, task$id, "cancelled")
  mcpserver:::task_clear_messages(store, task$id)
  s <- mcpserver:::Session$new("s", srv, function(env) NULL)
  assign("s", s, envir = srv$sessions)
  msg <- list(jsonrpc = "2.0", id = 1L, method = "tasks/result",
              params = list(taskId = task$id))
  out <- mcpserver:::route_message(srv, s, msg)
  expect_equal(out$result$status, "cancelled")
  expect_null(out$result$messages)
})
