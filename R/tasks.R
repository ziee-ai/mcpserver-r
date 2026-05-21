# Experimental task store ------------------------------------------------
#
# The MCP spec exposes a `tasks` extension where a tool can run
# asynchronously and the client can poll/cancel it. We back it with an
# in-memory store keyed by task id, and surface a `ctx$task` handle to
# tools that declare `tasks = TRUE`.

new_task_store <- function() {
  new.env(parent = emptyenv())
}

# Ensure the server has a task store; created lazily on first use.
ensure_task_store <- function(server) {
  if (is.null(server$task_store)) {
    server$task_store <- new_task_store()
  }
  server$task_store
}

task_create <- function(store, tool_name, session_id = NULL) {
  id <- new_uuid()
  task <- list(id = id, tool = tool_name,
               status = "pending",
               messages = list(),
               result = NULL,
               session_id = session_id,
               created = Sys.time())
  assign(id, task, envir = store)
  task
}

task_get <- function(store, id) {
  if (exists(id, envir = store, inherits = FALSE)) {
    get(id, envir = store, inherits = FALSE)
  } else NULL
}

task_update_status <- function(store, id, status) {
  t <- task_get(store, id)
  if (is.null(t)) return(invisible(NULL))
  t$status <- status
  t$last_updated <- Sys.time()
  assign(id, t, envir = store)
}

task_append_message <- function(store, id, message) {
  t <- task_get(store, id)
  if (is.null(t)) return(invisible(NULL))
  t$messages <- c(t$messages, list(message))
  assign(id, t, envir = store)
}

# Record the final tool-handler return value on the task so a later
# `tasks/result` call can deliver it. Used by the dispatcher when a
# task-mode tool's promise resolves.
task_set_result <- function(store, id, result) {
  t <- task_get(store, id)
  if (is.null(t)) return(invisible(NULL))
  t$result <- result
  t$last_updated <- Sys.time()
  assign(id, t, envir = store)
}

# Drop the queued messages for a task. Called on `tasks/cancel` so
# the cancelled task doesn't surface any further mid-flight events
# via a later `tasks/result` poll.
task_clear_messages <- function(store, id) {
  t <- task_get(store, id)
  if (is.null(t)) return(invisible(NULL))
  t$messages <- list()
  assign(id, t, envir = store)
}

# Build the `ctx$task` handle exposed to tool handlers when their tool
# declaration set `tasks = TRUE`. Methods: $id, $status(), $update_status(),
# $append_message(), $cancelled().
make_task_handle <- function(store, task_id) {
  handle <- new.env(parent = emptyenv())
  handle$id <- task_id
  handle$.store <- store
  handle$status <- function() {
    t <- task_get(store, task_id)
    if (is.null(t)) NA_character_ else t$status
  }
  handle$update_status <- function(status) {
    task_update_status(store, task_id, status)
  }
  handle$append_message <- function(message) {
    task_append_message(store, task_id, message)
  }
  handle$cancelled <- function() {
    isTRUE(handle$status() == "cancelled")
  }
  class(handle) <- c("McpTask", "environment")
  handle
}

handle_tasks_list <- function(server, session, params, msg = NULL) {
  store <- ensure_task_store(server)
  ids <- ls(store, all.names = TRUE)
  if (length(ids) == 0L) return(list(tasks = j_list(list())))
  list(tasks = j_list(lapply(ids, function(i) {
    t <- get(i, envir = store, inherits = FALSE)
    list(id = t$id, tool = t$tool, status = t$status,
         created = format(t$created, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"))
  })))
}

handle_tasks_cancel <- function(server, session, params, msg) {
  store <- ensure_task_store(server)
  id <- params$taskId %||% params$id
  if (is.null(id) ||
      !exists(id, envir = store, inherits = FALSE)) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params, "unknown task"))
  }
  current <- task_get(store, id)
  if (!is.null(current$session_id) &&
      !identical(current$session_id, session$session_id)) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params,
                      "task does not belong to this session"))
  }
  terminal <- c("completed", "failed", "cancelled")
  if (current$status %in% terminal) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_request,
                      sprintf("task already %s", current$status)))
  }
  task_update_status(store, id, "cancelled")
  task_clear_messages(store, id)
  emit_task_status(session, id, "cancelled")
  list()
}

# Emit a notifications/tasks/status notification for a status change.
emit_task_status <- function(session, task_id, status,
                             status_message = NULL) {
  params <- drop_nulls(list(taskId = task_id,
                            status = status,
                            statusMessage = status_message))
  session$send(jrpc_notification("notifications/tasks/status", params))
}

# tasks/get returns the current state of a task without blocking.
handle_tasks_get <- function(server, session, params, msg) {
  store <- ensure_task_store(server)
  id <- params$taskId %||% params$id
  if (is.null(id) ||
      !exists(id, envir = store, inherits = FALSE)) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params, "unknown task"))
  }
  t <- task_get(store, id)
  # Enforce session ownership — tasks are session-scoped per spec.
  if (!is.null(t$session_id) &&
      !identical(t$session_id, session$session_id)) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params,
                      "task does not belong to this session"))
  }
  last <- t$last_updated %||% t$created
  drop_nulls(list(
    taskId = t$id,
    status = t$status,
    statusMessage = NULL,
    createdAt = format(t$created, "%Y-%m-%dT%H:%M:%OS3Z",
                       tz = "UTC"),
    lastUpdatedAt = format(last, "%Y-%m-%dT%H:%M:%OS3Z",
                           tz = "UTC")
  ))
}

# tasks/result blocks (briefly) until the task reaches a terminal state.
handle_tasks_result <- function(server, session, params, msg) {
  store <- ensure_task_store(server)
  id <- params$taskId %||% params$id
  if (is.null(id) ||
      !exists(id, envir = store, inherits = FALSE)) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params, "unknown task"))
  }
  t <- task_get(store, id)
  if (!is.null(t$session_id) &&
      !identical(t$session_id, session$session_id)) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params,
                      "task does not belong to this session"))
  }
  terminal <- c("completed", "failed", "cancelled")
  deadline <- Sys.time() + 5
  while (!(t$status %in% terminal) && Sys.time() < deadline) {
    later::run_now(timeoutSecs = 0.05)
    t <- task_get(store, id)
  }
  drop_nulls(list(taskId = t$id,
                  status = t$status,
                  result = t$result,
                  messages = if (length(t$messages) > 0L) {
                    j_list(t$messages)
                  } else NULL))
}
