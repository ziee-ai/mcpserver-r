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

task_create <- function(store, tool_name) {
  id <- new_uuid()
  task <- list(id = id, tool = tool_name,
               status = "pending",
               messages = list(),
               result = NULL,
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
  assign(id, t, envir = store)
}

task_append_message <- function(store, id, message) {
  t <- task_get(store, id)
  if (is.null(t)) return(invisible(NULL))
  t$messages <- c(t$messages, list(message))
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
  id <- params$id
  if (is.null(id) ||
      !exists(id, envir = store, inherits = FALSE)) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params, "unknown task"))
  }
  task_update_status(store, id, "cancelled")
  list()
}
