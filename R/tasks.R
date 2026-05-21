# Experimental task store ------------------------------------------------
#
# The MCP spec exposes a `tasks` extension where a tool can run
# asynchronously and the client can poll/cancel it. We back it with an
# in-memory store keyed by task id.

new_task_store <- function() {
  new.env(parent = emptyenv())
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

handle_tasks_list <- function(server, session, params, msg = NULL) {
  ids <- ls(server$.task_store %||% new_task_store(), all.names = TRUE)
  if (length(ids) == 0L) return(list(tasks = j_list(list())))
  store <- server$.task_store
  list(tasks = j_list(lapply(ids, function(i) {
    t <- get(i, envir = store, inherits = FALSE)
    list(id = t$id, tool = t$tool, status = t$status,
         created = format(t$created, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"))
  })))
}

handle_tasks_cancel <- function(server, session, params, msg) {
  id <- params$id
  store <- server$.task_store
  if (is.null(store) || is.null(id) ||
      !exists(id, envir = store, inherits = FALSE)) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params, "unknown task"))
  }
  task_update_status(store, id, "cancelled")
  list()
}
