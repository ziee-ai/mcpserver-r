# Resource subscription notification helpers ------------------------------

#' Notify clients that a subscribed resource was updated
#'
#' Sends `notifications/resources/updated` to every session that has
#' subscribed to the given URI. If `session_id` is supplied, only that
#' session is notified.
#'
#' @param mcp An `McpServer`.
#' @param uri Resource URI that changed.
#' @param session_id Optional session id to target a single client.
#' @return `NULL`, invisibly.
#' @export
notify_resource_updated <- function(mcp, uri, session_id = NULL) {
  emit_to_sessions(mcp, session_id, function(sess) {
    if (exists(uri, envir = sess$subs, inherits = FALSE)) {
      sess$send(jrpc_notification("notifications/resources/updated",
                                  list(uri = uri)))
    }
  })
}

#' Notify clients that the resource list changed
#'
#' @param mcp An `McpServer`.
#' @param session_id Optional session id to target a single client.
#' @return `NULL`, invisibly.
#' @export
notify_resource_list_changed <- function(mcp, session_id = NULL) {
  emit_to_sessions(mcp, session_id, function(sess) {
    sess$send(jrpc_notification("notifications/resources/list_changed"))
  })
}

#' Notify clients that the tool list changed
#'
#' @param mcp An `McpServer`.
#' @param session_id Optional session id to target a single client.
#' @return `NULL`, invisibly.
#' @export
notify_tool_list_changed <- function(mcp, session_id = NULL) {
  emit_to_sessions(mcp, session_id, function(sess) {
    sess$send(jrpc_notification("notifications/tools/list_changed"))
  })
}

#' Notify clients that the prompt list changed
#'
#' @param mcp An `McpServer`.
#' @param session_id Optional session id to target a single client.
#' @return `NULL`, invisibly.
#' @export
notify_prompt_list_changed <- function(mcp, session_id = NULL) {
  emit_to_sessions(mcp, session_id, function(sess) {
    sess$send(jrpc_notification("notifications/prompts/list_changed"))
  })
}

emit_to_sessions <- function(mcp, session_id, fn) {
  if (!is.null(session_id)) {
    if (exists(session_id, envir = mcp$sessions, inherits = FALSE)) {
      fn(get(session_id, envir = mcp$sessions, inherits = FALSE))
    }
    return(invisible(NULL))
  }
  for (k in ls(mcp$sessions, all.names = TRUE)) {
    safely(fn(get(k, envir = mcp$sessions, inherits = FALSE)),
           log = FALSE)
  }
  invisible(NULL)
}
