# User-facing extension surface: setRequestHandler / setNotificationHandler.
#
# Mirrors the TypeScript SDK's `server.setRequestHandler(method, handler)`
# pattern, letting consumers wire in custom MCP request methods without
# patching the dispatcher. Handlers are stored on the McpServer instance
# and consulted by `route_message()` after the static METHOD_TABLE.

#' Register a custom request handler on the server
#'
#' Use this to extend the server with methods the package doesn't
#' implement natively, or to override a built-in handler. The handler is
#' invoked synchronously on the transport thread; if you need to do
#' off-thread work, return a [promises::promise()].
#'
#' @param mcp An `McpServer` (see [new_server()]).
#' @param method Method name, e.g. `"experimental/x"`.
#' @param handler A function `function(server, session, params, msg)`
#'   returning the JSON-RPC `result` payload (a list).
#' @return `mcp`, invisibly.
#' @export
set_request_handler <- function(mcp, method, handler) {
  stopifnot(inherits(mcp, "McpServer"))
  stopifnot(is.character(method), length(method) == 1L)
  stopifnot(is.function(handler))
  if (isTRUE(mcp$strict_capabilities)) {
    caps <- mcp$capabilities()
    method_root <- sub("/.*$", "", method)
    if (!method_root %in% names(caps) &&
        !method_root %in% c("notifications", "initialize", "ping")) {
      stop(sprintf(
        "strict_capabilities: cannot register handler for '%s' — no '%s' capability declared",
        method, method_root))
    }
  }
  if (is.null(mcp$custom_request_handlers)) {
    mcp$custom_request_handlers <- new.env(parent = emptyenv())
  }
  assign(method, handler, envir = mcp$custom_request_handlers)
  invisible(mcp)
}

#' Register a custom notification handler on the server
#'
#' Returns the server invisibly. Notification handlers run synchronously
#' and their return values are discarded.
#'
#' @param mcp An `McpServer`.
#' @param method Notification method name, e.g.
#'   `"notifications/experimental/x"`.
#' @param handler A function `function(server, session, params)`.
#' @return `mcp`, invisibly.
#' @export
set_notification_handler <- function(mcp, method, handler) {
  stopifnot(inherits(mcp, "McpServer"))
  stopifnot(is.character(method), length(method) == 1L)
  stopifnot(is.function(handler))
  if (is.null(mcp$custom_notification_handlers)) {
    mcp$custom_notification_handlers <- new.env(parent = emptyenv())
  }
  assign(method, handler, envir = mcp$custom_notification_handlers)
  invisible(mcp)
}

#' Register a fallback request handler
#'
#' Called when no built-in or user-registered handler matches a request.
#' Useful for plug-in / proxy patterns.
#'
#' @param mcp An `McpServer`.
#' @param handler A function `function(server, session, params, msg)`
#'   returning the JSON-RPC `result` payload.
#' @return `mcp`, invisibly.
#' @export
set_fallback_request_handler <- function(mcp, handler) {
  stopifnot(inherits(mcp, "McpServer"))
  stopifnot(is.function(handler))
  mcp$fallback_request_handler <- handler
  invisible(mcp)
}

#' Register a fallback notification handler
#'
#' Called when no built-in or user-registered handler matches an
#' incoming notification.
#'
#' @param mcp An `McpServer`.
#' @param handler A function `function(server, session, params)`.
#' @return `mcp`, invisibly.
#' @export
set_fallback_notification_handler <- function(mcp, handler) {
  stopifnot(inherits(mcp, "McpServer"))
  stopifnot(is.function(handler))
  mcp$fallback_notification_handler <- handler
  invisible(mcp)
}
