#' Build a typed MCP error condition
#'
#' Handlers can `stop()` with the value returned here to surface a
#' specific JSON-RPC error code and optional structured `data` field at
#' the protocol layer. Mirrors the TypeScript SDK's `McpError` class.
#'
#' @param message Error message.
#' @param code Integer JSON-RPC error code (defaults to `-32603`
#'   internal error). Use the constants in `mcp_error_codes()` or any
#'   application-defined code in `-32000..-32099`.
#' @param data Optional list with extra error data (echoed in the
#'   response's `error.data` field).
#' @return A condition that, when caught by the dispatcher, is turned
#'   into a JSON-RPC error envelope with the supplied code/data.
#' @export
#' @examples
#' err <- mcp_error("invalid input",
#'                  code = mcp_error_codes()$invalid_params,
#'                  data = list(arg = "x"))
#' inherits(err, "mcp_error")
mcp_error <- function(message,
                      code = -32603L,
                      data = NULL) {
  structure(
    class = c("mcp_error", "error", "condition"),
    list(message = as.character(message),
         call = sys.call(-1L),
         code = as.integer(code),
         data = data))
}

#' Standard MCP / JSON-RPC error codes
#'
#' @return A named list of integer codes accessible via `$` or `[[`.
#' @export
#' @examples
#' mcp_error_codes()$method_not_found
mcp_error_codes <- function() {
  list(
    parse_error      = -32700L,
    invalid_request  = -32600L,
    method_not_found = -32601L,
    invalid_params   = -32602L,
    internal_error   = -32603L,
    unauthorized     = -32001L,
    insufficient_scope = -32002L
  )
}
