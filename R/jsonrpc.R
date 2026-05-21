# JSON-RPC 2.0 protocol layer ---------------------------------------------

# Standard JSON-RPC 2.0 error codes plus the application-defined range
# reserved by the spec (-32099..-32000).
jrpc_codes <- list(
  parse_error      = -32700L,
  invalid_request  = -32600L,
  method_not_found = -32601L,
  invalid_params   = -32602L,
  internal_error   = -32603L
)

# Build a JSON-RPC request envelope. id may be character, integer, or NULL
# (for notifications).
jrpc_request <- function(method, params = NULL, id = NULL) {
  out <- list(jsonrpc = JSONRPC_VERSION, method = method)
  if (!is.null(params)) out$params <- params
  if (!is.null(id)) out$id <- id
  out
}

# Build a successful response envelope. An empty list result is coerced
# to a JSON object (`{}`) rather than a JSON array (`[]`) to satisfy
# strict MCP clients that validate the result shape.
jrpc_response <- function(id, result) {
  if (is.list(result) && length(result) == 0L &&
      is.null(names(result))) {
    result <- j_empty_obj()
  }
  list(jsonrpc = JSONRPC_VERSION, id = id, result = result)
}

# Build an error response envelope.
jrpc_error <- function(id, code, message, data = NULL) {
  err <- list(code = as.integer(code), message = as.character(message))
  if (!is.null(data)) err$data <- data
  list(jsonrpc = JSONRPC_VERSION, id = id, error = err)
}

# Build a notification envelope (no id).
jrpc_notification <- function(method, params = NULL) {
  out <- list(jsonrpc = JSONRPC_VERSION, method = method)
  if (!is.null(params)) out$params <- params
  out
}

# Classify a parsed JSON-RPC value. Returns one of:
#   "request"      — has id and method
#   "notification" — has method but no id
#   "response"     — has id and (result or error) but no method
#   "invalid"      — anything else
jrpc_kind <- function(msg) {
  if (!is.list(msg)) return("invalid")
  has_id     <- "id" %in% names(msg)
  has_method <- "method" %in% names(msg) &&
                is.character(msg$method) &&
                length(msg$method) == 1L &&
                nzchar(msg$method)
  has_result <- "result" %in% names(msg)
  has_error  <- "error" %in% names(msg)
  # Reject envelopes whose declared jsonrpc isn't "2.0" — they're
  # protocol-incompatible. Missing jsonrpc is permissive (some
  # malformed inputs omit it).
  if ("jsonrpc" %in% names(msg) && !identical(msg$jsonrpc, "2.0")) {
    return("invalid")
  }
  if (has_method && has_id) return("request")
  if (has_method && !has_id) return("notification")
  if (has_id && (has_result || has_error)) return("response")
  "invalid"
}

# Encode an envelope (or list of envelopes for batch) to JSON text.
jrpc_encode <- function(envelope) {
  to_json(envelope)
}

# Decode a JSON text or raw vector into a parsed envelope. Returns NULL on
# malformed input. Batches (top-level JSON arrays) are returned as-is so
# the caller can iterate.
jrpc_decode <- function(text) {
  from_json(text)
}

#' Test whether `x` is a well-formed JSON-RPC 2.0 response envelope
#'
#' A response carries `jsonrpc = "2.0"`, an `id`, and either a `result`
#' or an `error` (but not both). Requests and notifications are
#' rejected.
#'
#' @param x Any R object.
#' @return `TRUE` / `FALSE`.
#' @export
#' @examples
#' is_jsonrpc_response(list(jsonrpc = "2.0", id = 1, result = list()))
is_jsonrpc_response <- function(x) {
  if (!is.list(x)) return(FALSE)
  if (!identical(x$jsonrpc, "2.0")) return(FALSE)
  if (is.null(x$id)) return(FALSE)
  if ("method" %in% names(x)) return(FALSE)
  has_result <- "result" %in% names(x)
  has_error <- "error" %in% names(x)
  # MUST have exactly one of result / error
  xor(has_result, has_error)
}

#' Test whether `x` is a `tools/call` result envelope
#'
#' A `CallToolResult` carries a non-empty `content` array (each item
#' has a `type`) and may carry `isError` and `structuredContent`.
#'
#' @param x Any R object.
#' @return `TRUE` / `FALSE`.
#' @export
#' @examples
#' is_call_tool_result(list(content = list(list(type = "text", text = "x"))))
is_call_tool_result <- function(x) {
  if (!is.list(x)) return(FALSE)
  if (is.null(x$content)) return(FALSE)
  if (!is.list(x$content)) return(FALSE)
  for (item in x$content) {
    if (!is.list(item) ||
        is.null(item$type) ||
        !is.character(item$type) ||
        length(item$type) != 1L) {
      return(FALSE)
    }
  }
  TRUE
}
