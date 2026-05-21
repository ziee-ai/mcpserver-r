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
  has_method <- "method" %in% names(msg)
  has_result <- "result" %in% names(msg)
  has_error  <- "error" %in% names(msg)
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
