#' @keywords internal
#' @importFrom rlang %||%
#' @importFrom utils packageVersion
#' @importFrom R6 R6Class
NULL

# Constants ---------------------------------------------------------------

#' MCP Protocol Version
#'
#' Returns the *latest* MCP revision implemented by this package. Servers
#' advertise this string in their `initialize` response when the client
#' requests an unknown revision.
#'
#' @return A scalar character vector.
#' @export
#' @examples
#' mcp_protocol_version()
mcp_protocol_version <- function() "2025-11-25"

#' MCP protocol revisions accepted by this server
#'
#' Ordered newest to oldest. The HTTP transport accepts an incoming
#' `MCP-Protocol-Version` header that matches any of these.
#'
#' @return A character vector.
#' @export
#' @examples
#' mcp_supported_protocol_versions()
mcp_supported_protocol_versions <- function() {
  c("2025-11-25", "2025-06-18", "2025-03-26", "2024-11-05")
}

#' Negotiate the protocol version with an initializing client.
#'
#' @param requested The `protocolVersion` field from the client's
#'   `initialize` request, or `NULL` if absent.
#' @return The version string the server should respond with.
#' @export
#' @examples
#' negotiate_protocol_version("2024-11-05")
#' negotiate_protocol_version("2099-01-01")
negotiate_protocol_version <- function(requested) {
  supported <- mcp_supported_protocol_versions()
  if (!is.null(requested) && requested %in% supported) requested
  else supported[[1L]]
}

JSONRPC_VERSION <- "2.0"

LOG_LEVELS <- c("debug", "info", "notice", "warning",
                "error", "critical", "alert", "emergency")

# Internal helpers --------------------------------------------------------

# Force jsonlite to emit `[]` rather than `{}` for any vector or list that
# represents a JSON array. Atomic vectors (length 0, 1, or N) are wrapped
# with I() so auto_unbox does not collapse length-1 vectors to scalars.
j_list <- function(x) {
  if (is.null(x)) return(I(list()))
  if (is.list(x) && length(x) == 0L) return(I(list()))
  if (is.list(x) && is.null(names(x))) return(I(x))
  if (is.atomic(x)) return(I(x))
  x
}

# Force jsonlite to emit `{}` for an empty JSON object. Used by capability
# declarations such as `logging: {}` and `completions: {}`.
j_empty_obj <- function() {
  structure(list(), .Names = character(0L))
}

# Drop NULL entries so optional fields don't serialise as `"key":null`.
drop_nulls <- function(x) {
  if (is.null(x)) return(NULL)
  x[!vapply(x, is.null, logical(1L))]
}

# RFC 4122 v4 UUID without external dependencies.
new_uuid <- function() {
  bytes <- openssl::rand_bytes(16L)
  bytes[7L] <- as.raw(bitwOr(bitwAnd(as.integer(bytes[7L]), 0x0F), 0x40))
  bytes[9L] <- as.raw(bitwOr(bitwAnd(as.integer(bytes[9L]), 0x3F), 0x80))
  hex <- paste(format(bytes), collapse = "")
  paste(substr(hex, 1L, 8L),
        substr(hex, 9L, 12L),
        substr(hex, 13L, 16L),
        substr(hex, 17L, 20L),
        substr(hex, 21L, 32L),
        sep = "-")
}

# Render an R value to a JSON string using the MCP serialisation defaults.
to_json <- function(x, pretty = FALSE) {
  jsonlite::toJSON(x,
                   auto_unbox = TRUE,
                   null = "null",
                   na = "null",
                   pretty = pretty,
                   force = TRUE)
}

# Parse JSON text into an R list. Returns NULL on malformed input.
from_json <- function(text) {
  if (is.raw(text)) text <- rawToChar(text)
  tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE),
           error = function(e) NULL)
}

# Look up a header from the named character vector that nanonext provides.
# The lookup is case-insensitive (HTTP header names are not case-sensitive).
header_get <- function(headers, name, default = NULL) {
  if (is.null(headers) || length(headers) == 0L) return(default)
  nm <- names(headers)
  if (is.null(nm)) return(default)
  hit <- tolower(nm) == tolower(name)
  if (!any(hit)) return(default)
  unname(headers[hit][[1L]])
}

# Run a callable, returning NULL on error (logged to stderr if log = TRUE).
safely <- function(expr, log = FALSE) {
  tryCatch(force(expr),
           error = function(e) {
             if (isTRUE(log)) message("mcpserver: ", conditionMessage(e))
             NULL
           })
}

# Convert a character/raw value to character (UTF-8), keeping NULL.
as_char <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.raw(x)) return(rawToChar(x))
  as.character(x)
}
