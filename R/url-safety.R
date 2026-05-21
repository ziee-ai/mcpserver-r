# URL safety helpers. Mirrors core/test/shared/auth.test.ts SafeUrlSchema
# semantics — accept http/https, reject javascript:, data:, file:, and
# friends. Used by oauth_config validation and exposed for general use.

#' Test whether a URL string is in the safe HTTP(S) scheme set
#'
#' Rejects `javascript:`, `data:`, `vbscript:`, `file:`, `about:`, and
#' any other non-HTTP(S) scheme.
#'
#' @param url Single character URL.
#' @return `TRUE` / `FALSE`.
#' @export
#' @examples
#' is_safe_url("https://example.com/x")
#' is_safe_url("javascript:alert(1)")
is_safe_url <- function(url) {
  if (!is.character(url) || length(url) != 1L || is.na(url)) {
    return(FALSE)
  }
  if (!nzchar(url)) return(FALSE)
  # Anchored scheme match.
  m <- regmatches(url, regexec("^([A-Za-z][A-Za-z0-9+.-]*):", url))[[1L]]
  if (length(m) < 2L) return(FALSE)
  scheme <- tolower(m[[2L]])
  scheme %in% c("http", "https")
}

#' Resource URL canonicalisation: strip fragment, normalise trailing slash
#'
#' Mirrors `resourceUrlFromServerUrl()` in
#' `packages/core/src/shared/authUtils.ts`. Used to derive the
#' `resource` indicator from a base server URL.
#'
#' @param url Single character URL.
#' @return The URL with any `#fragment` removed.
#' @export
#' @examples
#' resource_url_from_server_url("https://api.example/mcp#frag")
resource_url_from_server_url <- function(url) {
  stopifnot(is.character(url), length(url) == 1L)
  sub("#.*$", "", url)
}

#' Test whether two resource URLs identify the same MCP resource
#'
#' Identity match after fragment removal. Path / domain / port
#' differences disqualify; trailing slash is significant per RFC 8615.
#'
#' @param a,b Single character URLs.
#' @return `TRUE` when both URLs canonicalise to the same resource.
#' @export
#' @examples
#' resource_matches("https://api.example/mcp",
#'                  "https://api.example/mcp#x")
resource_matches <- function(a, b) {
  identical(resource_url_from_server_url(a),
            resource_url_from_server_url(b))
}
