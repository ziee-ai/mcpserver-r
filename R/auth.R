# OAuth 2.1 resource-server validation -----------------------------------

#' Configure OAuth 2.1 bearer-token validation
#'
#' Returns a configuration object accepted by [serve_http()]'s `auth`
#' argument. Two flows are supported:
#'
#' * **JWT** (default): tokens are decoded and validated locally against a
#'   JWKS endpoint. `iss`, `aud`, `exp`, `nbf`, and required scopes are
#'   checked, with `leeway` seconds of clock skew tolerated.
#' * **Introspection**: when `introspection_url` is supplied, opaque
#'   tokens are validated by POSTing to that endpoint per RFC 7662.
#'
#' @param issuer Expected `iss` claim and base URL for the resource
#'   metadata pointer in `WWW-Authenticate`.
#' @param audience Required `aud` claim (this server's resource indicator).
#' @param jwks_uri JWKS endpoint URL.
#' @param required_scopes Optional character vector of scopes that must all
#'   be present.
#' @param introspection_url Optional RFC 7662 introspection endpoint.
#' @param introspection_basic Optional named list with `client_id` and
#'   `client_secret` used for HTTP Basic auth on the introspection call.
#' @param leeway Clock-skew tolerance in seconds (default 30).
#' @return A list with class `"mcp_oauth_config"`.
#' @export
oauth_config <- function(issuer,
                         audience,
                         jwks_uri,
                         required_scopes = character(0L),
                         introspection_url = NULL,
                         introspection_basic = NULL,
                         leeway = 30L) {
  cfg <- list(
    issuer = as.character(issuer),
    audience = as.character(audience),
    jwks_uri = as.character(jwks_uri),
    required_scopes = as.character(required_scopes),
    introspection_url = introspection_url,
    introspection_basic = introspection_basic,
    leeway = as.integer(leeway),
    jwks_cache = new.env(parent = emptyenv()),
    introspection_cache = new.env(parent = emptyenv())
  )
  class(cfg) <- "mcp_oauth_config"
  cfg
}

# Test-only helper to short-circuit the network for unit tests.
oauth_set_jwks <- function(cfg, jwks) {
  cfg$jwks_cache$json <- jwks
  cfg$jwks_cache$expires <- Sys.time() + 3600
  invisible(cfg)
}

oauth_fetch_jwks <- function(cfg, force = FALSE) {
  if (!force && !is.null(cfg$jwks_cache$json) &&
      !is.null(cfg$jwks_cache$expires) &&
      Sys.time() < cfg$jwks_cache$expires) {
    return(cfg$jwks_cache$json)
  }
  json <- tryCatch({
    resp <- httr2::req_perform(httr2::request(cfg$jwks_uri))
    httr2::resp_body_string(resp)
  }, error = function(e) NULL)
  if (is.null(json)) return(cfg$jwks_cache$json)
  cfg$jwks_cache$json <- json
  cfg$jwks_cache$expires <- Sys.time() + 3600
  json
}

# Find a JWK by `kid`. Returns the parsed JWK list or NULL.
oauth_find_jwk <- function(cfg, kid) {
  json <- oauth_fetch_jwks(cfg)
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  keys <- parsed$keys %||% list()
  for (k in keys) {
    if (identical(k$kid, kid)) return(k)
  }
  # kid miss — refresh once and try again.
  json <- oauth_fetch_jwks(cfg, force = TRUE)
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  for (k in parsed$keys %||% list()) {
    if (identical(k$kid, kid)) return(k)
  }
  NULL
}

# Parse a Bearer header and return the raw token, or NULL.
oauth_extract_bearer <- function(authz) {
  if (is.null(authz)) return(NULL)
  m <- regmatches(authz, regexec("^Bearer\\s+(.+)$", authz,
                                 ignore.case = TRUE))[[1L]]
  if (length(m) >= 2L) m[[2L]] else NULL
}

# Top-level validator. Returns list(ok, subject, scopes).
oauth_verify_bearer <- function(cfg, authz) {
  token <- oauth_extract_bearer(authz)
  if (is.null(token)) return(list(ok = FALSE))
  if (!is.null(cfg$introspection_url)) {
    return(oauth_verify_introspection(cfg, token))
  }
  oauth_verify_jwt(cfg, token)
}

oauth_verify_jwt <- function(cfg, token) {
  parts <- strsplit(token, ".", fixed = TRUE)[[1L]]
  if (length(parts) != 3L) {
    return(list(ok = FALSE, reason = "invalid_token"))
  }
  header <- safely(jsonlite::fromJSON(
    rawToChar(jose::base64url_decode(parts[[1L]])),
    simplifyVector = FALSE), log = FALSE)
  if (is.null(header)) return(list(ok = FALSE, reason = "invalid_token"))
  jwk <- oauth_find_jwk(cfg, header$kid)
  if (is.null(jwk)) return(list(ok = FALSE, reason = "invalid_token"))
  pub <- safely(jose::read_jwk(jsonlite::toJSON(jwk, auto_unbox = TRUE)),
                log = FALSE)
  if (is.null(pub)) return(list(ok = FALSE, reason = "invalid_token"))
  decoded <- safely(jose::jwt_decode_sig(token, pubkey = pub), log = FALSE)
  if (is.null(decoded)) {
    return(list(ok = FALSE, reason = "invalid_token"))
  }
  now <- as.numeric(Sys.time())
  if (!identical(decoded$iss, cfg$issuer)) {
    return(list(ok = FALSE, reason = "invalid_token"))
  }
  if (!isTRUE(claim_contains(decoded$aud, cfg$audience))) {
    return(list(ok = FALSE, reason = "invalid_token"))
  }
  if (!is.null(decoded$exp) &&
      now > as.numeric(decoded$exp) + cfg$leeway) {
    return(list(ok = FALSE, reason = "invalid_token"))
  }
  if (!is.null(decoded$nbf) &&
      now < as.numeric(decoded$nbf) - cfg$leeway) {
    return(list(ok = FALSE, reason = "invalid_token"))
  }
  scopes <- parse_scope(decoded$scope, decoded$scp)
  if (length(cfg$required_scopes) > 0L &&
      !all(cfg$required_scopes %in% scopes)) {
    return(list(ok = FALSE, reason = "insufficient_scope",
                scopes = scopes))
  }
  list(ok = TRUE,
       subject = decoded$sub,
       scopes = scopes)
}

oauth_verify_introspection <- function(cfg, token) {
  key <- digest_token(token)
  if (exists(key, envir = cfg$introspection_cache, inherits = FALSE)) {
    cached <- get(key, envir = cfg$introspection_cache, inherits = FALSE)
    if (Sys.time() < cached$expires) return(cached$value)
  }
  req <- httr2::request(cfg$introspection_url) |>
    httr2::req_method("POST") |>
    httr2::req_body_form(token = token, token_type_hint = "access_token")
  if (!is.null(cfg$introspection_basic)) {
    req <- httr2::req_auth_basic(req,
                                 cfg$introspection_basic$client_id,
                                 cfg$introspection_basic$client_secret)
  }
  resp <- safely(httr2::req_perform(req), log = TRUE)
  if (is.null(resp)) return(list(ok = FALSE))
  body <- safely(jsonlite::fromJSON(httr2::resp_body_string(resp),
                                    simplifyVector = FALSE), log = FALSE)
  if (is.null(body) || !isTRUE(body$active)) return(list(ok = FALSE))
  if (!identical(body$iss, cfg$issuer)) return(list(ok = FALSE))
  if (!isTRUE(claim_contains(body$aud, cfg$audience))) {
    return(list(ok = FALSE))
  }
  scopes <- parse_scope(body$scope, body$scp)
  if (length(cfg$required_scopes) > 0L &&
      !all(cfg$required_scopes %in% scopes)) {
    return(list(ok = FALSE))
  }
  ttl <- if (!is.null(body$exp)) {
    max(1, as.numeric(body$exp) - as.numeric(Sys.time()) - cfg$leeway)
  } else 60
  val <- list(ok = TRUE,
              subject = body$sub,
              scopes = scopes)
  assign(key,
         list(value = val, expires = Sys.time() + ttl),
         envir = cfg$introspection_cache)
  val
}

claim_contains <- function(claim, value) {
  if (is.null(claim)) return(FALSE)
  if (is.character(claim)) return(value %in% claim)
  if (is.list(claim)) return(value %in% unlist(claim))
  FALSE
}

parse_scope <- function(scope, scp = NULL) {
  if (!is.null(scope) && is.character(scope) && length(scope) == 1L) {
    return(strsplit(scope, "\\s+")[[1L]])
  }
  if (!is.null(scp)) {
    return(as.character(unlist(scp)))
  }
  character(0L)
}

digest_token <- function(token) {
  paste(openssl::sha256(charToRaw(token)), collapse = "")
}
