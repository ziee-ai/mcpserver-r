# OAuth 2.1 authorization server (minimal, demo-grade) --------------------

# This module implements a small, self-contained OAuth authorization
# server alongside the MCP resource-server side already provided by
# `oauth_config()`. It is intentionally minimal — PKCE-S256 only,
# `authorization_code` + `refresh_token` grants, dynamic client
# registration (RFC 7591), and a single signing key exposed via JWKS.
# Auto-consent is the default so tests can drive the flow without a
# real user agent. It is **not** suitable for production deployments;
# `oauth_server_config()` documents that loudly.
#
# The five handlers each return a `http_make_response()` list — the
# same shape the rest of `R/transport_http.R` produces — so wiring is
# just a matter of appending them to the nanonext handler list.

#' Configure an OAuth 2.1 authorization server
#'
#' Builds an in-memory authorization server suitable for development,
#' integration tests, and conformance fixtures. Exposes five
#' endpoints (metadata, /authorize, /token, /register, /jwks) and
#' mints RS256 JWT access + refresh tokens signed with a key
#' generated on first call (override via `signing_key`).
#'
#' **Not for production**: this server has auto-consent enabled by
#' default, stores all state in memory, and does not implement
#' rate-limiting or audit logging. Use a real IdP (Auth0, Keycloak,
#' Okta) in production.
#'
#' @param issuer The HTTPS URL this AS uses as its `iss` claim and
#'   metadata document base URL.
#' @param audience The resource indicator (the MCP server's public
#'   URL) put into the `aud` claim.
#' @param signing_key An `openssl::rsa_keygen()` result. Auto-
#'   generated (2048-bit) if `NULL`.
#' @param kid Key id put into the JWK and JWT header.
#' @param ttl_access Access-token lifetime in seconds.
#' @param ttl_refresh Refresh-token lifetime in seconds.
#' @param ttl_code Authorization-code lifetime in seconds.
#' @param scopes_supported Character vector of scopes advertised in
#'   the metadata document.
#' @param auto_consent When `TRUE` (default), `/authorize` responds
#'   with a 302 redirect carrying the authorization code; useful for
#'   tests. When `FALSE`, returns an HTML consent form.
#' @param subject Fixed `sub` claim. Real deployments would derive
#'   this from the authenticated user.
#' @param consent_html_fn Optional `function(req, cfg)` returning the
#'   consent page HTML. Only consulted when `auto_consent = FALSE`.
#' @param client_store,code_store,token_store Pluggable in-memory
#'   stores; defaults are `new_oauth_store()`.
#' @return A list of class `"mcp_oauth_server_config"` with helper
#'   accessors. Pass to [serve_http()] via the `oauth_as` argument.
#' @export
oauth_server_config <- function(issuer,
                                audience,
                                signing_key   = NULL,
                                kid           = "as-key-1",
                                ttl_access    = 3600L,
                                ttl_refresh   = 86400L,
                                ttl_code      = 60L,
                                scopes_supported = c("mcp:read",
                                                     "mcp:write"),
                                auto_consent  = TRUE,
                                subject       = "anonymous",
                                consent_html_fn = NULL,
                                client_store  = new_oauth_store(),
                                code_store    = new_oauth_store(),
                                token_store   = new_oauth_store()) {
  if (is.null(signing_key)) {
    signing_key <- openssl::rsa_keygen(2048L)
  }
  cfg <- list(
    issuer        = sub("/+$", "", as.character(issuer)),
    audience      = as.character(audience),
    signing_key   = signing_key,
    kid           = as.character(kid),
    ttl_access    = as.integer(ttl_access),
    ttl_refresh   = as.integer(ttl_refresh),
    ttl_code      = as.integer(ttl_code),
    scopes_supported = as.character(scopes_supported),
    auto_consent  = isTRUE(auto_consent),
    subject       = as.character(subject),
    consent_html_fn = consent_html_fn,
    client_store  = client_store,
    code_store    = code_store,
    token_store   = token_store
  )
  class(cfg) <- "mcp_oauth_server_config"
  cfg
}

# In-memory key/value store interface ------------------------------------

# Returns an opaque store with `$add(id, value)`, `$get(id)`,
# `$revoke(id)`, and `$list()`. Defaulted at construction time;
# accepting one in `oauth_server_config()` is enough to let users
# swap in Redis/SQLite/file-backed alternatives later.
new_oauth_store <- function() {
  e <- new.env(parent = emptyenv())
  list(
    add    = function(id, value) assign(as.character(id), value,
                                        envir = e),
    get    = function(id) {
      key <- as.character(id)
      if (exists(key, envir = e, inherits = FALSE)) {
        get(key, envir = e, inherits = FALSE)
      } else NULL
    },
    revoke = function(id) {
      key <- as.character(id)
      if (exists(key, envir = e, inherits = FALSE)) {
        rm(list = key, envir = e)
      }
      invisible()
    },
    list   = function() ls(e, all.names = TRUE)
  )
}

# JWKS construction ------------------------------------------------------

# Build the AS's public JWK (a single key set with one RS256 key).
# Used by both `/jwks` and the auto-built resource-server `oauth_config`
# when no separate `auth` was provided.
oauth_as_jwks_json <- function(cfg) {
  pub <- openssl::write_pkcs1(cfg$signing_key$pubkey)
  # jose::write_jwk emits a JSON string for a single JWK; we wrap it
  # in a JWKS document and decorate with our kid + use.
  jwk_json <- jose::write_jwk(cfg$signing_key$pubkey)
  jwk <- jsonlite::fromJSON(jwk_json, simplifyVector = FALSE)
  jwk$kid <- cfg$kid
  jwk$use <- "sig"
  jwk$alg <- "RS256"
  jsonlite::toJSON(list(keys = list(jwk)),
                   auto_unbox = TRUE, force = TRUE)
}

# Metadata handler (RFC 8414) --------------------------------------------

oauth_as_metadata_handler <- function(cfg) {
  function(req) {
    body <- jsonlite::toJSON(list(
      issuer = cfg$issuer,
      authorization_endpoint = paste0(cfg$issuer, "/authorize"),
      token_endpoint = paste0(cfg$issuer, "/token"),
      registration_endpoint = paste0(cfg$issuer, "/register"),
      jwks_uri = paste0(cfg$issuer, "/jwks"),
      scopes_supported = I(cfg$scopes_supported),
      response_types_supported = I("code"),
      grant_types_supported = I(c("authorization_code",
                                  "refresh_token")),
      code_challenge_methods_supported = I("S256"),
      token_endpoint_auth_methods_supported = I("none")
    ), auto_unbox = TRUE, force = TRUE)
    http_make_response(200L, body = body, json = TRUE)
  }
}

# JWKS handler -----------------------------------------------------------

oauth_as_jwks_handler <- function(cfg) {
  function(req) {
    http_make_response(200L, body = oauth_as_jwks_json(cfg),
                       json = TRUE)
  }
}

# Query-string / form-body parsing helpers --------------------------------

# Extract the query portion of a `req$uri` (`/authorize?foo=bar&baz`)
# and return a named character vector of decoded values.
parse_query_string <- function(qs) {
  if (is.null(qs) || identical(qs, "")) return(character(0L))
  if (substr(qs, 1L, 1L) == "?") qs <- substring(qs, 2L)
  parts <- strsplit(qs, "&", fixed = TRUE)[[1L]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) return(character(0L))
  out <- character(length(parts))
  names_ <- character(length(parts))
  for (i in seq_along(parts)) {
    kv <- strsplit(parts[[i]], "=", fixed = TRUE)[[1L]]
    names_[[i]] <- utils::URLdecode(kv[[1L]])
    out[[i]] <- if (length(kv) >= 2L) {
      utils::URLdecode(paste(kv[-1L], collapse = "="))
    } else ""
  }
  stats::setNames(out, names_)
}

uri_query_part <- function(uri) {
  if (is.null(uri)) return("")
  m <- regmatches(uri, regexec("\\?(.*)$", uri))[[1L]]
  if (length(m) >= 2L) m[[2L]] else ""
}

# POST body parsers ------------------------------------------------------

req_body_text <- function(req) {
  if (is.null(req$body)) return("")
  if (is.raw(req$body)) rawToChar(req$body) else as.character(req$body)
}

req_form_params <- function(req) {
  parse_query_string(req_body_text(req))
}

req_json_body <- function(req) {
  text <- req_body_text(req)
  if (!nzchar(text)) return(list())
  out <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE),
                  error = function(e) NULL)
  out %||% list()
}

# Token / code helpers ---------------------------------------------------

# Mint an RS256 JWT for the given claims. `extra` overrides defaults.
oauth_as_mint_jwt <- function(cfg, claims, ttl, kind = "access") {
  now <- as.integer(Sys.time())
  claim <- jose::jwt_claim(
    iss = cfg$issuer,
    aud = cfg$audience,
    sub = claims$subject %||% cfg$subject,
    iat = now,
    nbf = now,
    exp = now + as.integer(ttl),
    scope = paste(claims$scopes %||% character(0L), collapse = " "),
    jti = new_uuid(),
    token_type = kind)
  jose::jwt_encode_sig(claim, key = cfg$signing_key,
    header = list(typ = "JWT", alg = "RS256", kid = cfg$kid))
}

# /authorize handler (PKCE-S256, auto-consent 302) -----------------------

oauth_as_authorize_handler <- function(cfg) {
  function(req) {
    q <- parse_query_string(uri_query_part(req$uri))
    err <- function(code, desc, status = 400L) {
      body <- jsonlite::toJSON(list(error = code,
                                    error_description = desc),
                               auto_unbox = TRUE)
      http_make_response(status, body = body, json = TRUE)
    }
    if (!identical(unname(q["response_type"]), "code")) {
      return(err("unsupported_response_type",
                 "response_type must be 'code'"))
    }
    if (!identical(unname(q["code_challenge_method"]), "S256")) {
      return(err("invalid_request",
                 "code_challenge_method must be S256"))
    }
    cc <- unname(q["code_challenge"])
    if (is.na(cc) || !nzchar(cc)) {
      return(err("invalid_request", "code_challenge is required"))
    }
    client_id <- unname(q["client_id"])
    if (is.na(client_id) || !nzchar(client_id)) {
      return(err("invalid_request", "client_id is required"))
    }
    client <- cfg$client_store$get(client_id)
    if (is.null(client)) {
      return(err("invalid_client", "unknown client_id", 401L))
    }
    redirect_uri <- unname(q["redirect_uri"])
    if (is.na(redirect_uri) || !nzchar(redirect_uri)) {
      return(err("invalid_request", "redirect_uri is required"))
    }
    if (!redirect_uri %in% client$redirect_uris) {
      return(err("invalid_request",
                 "redirect_uri not registered for this client"))
    }
    scope <- unname(q["scope"])
    scope_vec <- if (!is.na(scope) && nzchar(scope)) {
      strsplit(scope, "\\s+")[[1L]]
    } else character(0L)
    state <- unname(q["state"])

    if (!isTRUE(cfg$auto_consent)) {
      # Hand control to user-supplied consent form when present.
      if (is.function(cfg$consent_html_fn)) {
        html <- cfg$consent_html_fn(req, cfg)
        return(http_make_response(200L, body = html,
          headers = c("Content-Type" = "text/html; charset=utf-8")))
      }
      # Minimal built-in form for manual testing.
      html <- sprintf(paste(
        "<!doctype html><meta charset='utf-8'>",
        "<title>Authorize</title><h1>Authorize %s?</h1>",
        "<form method='get' action='/authorize'>",
        "<input type='hidden' name='response_type' value='code'>",
        "<input type='hidden' name='client_id' value='%s'>",
        "<input type='hidden' name='redirect_uri' value='%s'>",
        "<input type='hidden' name='code_challenge' value='%s'>",
        "<input type='hidden' name='code_challenge_method' value='S256'>",
        "<input type='hidden' name='state' value='%s'>",
        "<input type='hidden' name='scope' value='%s'>",
        "<input type='hidden' name='_consented' value='1'>",
        "<button type='submit'>Allow</button>",
        "</form>",
        sep = ""),
        htmlEscape(client$client_name %||% client_id),
        htmlEscape(client_id),
        htmlEscape(redirect_uri),
        htmlEscape(cc),
        htmlEscape(state %||% ""),
        htmlEscape(paste(scope_vec, collapse = " ")))
      # Re-submission of the form (with _consented=1) proceeds to
      # code minting below; first GET shows the page.
      if (!identical(unname(q["_consented"]), "1")) {
        return(http_make_response(200L, body = html,
          headers = c("Content-Type" = "text/html; charset=utf-8")))
      }
    }
    # Mint a one-shot authorization code.
    code <- new_uuid()
    cfg$code_store$add(code, list(
      client_id = client_id,
      redirect_uri = redirect_uri,
      code_challenge = cc,
      scope = scope_vec,
      subject = cfg$subject,
      expires = as.numeric(Sys.time()) + cfg$ttl_code))
    sep <- if (grepl("?", redirect_uri, fixed = TRUE)) "&" else "?"
    state_q <- if (!is.na(state) && nzchar(state)) {
      sprintf("&state=%s", utils::URLencode(state, reserved = TRUE))
    } else ""
    location <- paste0(redirect_uri, sep, "code=",
                       utils::URLencode(code, reserved = TRUE),
                       state_q)
    http_make_response(302L,
      headers = c("Location" = location,
                  "Cache-Control" = "no-store",
                  "Content-Type" = "text/plain"),
      body = "redirecting")
  }
}

htmlEscape <- function(s) {
  s <- as.character(s %||% "")
  s <- gsub("&", "&amp;",  s, fixed = TRUE)
  s <- gsub("<", "&lt;",   s, fixed = TRUE)
  s <- gsub(">", "&gt;",   s, fixed = TRUE)
  s <- gsub('"', "&quot;", s, fixed = TRUE)
  s
}

# /token handler ---------------------------------------------------------

oauth_as_token_handler <- function(cfg) {
  function(req) {
    p <- req_form_params(req)
    err <- function(code, desc = NULL, status = 400L) {
      body <- jsonlite::toJSON(drop_nulls(list(
        error = code, error_description = desc)),
        auto_unbox = TRUE)
      http_make_response(status, body = body, json = TRUE)
    }
    gt <- unname(p["grant_type"])
    if (is.na(gt)) return(err("invalid_request", "grant_type missing"))
    if (identical(gt, "authorization_code")) {
      code <- unname(p["code"])
      verifier <- unname(p["code_verifier"])
      client_id <- unname(p["client_id"])
      redirect_uri <- unname(p["redirect_uri"])
      if (is.na(code) || is.na(verifier) ||
          is.na(client_id) || is.na(redirect_uri)) {
        return(err("invalid_request",
                   "missing code, code_verifier, client_id, or redirect_uri"))
      }
      stored <- cfg$code_store$get(code)
      if (is.null(stored)) {
        return(err("invalid_grant", "unknown or expired code"))
      }
      # One-shot use — revoke immediately to defeat replay.
      cfg$code_store$revoke(code)
      if (Sys.time() > stored$expires) {
        return(err("invalid_grant", "code expired"))
      }
      if (!identical(stored$client_id, client_id) ||
          !identical(stored$redirect_uri, redirect_uri)) {
        return(err("invalid_grant",
                   "client_id or redirect_uri does not match"))
      }
      # Verify PKCE: BASE64URL(SHA256(verifier)) must equal stored
      # code_challenge.
      computed <- jose::base64url_encode(
        openssl::sha256(charToRaw(verifier)))
      computed <- sub("=+$", "", computed)
      if (!identical(computed, stored$code_challenge)) {
        return(err("invalid_grant", "PKCE verifier mismatch"))
      }
      access <- oauth_as_mint_jwt(cfg,
        list(subject = stored$subject, scopes = stored$scope),
        cfg$ttl_access, kind = "access")
      refresh <- oauth_as_mint_jwt(cfg,
        list(subject = stored$subject, scopes = stored$scope),
        cfg$ttl_refresh, kind = "refresh")
      cfg$token_store$add(refresh, list(
        kind = "refresh",
        subject = stored$subject,
        scope = stored$scope,
        client_id = client_id,
        expires = as.numeric(Sys.time()) + cfg$ttl_refresh))
      body <- jsonlite::toJSON(list(
        access_token = access,
        token_type = "Bearer",
        expires_in = cfg$ttl_access,
        refresh_token = refresh,
        scope = paste(stored$scope, collapse = " ")),
        auto_unbox = TRUE)
      return(http_make_response(200L, body = body, json = TRUE))
    }
    if (identical(gt, "refresh_token")) {
      tok <- unname(p["refresh_token"])
      if (is.na(tok)) {
        return(err("invalid_request", "refresh_token missing"))
      }
      stored <- cfg$token_store$get(tok)
      if (is.null(stored) ||
          !identical(stored$kind, "refresh") ||
          Sys.time() > stored$expires) {
        return(err("invalid_grant",
                   "unknown, revoked, or expired refresh_token"))
      }
      access <- oauth_as_mint_jwt(cfg,
        list(subject = stored$subject, scopes = stored$scope),
        cfg$ttl_access, kind = "access")
      body <- jsonlite::toJSON(list(
        access_token = access,
        token_type = "Bearer",
        expires_in = cfg$ttl_access,
        scope = paste(stored$scope, collapse = " ")),
        auto_unbox = TRUE)
      return(http_make_response(200L, body = body, json = TRUE))
    }
    err("unsupported_grant_type",
        sprintf("grant_type '%s' not supported", gt))
  }
}

# /register handler (RFC 7591 Dynamic Client Registration) ---------------

oauth_as_register_handler <- function(cfg) {
  function(req) {
    body <- req_json_body(req)
    err <- function(code, desc = NULL, status = 400L) {
      payload <- jsonlite::toJSON(drop_nulls(list(
        error = code, error_description = desc)),
        auto_unbox = TRUE)
      http_make_response(status, body = payload, json = TRUE)
    }
    redirect_uris <- as.character(unlist(body$redirect_uris %||% list()))
    if (length(redirect_uris) == 0L) {
      return(err("invalid_redirect_uri",
                 "redirect_uris is required"))
    }
    client_id <- new_uuid()
    client_name <- as.character(body$client_name %||% client_id)
    issued_at <- as.integer(Sys.time())
    record <- list(
      client_id = client_id,
      client_name = client_name,
      redirect_uris = redirect_uris,
      grant_types = as.character(unlist(
        body$grant_types %||% c("authorization_code", "refresh_token"))),
      response_types = as.character(unlist(
        body$response_types %||% c("code"))),
      token_endpoint_auth_method = "none",
      client_id_issued_at = issued_at)
    cfg$client_store$add(client_id, record)
    payload <- jsonlite::toJSON(record, auto_unbox = TRUE,
                                force = TRUE)
    http_make_response(201L, body = payload, json = TRUE)
  }
}

# Helper: derive a resource-server `oauth_config()` from an AS so a
# single process can both issue and verify its own tokens. Embeds the
# AS's public JWK directly in `cfg$jwks_cache` to avoid a network
# round-trip back to ourselves.
oauth_config_from_server <- function(as_cfg,
                                     required_scopes = character(0L)) {
  cfg <- oauth_config(
    issuer = as_cfg$issuer,
    audience = as_cfg$audience,
    jwks_uri = paste0(as_cfg$issuer, "/jwks"),
    required_scopes = required_scopes)
  oauth_set_jwks(cfg, oauth_as_jwks_json(as_cfg))
  cfg
}
