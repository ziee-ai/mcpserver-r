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
#' @param signing_key An `openssl::rsa_keygen()` result. When `NULL`
#'   (default) the key is loaded from / persisted to `key_path` so it is
#'   stable across restarts. An explicit value takes precedence over
#'   `key_path`.
#' @param key_path Path to a PEM file holding the RS256 signing key. When
#'   `signing_key` is `NULL`, the key is read from this file if it exists,
#'   otherwise a fresh 2048-bit key is generated and written there (mode
#'   `0600`). Defaults to `.mcpserver-as-key.pem` in the current working
#'   directory, so each server launched from its own folder keeps its own
#'   key. **Persisting the key is required for previously-minted tokens to
#'   keep validating after a restart** — without it every restart rotates
#'   the key and silently invalidates all outstanding tokens. Two servers
#'   started from the *same* working directory would share this file; give
#'   each an explicit `key_path` (or distinct working dirs) to avoid that.
#'   Add the PEM to `.gitignore`. Note the token store must also be
#'   persistent (`new_mcp_store("sqlite", ...)`) for token rows to survive.
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
#' @param store Optional [new_mcp_store()] result. When supplied, the
#'   AS gains a per-user identity model: tokens minted via
#'   [oauth_mint_user_token()] carry the user's id in the `sub` claim
#'   plus a fresh `jti` whose row in `store$tokens` is checked on
#'   every request. Leaving this `NULL` preserves the legacy
#'   anonymous-subject behavior used by the `/authorize` + `/token`
#'   demo flow.
#' @return A list of class `"mcp_oauth_server_config"` with helper
#'   accessors. Pass to [serve_http()] via the `oauth_as` argument.
#' @export
oauth_server_config <- function(issuer,
                                audience,
                                signing_key   = NULL,
                                key_path      = NULL,
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
                                token_store   = new_oauth_store(),
                                store         = NULL) {
  issuer <- sub("/+$", "", as.character(issuer))
  validate_issuer_url(issuer)
  if (is.null(signing_key)) {
    signing_key <- load_or_create_signing_key(key_path)
  }
  if (!is.null(store) && !inherits(store, "mcp_store")) {
    stop("store must be a new_mcp_store() result", call. = FALSE)
  }
  cfg <- list(
    issuer        = issuer,
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
    token_store   = token_store,
    store         = store
  )
  class(cfg) <- "mcp_oauth_server_config"
  cfg
}

# Resolve a stable RS256 signing key. Load the PEM at `key_path`,
# generating + writing it (mode 0600) on first use so tokens minted before
# a restart still verify afterwards. A NULL/empty `key_path` defaults to a
# dotfile in the current working directory, giving each server launched
# from its own folder a distinct key.
load_or_create_signing_key <- function(key_path = NULL) {
  if (is.null(key_path) || !nzchar(key_path)) {
    key_path <- file.path(getwd(), ".mcpserver-as-key.pem")
  }
  if (file.exists(key_path)) {
    return(openssl::read_key(key_path))
  }
  key <- openssl::rsa_keygen(2048L)
  dir.create(dirname(key_path), recursive = TRUE, showWarnings = FALSE)
  openssl::write_pem(key, path = key_path)
  Sys.chmod(key_path, mode = "0600")
  key
}

#' Mint a user-bound access token
#'
#' Signs an RS256 JWT with `sub = user_id` and a fresh `jti`, and
#' persists the metadata row in `cfg$store$tokens` for instant
#' revocation. Requires `cfg` to have been built with a `store` arg.
#'
#' @param cfg An [oauth_server_config()] result with a `store` set.
#' @param user_id Id of an existing user in `cfg$store$users`.
#' @param scopes Character vector of scopes to embed in the token.
#' @param ttl Lifetime in seconds (defaults to `cfg$ttl_access`).
#' @param name Human label stored alongside the token row (e.g.
#'   `"ci-runner"`). Must be unique per user.
#' @return A list with `jti`, `token` (the JWT string), and
#'   `expires_at` (ISO 8601, UTC). The JWT is returned **once**; only
#'   metadata is persisted server-side.
#' @export
oauth_mint_user_token <- function(cfg, user_id, scopes,
                                  ttl = NULL, name = "token") {
  stopifnot(inherits(cfg, "mcp_oauth_server_config"))
  if (is.null(cfg$store)) {
    stop("oauth_server_config(store=) is required to mint user tokens",
         call. = FALSE)
  }
  user <- cfg$store$users$get(user_id)
  if (is.null(user)) {
    stop(sprintf("no such user: %s", user_id), call. = FALSE)
  }
  ttl <- if (is.null(ttl)) cfg$ttl_access else as.integer(ttl)
  if (ttl <= 0L) stop("ttl must be positive", call. = FALSE)
  now <- as.integer(Sys.time())
  jti <- new_uuid()
  scope_str <- paste(as.character(scopes), collapse = " ")
  claim <- jose::jwt_claim(
    iss   = cfg$issuer,
    aud   = cfg$audience,
    sub   = user_id,
    exp   = as.integer(now + ttl),
    nbf   = as.integer(now),
    iat   = as.integer(now),
    jti   = jti,
    scope = scope_str
  )
  jwt <- jose::jwt_encode_sig(claim, key = cfg$signing_key,
                              header = list(kid = cfg$kid))
  expires_at <- format(as.POSIXct(now + ttl, origin = "1970-01-01",
                                  tz = "UTC"),
                       "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  cfg$store$tokens$add(list(
    jti        = jti,
    user_id    = user_id,
    name       = name,
    scopes     = as.character(scopes),
    expires_at = expires_at
  ))
  list(jti = jti, token = jwt, expires_at = expires_at)
}

# Validate an AS issuer URL per RFC 8414 §3:
#   - Must be HTTPS (or HTTP with a loopback host: localhost / 127.0.0.1 /
#     [::1] — useful for local development per RFC 8252).
#   - MUST NOT carry a fragment or query string.
validate_issuer_url <- function(issuer) {
  if (!grepl("^https?://", issuer, ignore.case = TRUE)) {
    stop(sprintf("issuer must be an http(s) URL, got: %s", issuer))
  }
  if (grepl("#", issuer, fixed = TRUE)) {
    stop(sprintf("issuer must not contain a fragment: %s", issuer))
  }
  if (grepl("\\?", issuer, perl = TRUE)) {
    stop(sprintf("issuer must not contain a query string: %s", issuer))
  }
  if (grepl("^http://", issuer, ignore.case = TRUE)) {
    # Strip scheme + path so we can inspect the host portion.
    rest <- sub("^http://", "", issuer, ignore.case = TRUE)
    host <- sub("[/].*$", "", rest)
    host <- sub(":[0-9]+$", "", host)
    if (!(tolower(host) %in% c("localhost", "127.0.0.1", "[::1]"))) {
      stop(sprintf(
        "issuer must use HTTPS for non-loopback hosts, got: %s", issuer))
    }
  }
  invisible(NULL)
}

# RFC 8252 §7.3 — for native apps, redirect_uri matching relaxes the
# port for loopback addresses (localhost, 127.0.0.1, [::1]) so a CLI
# that listens on an OS-chosen ephemeral port can still match its
# port-less registration.
oauth_redirect_uri_matches <- function(registered, candidate) {
  if (identical(registered, candidate)) return(TRUE)
  reg <- parse_redirect_uri(registered)
  cand <- parse_redirect_uri(candidate)
  if (is.null(reg) || is.null(cand)) return(FALSE)
  if (!identical(reg$scheme, cand$scheme)) return(FALSE)
  if (!identical(reg$host, cand$host)) return(FALSE)
  if (!identical(reg$path, cand$path)) return(FALSE)
  loopback_hosts <- c("localhost", "127.0.0.1", "[::1]")
  if (!(tolower(reg$host) %in% loopback_hosts)) return(FALSE)
  # Loopback: ports may differ.
  TRUE
}

# Minimal scheme/host/port/path split — RFC 3986 enough for the
# loopback comparison we need.
parse_redirect_uri <- function(uri) {
  m <- regmatches(uri, regexec(
    "^([a-zA-Z][a-zA-Z0-9+.-]*)://(\\[[^]]+\\]|[^:/?#]+)(?::([0-9]+))?(/[^?#]*)?",
    uri))[[1L]]
  if (length(m) < 5L) return(NULL)
  list(scheme = tolower(m[[2L]]),
       host   = m[[3L]],
       port   = if (nzchar(m[[4L]])) m[[4L]] else NA_character_,
       path   = if (nzchar(m[[5L]])) m[[5L]] else "/")
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
    oauth_with_cors(oauth_as_metadata_impl(cfg))
  }
}

oauth_as_metadata_impl <- function(cfg) {
  body <- jsonlite::toJSON(list(
      issuer = cfg$issuer,
      authorization_endpoint = paste0(cfg$issuer, "/authorize"),
      token_endpoint = paste0(cfg$issuer, "/token"),
      registration_endpoint = paste0(cfg$issuer, "/register"),
      revocation_endpoint = paste0(cfg$issuer, "/revoke"),
      jwks_uri = paste0(cfg$issuer, "/jwks"),
      scopes_supported = I(cfg$scopes_supported),
      response_types_supported = I("code"),
      grant_types_supported = I(c("authorization_code",
                                  "refresh_token")),
      code_challenge_methods_supported = I("S256"),
      token_endpoint_auth_methods_supported = I(
        c("none", "client_secret_basic", "client_secret_post")),
      revocation_endpoint_auth_methods_supported = I(
        c("none", "client_secret_basic", "client_secret_post"))
    ), auto_unbox = TRUE, force = TRUE)
  http_make_response(200L, body = body, json = TRUE)
}

# JWKS handler -----------------------------------------------------------

oauth_as_jwks_handler <- function(cfg) {
  function(req) {
    oauth_with_cors(http_make_response(200L,
                                       body = oauth_as_jwks_json(cfg),
                                       json = TRUE))
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

# Client authentication --------------------------------------------------

# Generate a random URL-safe secret for confidential clients.
oauth_generate_secret <- function() {
  raw <- openssl::rand_bytes(32L)
  hex <- paste(format(raw), collapse = "")
  hex
}

# Hash a client_secret with a per-client salt. SHA-256 + 32-byte
# random salt is appropriate for the demo-grade in-memory AS we
# ship (the AS is documented as "not for production").
oauth_hash_secret <- function(secret, salt = NULL) {
  if (is.null(salt)) {
    salt <- paste(format(openssl::rand_bytes(16L)), collapse = "")
  }
  combined <- charToRaw(paste0(salt, ":", secret))
  digest <- paste(format(openssl::sha256(combined)), collapse = "")
  list(salt = salt, digest = digest)
}

oauth_verify_secret <- function(secret, hashed) {
  if (is.null(hashed) || is.null(hashed$salt) || is.null(hashed$digest)) {
    return(FALSE)
  }
  candidate <- oauth_hash_secret(secret, salt = hashed$salt)
  identical(candidate$digest, hashed$digest)
}

# Authenticate the requesting OAuth client on /token, /revoke, etc.
# Supports:
#   - Authorization: Basic <b64 client_id:client_secret> (client_secret_basic)
#   - body params  client_id + client_secret              (client_secret_post)
#   - public clients (token_endpoint_auth_method = "none")  — only client_id required
# Returns list(ok = TRUE, client = <record>) or list(ok = FALSE, response = <http response>).
oauth_authenticate_client <- function(cfg, req, params) {
  # Try Authorization: Basic first.
  authz <- header_get(req$headers, "Authorization")
  if (!is.null(authz) &&
      grepl("^Basic\\s+", authz, ignore.case = TRUE)) {
    b64 <- sub("^Basic\\s+", "", authz, ignore.case = TRUE)
    decoded <- tryCatch(
      rawToChar(jsonlite::base64_dec(b64)),
      error = function(e) NULL)
    if (is.null(decoded) || !grepl(":", decoded, fixed = TRUE)) {
      return(oauth_client_auth_failure("invalid Authorization header"))
    }
    cid <- sub(":.*$", "", decoded)
    csec <- sub("^[^:]*:", "", decoded)
    client <- cfg$client_store$get(cid)
    if (is.null(client)) {
      return(oauth_client_auth_failure("unknown client_id"))
    }
    if (!identical(client$token_endpoint_auth_method,
                   "client_secret_basic")) {
      return(oauth_client_auth_failure(
        "client did not register client_secret_basic"))
    }
    if (!oauth_verify_secret(csec, client$client_secret_hash)) {
      return(oauth_client_auth_failure("invalid client_secret"))
    }
    return(list(ok = TRUE, client = client))
  }
  cid <- unname(params["client_id"])
  if (is.na(cid) || !nzchar(cid)) {
    return(oauth_client_auth_failure("client_id is required"))
  }
  client <- cfg$client_store$get(cid)
  if (is.null(client)) {
    return(oauth_client_auth_failure("unknown client_id"))
  }
  method <- client$token_endpoint_auth_method %||% "none"
  if (identical(method, "none")) {
    return(list(ok = TRUE, client = client))
  }
  if (identical(method, "client_secret_post")) {
    csec <- unname(params["client_secret"])
    if (is.na(csec) || !nzchar(csec)) {
      return(oauth_client_auth_failure(
        "client_secret_post requires client_secret in the request body"))
    }
    if (!oauth_verify_secret(csec, client$client_secret_hash)) {
      return(oauth_client_auth_failure("invalid client_secret"))
    }
    return(list(ok = TRUE, client = client))
  }
  # method is client_secret_basic but no Authorization header — RFC
  # 6749 §2.3.1 lets servers also accept POST body params for
  # client_secret_basic clients. We accept that gracefully.
  csec <- unname(params["client_secret"])
  if (!is.na(csec) && nzchar(csec) &&
      oauth_verify_secret(csec, client$client_secret_hash)) {
    return(list(ok = TRUE, client = client))
  }
  oauth_client_auth_failure(
    "client must authenticate via Authorization: Basic header")
}

oauth_client_auth_failure <- function(desc) {
  body <- jsonlite::toJSON(list(
    error = "invalid_client",
    error_description = desc), auto_unbox = TRUE)
  list(ok = FALSE,
       response = http_make_response(401L, body = body, json = TRUE))
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
    oauth_with_cors(oauth_as_authorize_impl(cfg, req))
  }
}

oauth_as_authorize_impl <- function(cfg, req) {
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
    matched <- any(vapply(client$redirect_uris,
      function(r) oauth_redirect_uri_matches(r, redirect_uri),
      logical(1L)))
    if (!matched) {
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

# Attach permissive CORS headers to an OAuth-AS response. Spec-aligned
# allow-list: GET/POST + standard auth headers + 24-hour preflight
# cache. Real deployments should narrow Allow-Origin; the demo-grade
# AS defaults to "*" to keep browser SPAs working.
oauth_with_cors <- function(resp) {
  cors <- c(
    "Access-Control-Allow-Origin" = "*",
    "Access-Control-Allow-Methods" = "GET, POST, OPTIONS",
    "Access-Control-Allow-Headers" = paste("Content-Type",
                                            "Authorization",
                                            sep = ", "),
    "Access-Control-Max-Age" = "86400")
  resp$headers <- c(resp$headers %||% character(0L), cors)
  resp
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
    oauth_with_cors(oauth_as_token_impl(cfg, req))
  }
}

oauth_as_token_impl <- function(cfg, req) {
  p <- req_form_params(req)
    err <- function(code, desc = NULL, status = 400L) {
      body <- jsonlite::toJSON(drop_nulls(list(
        error = code, error_description = desc)),
        auto_unbox = TRUE)
      http_make_response(status, body = body, json = TRUE)
    }
    gt <- unname(p["grant_type"])
    if (is.na(gt)) return(err("invalid_request", "grant_type missing"))
    if (!(gt %in% c("authorization_code", "refresh_token"))) {
      return(err("unsupported_grant_type",
                 sprintf("grant_type '%s' not supported", gt)))
    }
    auth <- oauth_authenticate_client(cfg, req, p)
    if (!isTRUE(auth$ok)) return(auth$response)
    if (identical(gt, "authorization_code")) {
      code <- unname(p["code"])
      verifier <- unname(p["code_verifier"])
      client_id <- auth$client$client_id
      redirect_uri <- unname(p["redirect_uri"])
      if (is.na(code) || is.na(verifier) ||
          is.na(redirect_uri)) {
        return(err("invalid_request",
                   "missing code, code_verifier, or redirect_uri"))
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
      # RFC 6749 §6: requested `scope` parameter on refresh MUST be a
      # subset of the scope originally granted to the refresh token.
      # When omitted, the access token is reissued with the original
      # scope.
      requested_scope_str <- unname(p["scope"])
      issued_scope <- stored$scope
      if (!is.na(requested_scope_str) && nzchar(requested_scope_str)) {
        requested <- strsplit(requested_scope_str, "\\s+")[[1L]]
        requested <- requested[nzchar(requested)]
        extra <- setdiff(requested, stored$scope)
        if (length(extra) > 0L) {
          return(err("invalid_scope",
                     sprintf("requested scope exceeds granted: %s",
                             paste(extra, collapse = " "))))
        }
        issued_scope <- requested
      }
      access <- oauth_as_mint_jwt(cfg,
        list(subject = stored$subject, scopes = issued_scope),
        cfg$ttl_access, kind = "access")
      body <- jsonlite::toJSON(list(
        access_token = access,
        token_type = "Bearer",
        expires_in = cfg$ttl_access,
        scope = paste(issued_scope, collapse = " ")),
        auto_unbox = TRUE)
      return(http_make_response(200L, body = body, json = TRUE))
    }
  # Unreachable — gt was validated above.
  err("unsupported_grant_type",
      sprintf("grant_type '%s' not supported", gt))
}

# /register handler (RFC 7591 Dynamic Client Registration) ---------------

oauth_as_register_handler <- function(cfg) {
  function(req) {
    oauth_with_cors(oauth_as_register_impl(cfg, req))
  }
}

oauth_as_register_impl <- function(cfg, req) {
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
    method <- as.character(body$token_endpoint_auth_method %||% "none")
    if (!(method %in% c("none", "client_secret_basic",
                        "client_secret_post"))) {
      return(err("invalid_client_metadata",
                 sprintf("unsupported token_endpoint_auth_method '%s'",
                         method)))
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
      token_endpoint_auth_method = method,
      client_id_issued_at = issued_at)
    # Mint a client_secret for confidential clients. Store only the
    # hash; surface the cleartext once in the registration response.
    response_payload <- record
    if (!identical(method, "none")) {
      secret <- oauth_generate_secret()
      record$client_secret_hash <- oauth_hash_secret(secret)
      response_payload$client_secret <- secret
    }
  cfg$client_store$add(client_id, record)
  payload <- jsonlite::toJSON(response_payload, auto_unbox = TRUE,
                              force = TRUE)
  http_make_response(201L, body = payload, json = TRUE)
}

# /revoke handler (RFC 7009) --------------------------------------------

# Revokes the supplied token if it exists. Per RFC 7009 §2.2, the
# response is 200 OK whether or not the token was known — clients
# must not be able to probe for token existence. The client is
# authenticated the same way as on /token.
oauth_as_revoke_handler <- function(cfg) {
  function(req) {
    oauth_with_cors(oauth_as_revoke_impl(cfg, req))
  }
}

oauth_as_revoke_impl <- function(cfg, req) {
  p <- req_form_params(req)
  err <- function(code, desc = NULL, status = 400L) {
    body <- jsonlite::toJSON(drop_nulls(list(
      error = code, error_description = desc)),
      auto_unbox = TRUE)
    http_make_response(status, body = body, json = TRUE)
  }
  token <- unname(p["token"])
  if (is.na(token) || !nzchar(token)) {
    return(err("invalid_request", "token parameter is required"))
  }
  auth <- oauth_authenticate_client(cfg, req, p)
  if (!isTRUE(auth$ok)) return(auth$response)
  # token_type_hint is advisory — we just look up by id either way.
  cfg$token_store$revoke(token)
  http_make_response(200L, body = "", json = TRUE)
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
