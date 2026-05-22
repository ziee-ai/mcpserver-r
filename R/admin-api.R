# Admin REST API ---------------------------------------------------------

# Surface mounted under /admin/ when `serve_http(admin = ...)` is set.
# All endpoints require admin authentication (`require_admin()`), which
# accepts either the bootstrap admin token (env-configured opaque string)
# or a JWT whose `sub` resolves to a `is_admin = TRUE` user.
#
# Routes (handled by `admin_router()` via nanonext prefix matching):
#
#   GET    /admin/healthz
#   GET    /admin/users
#   POST   /admin/users
#   GET    /admin/users/{id}
#   PATCH  /admin/users/{id}
#   DELETE /admin/users/{id}
#   GET    /admin/users/{id}/tokens
#   POST   /admin/tokens/mint
#   POST   /admin/tokens/{jti}/revoke
#
# Responses are JSON; errors carry `{error, message}` payloads with the
# semantics documented per-endpoint.

# Bare JSON response. We do NOT layer a JSON-RPC envelope here — these
# are plain REST endpoints, distinct from the /mcp transport.
admin_json <- function(status, obj = NULL, headers = NULL) {
  body <- if (is.null(obj)) NULL else to_json(obj)
  hd <- c(headers %||% character(0L),
          c("Content-Type" = "application/json"))
  http_make_response(status, body = body, headers = hd)
}

admin_error <- function(status, error, message,
                        extra_headers = NULL) {
  admin_json(status,
             list(error = error, message = message),
             headers = extra_headers)
}

# Extract a parsed JSON body from req. Returns NULL when body is empty
# or malformed; callers should treat NULL as a 400.
admin_parse_body <- function(req) {
  if (is.null(req$body) || length(req$body) == 0L) return(NULL)
  from_json(req$body)
}

# Resolve the admin principal from the Authorization header.
# Returns one of:
#   list(ok = TRUE, kind = "bootstrap")
#   list(ok = TRUE, kind = "user", user_id = ..., user = ...)
#   list(ok = FALSE, status = 401L | 403L, reason = "...")
require_admin <- function(state, req) {
  cfg <- state$admin
  if (is.null(cfg)) {
    # serve_http(admin = NULL) -> the admin handlers shouldn't be
    # registered. Defensive check.
    return(list(ok = FALSE, status = 404L, reason = "admin not enabled"))
  }
  authz <- header_get(req$headers, "Authorization")
  if (is.null(authz) || !nzchar(authz)) {
    return(list(ok = FALSE, status = 401L, reason = "missing"))
  }
  token <- oauth_extract_bearer(authz)
  if (is.null(token)) {
    return(list(ok = FALSE, status = 401L, reason = "malformed"))
  }
  # 1) bootstrap admin token: opaque, env-configured. Constant-time
  #    compare so unequal lengths don't leak via timing.
  if (!is.null(cfg$bootstrap_token) && nzchar(cfg$bootstrap_token)) {
    if (constant_time_equal(token, cfg$bootstrap_token)) {
      return(list(ok = TRUE, kind = "bootstrap"))
    }
  }
  # 2) admin-user JWT: verified by the same resource-server config used
  #    for /mcp, plus the user lookup to confirm is_admin.
  if (is.null(state$auth)) {
    # No verifier configured => no JWT path available.
    return(list(ok = FALSE, status = 401L, reason = "no jwt verifier"))
  }
  res <- oauth_verify_bearer(state$auth, authz)
  if (!isTRUE(res$ok)) {
    return(list(ok = FALSE, status = 401L, reason = "invalid jwt"))
  }
  store <- cfg$store
  if (is.null(store) || is.null(store$users)) {
    return(list(ok = FALSE, status = 401L, reason = "no user store"))
  }
  user <- store$users$get(res$subject)
  if (is.null(user)) {
    return(list(ok = FALSE, status = 401L, reason = "unknown subject"))
  }
  if (!isTRUE(user$is_admin)) {
    return(list(ok = FALSE, status = 403L, reason = "not admin"))
  }
  list(ok = TRUE, kind = "user", user_id = user$id, user = user)
}

constant_time_equal <- function(a, b) {
  a <- as.character(a); b <- as.character(b)
  if (nchar(a) != nchar(b)) return(FALSE)
  identical(openssl::sha256(charToRaw(a)),
            openssl::sha256(charToRaw(b)))
}

# Reject a request that failed require_admin().
admin_reject <- function(rej) {
  status <- rej$status %||% 401L
  if (status == 403L) {
    admin_error(403L, "forbidden", rej$reason %||% "forbidden")
  } else {
    admin_error(status, "unauthorized",
                rej$reason %||% "unauthorized")
  }
}

# Public-facing user record. Strip the internal id <-> username index
# bits and JSON-decode the embedded fields so JSON serialization
# matches what the SPA / curl expects.
user_view <- function(u) {
  if (is.null(u)) return(NULL)
  groups <- if (is.null(u$groups)) character(0L)
            else as.character(unlist(u$groups))
  list(
    id         = u$id,
    username   = u$username,
    email      = u$email,
    is_admin   = isTRUE(u$is_admin),
    # I() forces a JSON array even for length-1 / empty groups, matching
    # the SPA's `string[]` type.
    groups     = I(groups),
    metadata   = if (is.null(u$metadata)) j_empty_obj() else u$metadata,
    created_at = u$created_at,
    updated_at = u$updated_at
  )
}

token_view <- function(t) {
  if (is.null(t)) return(NULL)
  list(
    jti          = t$jti,
    user_id      = t$user_id,
    name         = t$name,
    # Wrap with I() so jsonlite emits an array even for length-1 scopes
    # (we serialize with auto_unbox = TRUE elsewhere).
    scopes       = I(as.character(t$scopes)),
    created_at   = t$created_at,
    expires_at   = t$expires_at,
    last_used_at = t$last_used_at,
    revoked      = isTRUE(t$revoked)
  )
}

# Route dispatch. Returns a response or NULL if the path doesn't match.
admin_route <- function(state, req, principal) {
  cfg   <- state$admin
  store <- cfg$store
  uri   <- req$uri
  # Strip a leading admin/api prefix and any trailing slash before
  # matching against the route table.
  path  <- sub("\\?.*$", "", uri)            # drop query string
  path  <- sub("/+$", "", path)              # trailing slash
  method <- toupper(req$method %||% "GET")

  if (identical(path, "/admin/healthz")) {
    if (method != "GET") return(method_not_allowed(c("GET")))
    return(admin_json(200L, list(status = "ok")))
  }

  if (identical(path, "/admin/users")) {
    if (method == "GET")  return(handle_users_list(store, req))
    if (method == "POST") return(handle_users_create(store, req,
                                                    principal))
    return(method_not_allowed(c("GET", "POST")))
  }

  m <- regmatches(path,
                  regexec("^/admin/users/([^/]+)$", path))[[1L]]
  if (length(m) == 2L) {
    id <- m[[2L]]
    if (method == "GET")    return(handle_users_get(store, id))
    if (method == "PATCH")  return(handle_users_patch(store, req, id,
                                                      principal))
    if (method == "DELETE") return(handle_users_delete(store, id))
    return(method_not_allowed(c("GET", "PATCH", "DELETE")))
  }

  m <- regmatches(path,
                  regexec("^/admin/users/([^/]+)/tokens$",
                          path))[[1L]]
  if (length(m) == 2L) {
    id <- m[[2L]]
    if (method == "GET") return(handle_user_tokens_list(store, req, id))
    return(method_not_allowed(c("GET")))
  }

  if (identical(path, "/admin/tokens/mint")) {
    if (method != "POST") return(method_not_allowed(c("POST")))
    return(handle_tokens_mint(state, req))
  }

  m <- regmatches(path,
                  regexec("^/admin/tokens/([^/]+)/revoke$",
                          path))[[1L]]
  if (length(m) == 2L) {
    if (method != "POST") return(method_not_allowed(c("POST")))
    return(handle_tokens_revoke(store, m[[2L]]))
  }

  admin_error(404L, "not_found",
              sprintf("no such admin route: %s %s", method, path))
}

method_not_allowed <- function(allow) {
  admin_error(405L, "method_not_allowed",
              sprintf("allowed: %s", paste(allow, collapse = ", ")),
              extra_headers =
                c("Allow" = paste(allow, collapse = ", ")))
}

# ---- /admin/users -------------------------------------------------------

handle_users_list <- function(store, req) {
  qs <- admin_parse_query(req$uri %||% "")
  limit <- NULL
  if (!is.null(qs$limit) && nzchar(qs$limit)) {
    limit <- suppressWarnings(as.integer(qs$limit))
    if (is.na(limit) || limit < 0L) {
      return(admin_error(400L, "bad_request",
                         "limit must be a non-negative integer"))
    }
  }
  cursor <- qs$cursor
  users <- lapply(store$users$list(limit = limit, cursor = cursor),
                  user_view)
  next_cursor <- if (!is.null(limit) && length(users) == limit) {
    users[[length(users)]]$id
  } else {
    NULL
  }
  body <- list(users = users)
  if (!is.null(next_cursor)) body$next_cursor <- next_cursor
  admin_json(200L, body)
}

# Minimal query-string parser. Returns a named list of decoded values.
# Doesn't try to be a full RFC 3986 implementation — just enough for
# our admin endpoints' `?limit=N&cursor=...` pattern.
admin_parse_query <- function(uri) {
  q <- sub("^[^?]*\\?", "", uri)
  if (identical(q, uri)) return(list())  # no '?'
  out <- list()
  for (kv in strsplit(q, "&", fixed = TRUE)[[1L]]) {
    if (!nzchar(kv)) next
    eq <- regmatches(kv, regexec("^([^=]+)=?(.*)$", kv))[[1L]]
    if (length(eq) >= 3L) {
      out[[utils::URLdecode(eq[[2L]])]] <- utils::URLdecode(eq[[3L]])
    }
  }
  out
}

handle_users_create <- function(store, req, principal) {
  body <- admin_parse_body(req)
  if (is.null(body)) {
    return(admin_error(400L, "bad_request",
                       "body must be a JSON object"))
  }
  if (is.null(body$username) || !nzchar(body$username)) {
    return(admin_error(400L, "bad_request",
                       "username is required"))
  }
  # Only bootstrap can flip the is_admin bit on creation.
  if (isTRUE(as.logical(body$is_admin)) &&
      !identical(principal$kind, "bootstrap")) {
    return(admin_error(403L, "forbidden",
                       "only bootstrap admin may set is_admin"))
  }
  user <- list(
    username = body$username,
    email    = body$email,
    is_admin = isTRUE(as.logical(body$is_admin)),
    groups   = body$groups,
    metadata = body$metadata
  )
  out <- tryCatch(store$users$add(user),
                  error = function(e) e)
  if (inherits(out, "error")) {
    msg <- conditionMessage(out)
    if (grepl("already exists", msg)) {
      return(admin_error(409L, "conflict", msg))
    }
    return(admin_error(400L, "bad_request", msg))
  }
  admin_json(201L, user_view(out))
}

handle_users_get <- function(store, id) {
  u <- store$users$get(id)
  if (is.null(u)) {
    return(admin_error(404L, "not_found",
                       sprintf("no such user: %s", id)))
  }
  admin_json(200L, user_view(u))
}

USER_PATCH_FIELDS <- c("username", "email", "groups", "metadata",
                       "is_admin")

handle_users_patch <- function(store, req, id, principal) {
  body <- admin_parse_body(req)
  if (is.null(body)) {
    return(admin_error(400L, "bad_request",
                       "body must be a JSON object"))
  }
  if (is.null(store$users$get(id))) {
    return(admin_error(404L, "not_found",
                       sprintf("no such user: %s", id)))
  }
  unknown <- setdiff(names(body), USER_PATCH_FIELDS)
  if (length(unknown) > 0L) {
    return(admin_error(400L, "bad_request",
                       sprintf("unknown field(s): %s",
                               paste(unknown, collapse = ", "))))
  }
  if (!is.null(body$is_admin) &&
      !identical(principal$kind, "bootstrap")) {
    return(admin_error(403L, "forbidden",
                       "only bootstrap admin may change is_admin"))
  }
  changes <- list()
  for (k in c("username", "email", "groups", "metadata")) {
    if (!is.null(body[[k]])) changes[[k]] <- body[[k]]
  }
  if (!is.null(body$is_admin)) {
    changes$is_admin <- isTRUE(as.logical(body$is_admin))
  }
  out <- tryCatch(store$users$update(id, changes),
                  error = function(e) e)
  if (inherits(out, "error")) {
    msg <- conditionMessage(out)
    if (grepl("already exists", msg)) {
      return(admin_error(409L, "conflict", msg))
    }
    return(admin_error(400L, "bad_request", msg))
  }
  admin_json(200L, user_view(out))
}

handle_users_delete <- function(store, id) {
  ok <- store$users$delete(id)
  if (!isTRUE(ok)) {
    return(admin_error(404L, "not_found",
                       sprintf("no such user: %s", id)))
  }
  admin_json(204L, NULL)
}

handle_user_tokens_list <- function(store, req, user_id) {
  if (is.null(store$users$get(user_id))) {
    return(admin_error(404L, "not_found",
                       sprintf("no such user: %s", user_id)))
  }
  include_revoked <- grepl("include_revoked=true",
                           req$uri %||% "", fixed = TRUE)
  toks <- lapply(store$tokens$list_for_user(
                   user_id, include_revoked = include_revoked),
                 token_view)
  admin_json(200L, list(tokens = toks))
}

# ---- /admin/tokens ------------------------------------------------------

handle_tokens_mint <- function(state, req) {
  body <- admin_parse_body(req)
  if (is.null(body)) {
    return(admin_error(400L, "bad_request",
                       "body must be a JSON object"))
  }
  if (is.null(body$user_id) || !nzchar(body$user_id)) {
    return(admin_error(400L, "bad_request",
                       "user_id is required"))
  }
  if (is.null(state$oauth_as)) {
    return(admin_error(500L, "misconfigured",
                       "oauth_as is required to mint tokens"))
  }
  ttl <- if (is.null(body$ttl)) state$oauth_as$ttl_access
         else suppressWarnings(as.integer(body$ttl))
  if (is.na(ttl) || ttl <= 0L) {
    return(admin_error(400L, "bad_request",
                       "ttl must be a positive integer"))
  }
  max_ttl <- state$admin$max_ttl %||% (60L * 60L * 24L * 365L)
  if (ttl > max_ttl) {
    return(admin_error(400L, "bad_request",
                       sprintf("ttl exceeds maximum %d seconds",
                               max_ttl)))
  }
  scopes <- if (is.null(body$scopes)) character(0L)
            else as.character(unlist(body$scopes))
  allowed <- state$oauth_as$scopes_supported %||% character(0L)
  if (length(allowed) > 0L && length(scopes) > 0L &&
      !all(scopes %in% allowed)) {
    return(admin_error(400L, "bad_request",
                       sprintf(
                         "scopes must be a subset of: %s",
                         paste(allowed, collapse = ", "))))
  }
  name <- if (is.null(body$name) || !nzchar(body$name)) "token"
          else as.character(body$name)
  out <- tryCatch(
    oauth_mint_user_token(state$oauth_as,
                          user_id = body$user_id,
                          scopes  = scopes,
                          ttl     = ttl,
                          name    = name),
    error = function(e) e)
  if (inherits(out, "error")) {
    msg <- conditionMessage(out)
    if (grepl("no such user", msg)) {
      return(admin_error(404L, "not_found", msg))
    }
    if (grepl("already in use", msg)) {
      return(admin_error(409L, "conflict", msg))
    }
    return(admin_error(400L, "bad_request", msg))
  }
  admin_json(200L, out)
}

handle_tokens_revoke <- function(store, jti) {
  if (is.null(store$tokens$get(jti))) {
    return(admin_error(404L, "not_found",
                       sprintf("no such token: %s", jti)))
  }
  store$tokens$revoke(jti)
  admin_json(204L, NULL)
}

# Top-level handler factory used by
# `nanonext::handler("/admin", admin_router(state), method = "*",
#                    prefix = TRUE)`. Two surfaces share the prefix:
#
#   1. /admin/ui/*   - static SPA, unauthenticated (the SPA's own API
#                      calls carry the bearer token and are gated by
#                      require_admin separately).
#   2. /admin/*      - JSON REST API, gated by require_admin.
#
# The static SPA is only mounted when `admin$ui = TRUE`.
admin_router <- function(state) {
  function(req) {
    uri <- req$uri %||% ""
    if (isTRUE(state$admin$ui) &&
        (identical(sub("\\?.*$", "", uri), "/admin/ui") ||
         startsWith(sub("\\?.*$", "", uri), "/admin/ui/"))) {
      return(serve_admin_static(state, req))
    }
    rej <- require_admin(state, req)
    # /admin/healthz remains gated; it's used by the SPA to validate
    # the bootstrap token on the Login page.
    if (!isTRUE(rej$ok)) return(admin_reject(rej))
    admin_route(state, req, principal = rej)
  }
}
