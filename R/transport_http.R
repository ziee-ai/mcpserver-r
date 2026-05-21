# Streamable HTTP transport ----------------------------------------------

#' Run an MCP server over Streamable HTTP
#'
#' Implements the MCP 2025-06-18 Streamable HTTP transport on top of
#' `nanonext::http_server()`:
#'
#' * `POST <path>` — clients send JSON-RPC requests/notifications. The
#'   server replies with JSON for requests or `202 Accepted` for
#'   notifications/responses.
#' * `GET <path>` — clients open a long-lived SSE stream that the server
#'   uses for asynchronous notifications and outgoing requests
#'   (sampling, elicitation, roots).
#' * `DELETE <path>` — clients terminate the session.
#'
#' Session lifecycle is keyed by the `Mcp-Session-Id` HTTP header. The
#' `Origin` header is validated against `allowed_origins` to defend
#' against DNS rebinding attacks; missing `Origin` is rejected unless
#' `require_origin = FALSE`. The `MCP-Protocol-Version` header is
#' enforced on all non-`initialize` requests.
#'
#' `Last-Event-ID` on `GET` triggers replay of events appended after that
#' id from a per-session ring buffer capped by `max_event_log`.
#'
#' @param mcp An `McpServer`.
#' @param host Address to bind to. Default `"127.0.0.1"`.
#' @param port TCP port.
#' @param path URL path for the MCP endpoint.
#' @param allowed_origins Character vector of allowed `Origin` prefixes.
#' @param allowed_hosts Optional character vector of allowed values for
#'   the HTTP `Host` header (bare host or `host:port`). When `NULL` (the
#'   default) the Host check is skipped.
#' @param require_origin Whether `Origin` is required (default `TRUE`).
#' @param tls Optional TLS configuration from `nanonext::tls_config()`.
#' @param max_event_log Maximum events retained per session for SSE replay.
#' @param auth Optional auth configuration from [oauth_config()].
#' @param oauth_as Optional authorization-server configuration from
#'   [oauth_server_config()]. When set, the server additionally
#'   exposes `/.well-known/oauth-authorization-server`, `/authorize`,
#'   `/token`, `/register`, and `/jwks`. If `auth` is also `NULL`
#'   a matching resource-server config is derived automatically so a
#'   single process can both issue and verify its own bearer tokens.
#' @param daemons Number of `mirai` daemons.
#' @param stateless When `TRUE`, the server does not allocate or
#'   validate `Mcp-Session-Id` headers and every request is treated as
#'   a fresh self-contained interaction (no SSE GET stream, no
#'   resource subscriptions, no server-to-client requests). Mirrors
#'   the TS SDK's `sessionIdGenerator = undefined` mode.
#' @param enable_json_response When `TRUE` (default), POST responses
#'   are always returned as a single JSON envelope. Reserved for a
#'   future SSE-on-POST mode; today only `TRUE` is supported.
#' @param onsessioninitialized Optional callback `function(session_id)`
#'   invoked when a new session is allocated.
#' @param onsessionclosed Optional callback `function(session_id)`
#'   invoked when a session is torn down (DELETE or eviction).
#' @param block Whether to block in the server's `$serve()` event loop. If
#'   `FALSE`, returns the running `nanoServer` object so the caller can
#'   drive `later::run_now()` themselves; useful for tests.
#' @return Invisibly, the running `nanoServer` object (only meaningful when
#'   `block = FALSE`).
#' @export
serve_http <- function(mcp,
                       host = "127.0.0.1",
                       port = 3000L,
                       path = "/mcp",
                       allowed_origins = c("http://localhost",
                                           "http://127.0.0.1"),
                       allowed_hosts = NULL,
                       require_origin = TRUE,
                       tls = NULL,
                       max_event_log = 1000L,
                       auth = NULL,
                       oauth_as = NULL,
                       daemons = 4L,
                       stateless = FALSE,
                       enable_json_response = TRUE,
                       onsessioninitialized = NULL,
                       onsessionclosed = NULL,
                       block = TRUE) {
  stopifnot(inherits(mcp, "McpServer"))
  ensure_daemons(daemons)

  # If an authorization server is supplied without an explicit
  # resource-server config, derive a matching one so issued tokens
  # are accepted by the same process. If both are set, sanity-check
  # that issuer + audience agree.
  if (!is.null(oauth_as)) {
    if (!inherits(oauth_as, "mcp_oauth_server_config")) {
      stop("oauth_as must be an oauth_server_config() result")
    }
    if (is.null(auth)) {
      auth <- oauth_config_from_server(oauth_as)
    } else {
      if (!identical(auth$issuer, oauth_as$issuer) ||
          !identical(auth$audience, oauth_as$audience)) {
        stop("oauth_as and auth disagree on issuer/audience")
      }
    }
  }

  state <- new.env(parent = emptyenv())
  state$server <- mcp
  state$auth   <- auth
  state$oauth_as <- oauth_as
  state$allowed_origins <- allowed_origins
  state$allowed_hosts <- allowed_hosts
  state$require_origin <- isTRUE(require_origin)
  state$max_event_log <- as.integer(max_event_log)
  state$path <- path
  state$stateless <- isTRUE(stateless)
  state$enable_json_response <- isTRUE(enable_json_response)
  state$onsessioninitialized <- onsessioninitialized
  state$onsessionclosed <- onsessionclosed

  handlers <- list(
    nanonext::handler(path, http_post_handler(state),  method = "POST"),
    nanonext::handler_stream(path, http_get_handler(state),
                             on_close = http_stream_on_close(state),
                             method = "GET"),
    nanonext::handler(path, http_delete_handler(state), method = "DELETE"),
    # Public OAuth resource-server metadata discovery
    # (RFC 9728 / MCP authorization spec).
    nanonext::handler("/.well-known/oauth-protected-resource",
                      http_protected_resource_metadata_handler(state),
                      method = "GET"),
    # Spec-recommended 405 with `Allow` for unsupported methods.
    nanonext::handler(path, http_method_not_allowed_handler(state),
                      method = "PUT"),
    nanonext::handler(path, http_method_not_allowed_handler(state),
                      method = "PATCH"),
    nanonext::handler(path, http_method_not_allowed_handler(state),
                      method = "HEAD")
  )
  # Authorization-server endpoints (RFC 8414 + RFC 7591 + PKCE-S256)
  # are mounted only when an `oauth_as` config was provided.
  if (!is.null(oauth_as)) {
    handlers <- c(handlers, list(
      nanonext::handler("/.well-known/oauth-authorization-server",
                        oauth_as_metadata_handler(oauth_as),
                        method = "GET"),
      nanonext::handler("/authorize",
                        oauth_as_authorize_handler(oauth_as),
                        method = "GET"),
      nanonext::handler("/token",
                        oauth_as_token_handler(oauth_as),
                        method = "POST"),
      nanonext::handler("/register",
                        oauth_as_register_handler(oauth_as),
                        method = "POST"),
      nanonext::handler("/jwks",
                        oauth_as_jwks_handler(oauth_as),
                        method = "GET")))
  }
  url <- sprintf("%s://%s:%d",
                 if (is.null(tls)) "http" else "https",
                 host, port)
  srv <- nanonext::http_server(url, handlers = handlers, tls = tls)
  srv$start()
  on.exit(safely(srv$close(), log = FALSE), add = TRUE)
  if (!isTRUE(block)) {
    on.exit() # cancel the close hook so caller manages lifecycle
    return(invisible(srv))
  }
  repeat later::run_now(timeoutSecs = Inf)
}

# Helpers ----------------------------------------------------------------

validate_origin <- function(state, req) {
  origin <- header_get(req$headers, "Origin")
  if (is.null(origin) || identical(origin, "")) {
    return(!state$require_origin)
  }
  # Exact equality against allowed_origins is the spec-recommended check.
  if (origin %in% state$allowed_origins) return(TRUE)
  # Also accept any allowed_origin with a `:<port>` suffix appended,
  # since browsers/clients normally send the full origin including
  # the port. "http://localhost" should match "http://localhost:1234"
  # without letting "http://localhost.evil.com" through.
  for (allowed in state$allowed_origins) {
    if (grepl(paste0("^",
                     gsub("([][.\\+*?^$|(){}])", "\\\\\\1", allowed,
                          perl = TRUE),
                     ":\\d+$"),
              origin, perl = TRUE)) {
      return(TRUE)
    }
  }
  FALSE
}

# Reject requests whose Host header doesn't match the server's configured
# bind address — a complementary DNS-rebinding defense alongside Origin.
validate_host <- function(state, req) {
  if (is.null(state$allowed_hosts) || length(state$allowed_hosts) == 0L) {
    return(TRUE)
  }
  host <- header_get(req$headers, "Host")
  if (is.null(host)) return(FALSE)
  # Allow either bare host or host:port.
  bare <- sub(":.*$", "", host)
  host %in% state$allowed_hosts || bare %in% state$allowed_hosts
}

check_protocol_version <- function(req) {
  v <- header_get(req$headers, "MCP-Protocol-Version")
  if (is.null(v)) return(TRUE)
  v %in% mcp_supported_protocol_versions()
}

# Accept header must list both application/json and text/event-stream
# per the Streamable HTTP spec; clients can satisfy this with */* as well.
check_accept <- function(req) {
  accept <- header_get(req$headers, "Accept")
  if (is.null(accept) || identical(accept, "")) return(TRUE)
  if (grepl("\\*/\\*", accept, fixed = FALSE)) return(TRUE)
  has_json <- grepl("application/json", accept, fixed = TRUE)
  has_sse <- grepl("text/event-stream", accept, fixed = TRUE)
  has_json && has_sse
}

check_content_type <- function(req) {
  ct <- header_get(req$headers, "Content-Type")
  if (is.null(ct) || identical(ct, "")) return(TRUE)
  grepl("^application/json(;|$)", ct, perl = TRUE)
}

http_make_response <- function(status, body = NULL,
                               headers = NULL,
                               json = FALSE) {
  hd <- headers %||% character(0L)
  if (isTRUE(json)) {
    hd <- c(hd, c("Content-Type" = "application/json"))
  }
  out <- list(status = as.integer(status), headers = hd)
  if (!is.null(body)) out$body <- body
  out
}

# Build a JSON-RPC error envelope as the response body for transport-level
# failures (bad Origin, bad Accept, malformed JSON, etc.). Matches the
# TypeScript SDK's createJsonErrorResponse() semantics.
http_jsonrpc_error <- function(status, code, message,
                               headers = NULL) {
  body <- jrpc_encode(list(jsonrpc = JSONRPC_VERSION, id = NULL,
                           error = list(code = as.integer(code),
                                        message = as.character(message))))
  http_make_response(status, body = body, headers = headers, json = TRUE)
}

# RFC 6750-compliant WWW-Authenticate challenge.
www_authenticate_challenge <- function(state,
                                       error = NULL,
                                       description = NULL,
                                       scope = NULL) {
  if (is.null(state$auth)) return("Bearer")
  parts <- c(sprintf('realm="%s"', state$auth$audience),
             sprintf('resource_metadata="%s"',
                     sub("/+$", "", state$auth$issuer)))
  if (!is.null(error)) {
    parts <- c(parts, sprintf('error="%s"', error))
  }
  if (!is.null(description)) {
    parts <- c(parts, sprintf('error_description="%s"',
                              gsub('"', "'", description, fixed = TRUE)))
  }
  if (!is.null(scope) && length(scope) > 0L) {
    parts <- c(parts, sprintf('scope="%s"', paste(scope, collapse = " ")))
  }
  paste("Bearer", paste(parts, collapse = ", "))
}

http_unauthorized <- function(state, status = 401L,
                              error = "invalid_token",
                              description = "Bearer token missing, malformed, or expired") {
  challenge <- www_authenticate_challenge(state, error = error,
                                          description = description)
  http_make_response(status,
    body = jrpc_encode(list(jsonrpc = JSONRPC_VERSION, id = NULL,
                            error = list(code = -32001L,
                                         message = description))),
    headers = c("WWW-Authenticate" = challenge,
                "Content-Type" = "application/json"))
}

http_forbidden <- function(state, scope = NULL) {
  challenge <- www_authenticate_challenge(state,
    error = "insufficient_scope",
    description = "Insufficient scope for requested operation",
    scope = scope)
  http_make_response(403L,
    body = jrpc_encode(list(jsonrpc = JSONRPC_VERSION, id = NULL,
                            error = list(code = -32002L,
                                         message = "insufficient scope"))),
    headers = c("WWW-Authenticate" = challenge,
                "Content-Type" = "application/json"))
}

post_authenticate <- function(state, req) {
  if (is.null(state$auth)) {
    return(list(ok = TRUE, subject = NULL, scopes = character(0L),
                reason = "ok"))
  }
  authz <- header_get(req$headers, "Authorization")
  res <- oauth_verify_bearer(state$auth, authz)
  # Distinguish "no/bad token" from "insufficient scope" so the HTTP
  # layer can pick 401 vs 403 per RFC 6750.
  if (isTRUE(res$ok)) return(res)
  reason <- if (is.null(authz) || !nzchar(authz)) {
    "invalid_token"
  } else if (!is.null(res$reason)) {
    res$reason
  } else {
    "invalid_token"
  }
  list(ok = FALSE, reason = reason)
}

http_post_handler <- function(state) {
  function(req) {
    if (!validate_origin(state, req)) {
      return(http_jsonrpc_error(403L, -32603, "bad origin"))
    }
    if (!validate_host(state, req)) {
      return(http_jsonrpc_error(403L, -32603, "bad host"))
    }
    if (!check_protocol_version(req)) {
      return(http_jsonrpc_error(400L, -32602,
        "unsupported MCP-Protocol-Version"))
    }
    if (!check_accept(req)) {
      return(http_jsonrpc_error(406L, -32602,
        "Accept must include application/json and text/event-stream"))
    }
    if (!check_content_type(req)) {
      return(http_jsonrpc_error(415L, -32602,
        "Content-Type must be application/json"))
    }
    auth_res <- post_authenticate(state, req)
    if (!isTRUE(auth_res$ok)) {
      if (identical(auth_res$reason, "insufficient_scope")) {
        return(http_forbidden(state,
                              scope = state$auth$required_scopes))
      }
      return(http_unauthorized(state))
    }

    body_text <- if (is.null(req$body)) "" else rawToChar(req$body)
    msg <- jrpc_decode(body_text)
    if (is.null(msg)) {
      return(http_jsonrpc_error(400L, jrpc_codes$parse_error,
                                "parse error"))
    }

    # JSON-RPC batch: a top-level array of envelopes. We process each
    # entry through route_message and stream the responses back as a
    # JSON array (or 202 when none of the entries expect a result).
    is_batch <- is.list(msg) && is.null(names(msg)) &&
                length(msg) > 0L && is.list(msg[[1L]])

    is_init <- isTRUE(is.list(msg) && identical(msg$method, "initialize"))
    session_id <- header_get(req$headers, "Mcp-Session-Id")

    if (is_init && isTRUE(state$stateless)) {
      ephemeral <- new_ephemeral_session(state$server)
      out <- route_message(state$server, ephemeral, msg)
      return(http_make_response(200L,
        body = jrpc_encode(out),
        headers = c("MCP-Protocol-Version" = mcp_protocol_version(),
                    "Content-Type" = "application/json")))
    }
    if (is_init) {
      # Reject re-initialization on an already-active session.
      if (!is.null(session_id) &&
          exists(session_id, envir = state$server$sessions,
                 inherits = FALSE)) {
        return(http_jsonrpc_error(400L, jrpc_codes$invalid_request,
          "Server already initialized for this session"))
      }
      session_id <- new_uuid()
      session <- new_http_session(state, session_id, auth_res)
      assign(session_id, session, envir = state$server$sessions)
      if (is.function(state$onsessioninitialized)) {
        safely(state$onsessioninitialized(session_id), log = TRUE)
      }
      out <- route_message(state$server, session, msg)
      env_extra_headers <- c("Mcp-Session-Id" = session_id,
                             "MCP-Protocol-Version" = mcp_protocol_version())
      if (is.list(out) && isTRUE(out$.async)) {
        # Should not happen for initialize; safety net.
        env_f <- new.env(parent = emptyenv()); env_f$done <- FALSE
        promises::then(finalize_async(out, state$server, session),
                       onFulfilled = function(v) {
                         env_f$value <- v; env_f$done <- TRUE
                       },
                       onRejected = function(e) {
                         env_f$value <- NULL; env_f$done <- TRUE
                       })
        t0 <- Sys.time()
        while (!isTRUE(env_f$done) &&
               difftime(Sys.time(), t0, units = "secs") < 5) {
          later::run_now(timeoutSecs = 0.05)
        }
        out <- env_f$value %||% jrpc_response(msg$id, list())
      }
      return(http_make_response(200L,
                                body = jrpc_encode(out),
                                headers = c(env_extra_headers,
                                            c("Content-Type" = "application/json"))))
    }

    # Stateless mode: handle every request without session state.
    if (isTRUE(state$stateless)) {
      ephemeral <- new_ephemeral_session(state$server)
      ephemeral$auth_subject <- auth_res$subject
      ephemeral$auth_scopes <- auth_res$scopes
      ephemeral$request_info <- list(
        method = req$method, uri = req$uri, headers = req$headers)
      out <- route_message(state$server, ephemeral, msg)
      if (is.null(out)) {
        return(http_make_response(202L,
          headers = c("Content-Type" = "application/json")))
      }
      if (is.list(out) && isTRUE(out$.async)) {
        out <- drive_async_to_completion(out, state$server, ephemeral)
      }
      return(http_make_response(200L,
        body = jrpc_encode(out),
        headers = c("Content-Type" = "application/json")))
    }
    if (is.null(session_id) ||
        !exists(session_id, envir = state$server$sessions, inherits = FALSE)) {
      return(http_make_response(404L,
        body = '{"error":"unknown session"}', json = TRUE))
    }
    session <- get(session_id, envir = state$server$sessions,
                   inherits = FALSE)
    session$auth_subject <- auth_res$subject
    session$auth_scopes <- auth_res$scopes
    # Make raw HTTP request metadata available to handlers via ctx.
    session$request_info <- list(
      method = req$method,
      uri = req$uri,
      headers = req$headers)

    if (is_batch) {
      responses <- list()
      for (m in msg) {
        out <- route_message(state$server, session, m)
        if (is.null(out)) next  # notifications / responses are bookkeeping
        if (is.list(out) && isTRUE(out$.async)) {
          env_f <- new.env(parent = emptyenv()); env_f$done <- FALSE
          promises::then(finalize_async(out, state$server, session),
                         onFulfilled = function(v) {
                           env_f$value <- v; env_f$done <- TRUE
                         },
                         onRejected = function(e) {
                           env_f$value <- jrpc_error(out$.id %||% m$id,
                                                     jrpc_codes$internal_error,
                                                     conditionMessage(e))
                           env_f$done <- TRUE
                         })
          deadline <- Sys.time() + 120
          while (!isTRUE(env_f$done) && Sys.time() < deadline) {
            later::run_now(timeoutSecs = 0.05)
          }
          out <- env_f$value
        }
        responses <- c(responses, list(out))
      }
      if (length(responses) == 0L) {
        return(http_make_response(202L,
          headers = c("Content-Type" = "application/json")))
      }
      return(http_make_response(200L,
        body = jrpc_encode(responses),
        headers = c("Content-Type" = "application/json")))
    }

    kind <- jrpc_kind(msg)
    if (kind == "notification" || kind == "response") {
      route_message(state$server, session, msg)
      return(http_make_response(202L,
                                headers = c("Content-Type" = "application/json")))
    }
    out <- route_message(state$server, session, msg)
    if (is.list(out) && isTRUE(out$.async)) {
      # Block briefly waiting for the promise to resolve; user code runs
      # in the daemon. For simplicity we synchronously poll the promise.
      env_final <- new.env(parent = emptyenv())
      env_final$done <- FALSE
      promises::then(finalize_async(out, state$server, session),
                     onFulfilled = function(v) {
                       env_final$value <- v; env_final$done <- TRUE
                     },
                     onRejected = function(e) {
                       env_final$value <- jrpc_error(out$.id %||% msg$id,
                                                     jrpc_codes$internal_error,
                                                     conditionMessage(e))
                       env_final$done <- TRUE
                     })
      deadline <- Sys.time() + 120
      while (!isTRUE(env_final$done) && Sys.time() < deadline) {
        later::run_now(timeoutSecs = 0.05)
      }
      out <- env_final$value %||% jrpc_error(msg$id,
                                             jrpc_codes$internal_error,
                                             "tool timed out")
    }
    http_make_response(200L,
                      body = jrpc_encode(out),
                      headers = c("Content-Type" = "application/json"))
  }
}

http_get_handler <- function(state) {
  function(conn, req) {
    if (!validate_origin(state, req)) {
      conn$set_status(403L); conn$send("bad origin"); conn$close(); return()
    }
    if (!check_protocol_version(req)) {
      conn$set_status(400L); conn$send("bad MCP-Protocol-Version")
      conn$close(); return()
    }
    auth_res <- post_authenticate(state, req)
    if (!isTRUE(auth_res$ok)) {
      conn$set_status(401L)
      challenge <- if (!is.null(state$auth)) {
        sprintf('Bearer realm="%s", resource_metadata="%s"',
                state$auth$audience,
                sub("/+$", "", state$auth$issuer))
      } else "Bearer"
      conn$set_header("WWW-Authenticate", challenge)
      conn$send("unauthorized"); conn$close(); return()
    }
    session_id <- header_get(req$headers, "Mcp-Session-Id")
    if (is.null(session_id) ||
        !exists(session_id, envir = state$server$sessions, inherits = FALSE)) {
      conn$set_status(404L); conn$send("unknown session")
      conn$close(); return()
    }
    session <- get(session_id, envir = state$server$sessions,
                   inherits = FALSE)
    conn$set_status(200L)
    conn$set_header("Content-Type", "text/event-stream")
    conn$set_header("Cache-Control", "no-cache")
    conn$set_header("X-Accel-Buffering", "no")
    assign(as.character(conn$id), conn, envir = session$gets)
    # SEP-1699 priming event: send `id: 0`, `retry: 3000`, empty data
    # as the first frame so the client can install its EventSource and
    # learn the retry cadence before any real payload arrives.
    emit_priming_event(conn)
    # Replay any events after Last-Event-ID.
    last_id <- header_get(req$headers, "Last-Event-ID")
    if (!is.null(last_id) && !identical(last_id, "0")) {
      for (e in session$replay_after(last_id)) {
        send_sse_raw(conn, e$payload, id = e$id)
      }
    }
    # Keep alive — a comment line is innocuous to SSE clients.
    schedule_keepalive(conn)
    invisible(NULL)
  }
}

http_stream_on_close <- function(state) {
  function(conn) {
    # Remove conn from any session that may have stored it.
    for (sid in ls(state$server$sessions, all.names = TRUE)) {
      sess <- get(sid, envir = state$server$sessions, inherits = FALSE)
      key <- as.character(conn$id)
      if (exists(key, envir = sess$gets, inherits = FALSE)) {
        rm(list = key, envir = sess$gets)
      }
    }
  }
}

# Build a throwaway Session for a single stateless request. Has no SSE
# streams and no subscriptions; the dispatcher treats it like any
# other session for the duration of the call.
new_ephemeral_session <- function(server) {
  Session$new("stateless", server, function(e) invisible(NULL),
              max_event_log = 1L)
}

# Drive an async marker to completion synchronously by polling the
# later loop. Returns the final JSON-RPC envelope.
drive_async_to_completion <- function(marker, server, session,
                                      deadline_s = 120) {
  env_f <- new.env(parent = emptyenv()); env_f$done <- FALSE
  promises::then(finalize_async(marker, server, session),
    onFulfilled = function(v) { env_f$value <- v; env_f$done <- TRUE },
    onRejected = function(e) {
      env_f$value <- jrpc_error(marker$.id %||% NULL,
                                jrpc_codes$internal_error,
                                conditionMessage(e))
      env_f$done <- TRUE
    })
  deadline <- Sys.time() + deadline_s
  while (!isTRUE(env_f$done) && Sys.time() < deadline) {
    later::run_now(timeoutSecs = 0.05)
  }
  env_f$value %||% jrpc_error(marker$.id %||% NULL,
                              jrpc_codes$internal_error,
                              "stateless dispatch timed out")
}

http_method_not_allowed_handler <- function(state) {
  function(req) {
    http_make_response(405L,
      body = jrpc_encode(list(jsonrpc = JSONRPC_VERSION, id = NULL,
                              error = list(code = jrpc_codes$invalid_request,
                                           message = "Method Not Allowed"))),
      headers = c("Allow" = "GET, POST, DELETE",
                  "Content-Type" = "application/json"))
  }
}

# Serve the RFC 9728 OAuth protected-resource metadata document.
http_protected_resource_metadata_handler <- function(state) {
  function(req) {
    if (is.null(state$auth)) {
      return(http_make_response(404L,
        body = '{"error":"auth not configured"}', json = TRUE))
    }
    cfg <- state$auth
    body <- jrpc_encode(drop_nulls(list(
      resource = cfg$audience,
      authorization_servers = I(cfg$issuer),
      scopes_supported = if (length(cfg$required_scopes) > 0L)
        I(cfg$required_scopes) else NULL,
      bearer_methods_supported = I(c("header"))
    )))
    http_make_response(200L, body = body, json = TRUE)
  }
}

http_delete_handler <- function(state) {
  function(req) {
    if (!validate_origin(state, req)) {
      return(http_make_response(403L))
    }
    if (!check_protocol_version(req)) {
      return(http_make_response(400L,
        body = '{"error":"unsupported MCP-Protocol-Version"}',
        json = TRUE))
    }
    auth_res <- post_authenticate(state, req)
    if (!isTRUE(auth_res$ok)) return(http_unauthorized(state))
    session_id <- header_get(req$headers, "Mcp-Session-Id")
    if (is.null(session_id) ||
        !exists(session_id, envir = state$server$sessions, inherits = FALSE)) {
      return(http_make_response(404L, body = '{"error":"unknown session"}',
                                json = TRUE))
    }
    session <- get(session_id, envir = state$server$sessions,
                   inherits = FALSE)
    session$close()
    rm(list = session_id, envir = state$server$sessions)
    if (is.function(state$onsessionclosed)) {
      safely(state$onsessionclosed(session_id), log = TRUE)
    }
    safely(state$server$fire_close(session_id), log = FALSE)
    http_make_response(204L)
  }
}

new_http_session <- function(state, session_id, auth_res) {
  http_write <- function(envelope) {
    payload <- jrpc_encode(envelope)
    sess <- get(session_id, envir = state$server$sessions,
                inherits = FALSE)
    # Broadcast to all open GET streams for this session.
    keys <- ls(sess$gets, all.names = TRUE)
    if (length(keys) == 0L) {
      # No open stream — record the event so a reconnecting client can
      # pick it up. (Real clients should open GET to receive these.)
      sess$record_event(payload)
      return(invisible(NULL))
    }
    for (k in keys) {
      conn <- get(k, envir = sess$gets, inherits = FALSE)
      record_and_send_sse(sess, conn, envelope)
    }
  }
  s <- Session$new(session_id, state$server, http_write,
                   max_event_log = state$max_event_log)
  s$auth_subject <- auth_res$subject
  s$auth_scopes <- auth_res$scopes
  s
}

schedule_keepalive <- function(conn) {
  later::later(function() {
    if (safely(conn$send(": keep\n\n"), log = FALSE) %||% TRUE) {
      schedule_keepalive(conn)
    }
  }, delay = 15)
}
