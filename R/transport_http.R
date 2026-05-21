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
#' @param require_origin Whether `Origin` is required (default `TRUE`).
#' @param tls Optional TLS configuration from `nanonext::tls_config()`.
#' @param max_event_log Maximum events retained per session for SSE replay.
#' @param auth Optional auth configuration from [oauth_config()].
#' @param daemons Number of `mirai` daemons.
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
                       require_origin = TRUE,
                       tls = NULL,
                       max_event_log = 1000L,
                       auth = NULL,
                       daemons = 4L,
                       block = TRUE) {
  stopifnot(inherits(mcp, "McpServer"))
  ensure_daemons(daemons)

  state <- new.env(parent = emptyenv())
  state$server <- mcp
  state$auth   <- auth
  state$allowed_origins <- allowed_origins
  state$require_origin <- isTRUE(require_origin)
  state$max_event_log <- as.integer(max_event_log)

  handlers <- list(
    nanonext::handler(path, http_post_handler(state),  method = "POST"),
    nanonext::handler_stream(path, http_get_handler(state),
                             on_close = http_stream_on_close(state),
                             method = "GET"),
    nanonext::handler(path, http_delete_handler(state), method = "DELETE")
  )
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
  any(vapply(state$allowed_origins, function(prefix) {
    startsWith(origin, prefix)
  }, logical(1L)))
}

check_protocol_version <- function(req) {
  v <- header_get(req$headers, "MCP-Protocol-Version")
  if (is.null(v)) return(TRUE)
  identical(v, mcp_protocol_version())
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

http_unauthorized <- function(state) {
  www <- if (!is.null(state$auth)) {
    sprintf('Bearer realm="%s", resource_metadata="%s"',
            state$auth$audience,
            sub("/+$", "", state$auth$issuer))
  } else "Bearer"
  http_make_response(401L, body = '{"error":"unauthorized"}',
                     headers = c("WWW-Authenticate" = www,
                                 "Content-Type" = "application/json"))
}

post_authenticate <- function(state, req) {
  if (is.null(state$auth)) {
    return(list(ok = TRUE, subject = NULL, scopes = character(0L)))
  }
  authz <- header_get(req$headers, "Authorization")
  oauth_verify_bearer(state$auth, authz)
}

http_post_handler <- function(state) {
  function(req) {
    if (!validate_origin(state, req)) {
      return(http_make_response(403L, body = '{"error":"bad origin"}',
                                json = TRUE))
    }
    if (!check_protocol_version(req)) {
      return(http_make_response(400L,
        body = '{"error":"unsupported MCP-Protocol-Version"}',
        json = TRUE))
    }
    auth_res <- post_authenticate(state, req)
    if (!isTRUE(auth_res$ok)) return(http_unauthorized(state))

    body_text <- if (is.null(req$body)) "" else rawToChar(req$body)
    msg <- jrpc_decode(body_text)
    if (is.null(msg)) {
      return(http_make_response(400L,
        body = jrpc_encode(jrpc_error(NULL, jrpc_codes$parse_error,
                                      "parse error")),
        json = TRUE))
    }

    is_init <- isTRUE(is.list(msg) && identical(msg$method, "initialize"))
    session_id <- header_get(req$headers, "Mcp-Session-Id")

    if (is_init) {
      # Always create a fresh session on initialize.
      session_id <- new_uuid()
      session <- new_http_session(state, session_id, auth_res)
      assign(session_id, session, envir = state$server$sessions)
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

    if (is.null(session_id) ||
        !exists(session_id, envir = state$server$sessions, inherits = FALSE)) {
      return(http_make_response(404L,
        body = '{"error":"unknown session"}', json = TRUE))
    }
    session <- get(session_id, envir = state$server$sessions,
                   inherits = FALSE)
    session$auth_subject <- auth_res$subject
    session$auth_scopes <- auth_res$scopes

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
      conn$set_header("WWW-Authenticate", "Bearer")
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
    # Replay any events after Last-Event-ID.
    last_id <- header_get(req$headers, "Last-Event-ID")
    if (!is.null(last_id)) {
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

http_delete_handler <- function(state) {
  function(req) {
    if (!validate_origin(state, req)) {
      return(http_make_response(403L))
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
