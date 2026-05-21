# Server-to-client request: roots/list -----------------------------------

request_roots_impl <- function(session, timeout = 5) {
  caps <- session$client_capabilities %||% list()
  if (is.null(caps$roots)) {
    stop("client did not declare roots capability")
  }
  # Pass NULL (omitted) rather than list() — an empty R list serialises
  # as `[]` which the TS SDK rejects (params is expected to be an object
  # when present).
  res <- call_client_blocking(session, "roots/list", NULL, timeout)
  session$roots_cache <- res$roots
  res
}

handle_roots_changed <- function(server, session, params) {
  # Invalidate the cache; tools that need fresh roots will fetch again.
  session$roots_cache <- NULL
  invisible(NULL)
}

# Shared utility used by sampling/elicitation/roots. Send a JSON-RPC
# request to the client and yield to `later::run_now()` until the
# matching response is routed into `session$pending`. Must be called on
# the transport thread (i.e. inside a tool marked `bidirectional = TRUE`),
# because the pending-request table is local to that process.
#
# `reset_timeout_on_progress`: when TRUE, an inbound
# `notifications/progress` whose `progressToken` matches this
# request's token resets the per-call deadline (but never past
# `max_total_timeout` if supplied). The progressToken used by the
# request is automatically generated and surfaced via the request's
# `_meta.progressToken` so the client can correlate.
call_client_blocking <- function(session, method, params, timeout,
                                 reset_timeout_on_progress = FALSE,
                                 max_total_timeout = NULL) {
  if (session$pending_count() >= session$pending_cap) {
    stop(sprintf(
      "pending server->client request cap (%d) reached",
      session$pending_cap))
  }
  rid <- session$new_request_id()
  bag <- new.env(parent = emptyenv())
  bag$done <- FALSE
  bag$value <- NULL
  bag$is_error <- FALSE
  # When progress-driven timeout reset is requested, generate a
  # progressToken and embed it in the outgoing request's `_meta` so
  # the client knows what token to send progress with. The
  # notifications/progress handler will look up by token to find this
  # pending entry.
  progress_token <- NULL
  if (isTRUE(reset_timeout_on_progress)) {
    progress_token <- new_uuid()
    if (is.null(params)) params <- list()
    if (is.null(params$`_meta`)) params$`_meta` <- list()
    params$`_meta`$progressToken <- progress_token
  }
  now <- Sys.time()
  started_at <- as.numeric(now)
  base_timeout <- as.numeric(timeout)
  max_deadline <- if (!is.null(max_total_timeout)) {
    started_at + as.numeric(max_total_timeout)
  } else {
    NA_real_
  }
  assign(as.character(rid),
         list(cv = NULL, env = bag,
              deadline = started_at + base_timeout,
              base_timeout = base_timeout,
              progress_token = progress_token,
              started_at = started_at,
              max_deadline = max_deadline),
         envir = session$pending)
  session$send(jrpc_request(method, params, id = rid))
  repeat {
    if (isTRUE(bag$done)) break
    entry <- get(as.character(rid), envir = session$pending,
                 inherits = FALSE)
    if (Sys.time() >= entry$deadline) break
    later::run_now(timeoutSecs = 0.05)
  }
  key <- as.character(rid)
  if (exists(key, envir = session$pending, inherits = FALSE)) {
    rm(list = key, envir = session$pending)
  }
  if (!isTRUE(bag$done)) {
    # Tell the client we're abandoning the request so they don't
    # silently wait forever.
    safely(session$send(jrpc_notification(
      "notifications/cancelled",
      list(requestId = rid, reason = "timeout"))), log = FALSE)
    stop(sprintf("server->client request '%s' timed out", method))
  }
  if (isTRUE(bag$is_error)) {
    stop(sprintf("server->client request '%s' errored: %s",
                 method,
                 bag$value$message %||% "<unknown>"))
  }
  bag$value
}
