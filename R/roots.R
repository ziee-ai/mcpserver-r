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
call_client_blocking <- function(session, method, params, timeout) {
  rid <- session$new_request_id()
  bag <- new.env(parent = emptyenv())
  bag$done <- FALSE
  bag$value <- NULL
  bag$is_error <- FALSE
  # Register without a cv: we drive completion via later::run_now.
  assign(as.character(rid),
         list(cv = NULL, env = bag, deadline = NA_real_),
         envir = session$pending)
  session$send(jrpc_request(method, params, id = rid))
  deadline <- Sys.time() + as.numeric(timeout)
  while (!isTRUE(bag$done) && Sys.time() < deadline) {
    later::run_now(timeoutSecs = 0.05)
  }
  key <- as.character(rid)
  if (exists(key, envir = session$pending, inherits = FALSE)) {
    rm(list = key, envir = session$pending)
  }
  if (!isTRUE(bag$done)) {
    stop(sprintf("server->client request '%s' timed out", method))
  }
  if (isTRUE(bag$is_error)) {
    stop(sprintf("server->client request '%s' errored: %s",
                 method,
                 bag$value$message %||% "<unknown>"))
  }
  bag$value
}
