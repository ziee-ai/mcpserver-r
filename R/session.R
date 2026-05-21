# Per-connection session state -------------------------------------------

# An MCP session bundles together the negotiated client capabilities, the
# active subscriptions, pending server-to-client requests, logging level,
# and the transport-specific write callback. There is one Session per stdio
# process and one per Mcp-Session-Id on Streamable HTTP.

Session <- R6::R6Class(
  "Session",
  public = list(
    session_id = NULL,
    server = NULL,
    write_fn = NULL,
    protocol_version = NULL,
    client_capabilities = NULL,
    client_info = NULL,
    initialized = FALSE,
    log_level = "warning",
    roots_cache = NULL,
    auth_subject = NULL,
    auth_scopes = NULL,
    # Per-call HTTP request metadata (headers / uri / method). Updated
    # on every POST so tool handlers can read it via ctx$request_info.
    request_info = NULL,
    # Per-server-stream SSE event log for Last-Event-ID replay (HTTP only).
    event_log = NULL,
    max_event_log = 1000L,
    # Open server-to-client streams (nanonext conn objects), keyed by id.
    gets = NULL,
    # Subscriptions: env keyed by URI.
    subs = NULL,
    # Pending server->client requests: env keyed by request id.
    pending = NULL,
    # Next available negative request id for our outgoing requests.
    next_neg_id = -1L,
    # Cancellation tokens: env keyed by *client* request id.
    cancel = NULL,

    initialize = function(session_id, server, write_fn,
                          max_event_log = 1000L) {
      self$session_id <- session_id
      self$server <- server
      self$write_fn <- write_fn
      self$max_event_log <- as.integer(max_event_log)
      self$event_log <- list()
      self$gets <- new.env(parent = emptyenv())
      self$subs <- new.env(parent = emptyenv())
      self$pending <- new.env(parent = emptyenv())
      self$cancel <- new.env(parent = emptyenv())
    },

    # Allocate a fresh negative id for an outgoing server->client request.
    new_request_id = function() {
      id <- self$next_neg_id
      self$next_neg_id <- id - 1L
      id
    },

    # Maximum outstanding server->client requests per session. Bounds
    # the daemon-pool exposure to a misbehaving client that never
    # answers sampling/elicitation/roots.
    pending_cap = 32L,

    pending_count = function() {
      length(ls(self$pending, all.names = TRUE))
    },

    # Register an outgoing request that the dispatcher should match against
    # a subsequent client `response` envelope. cv is a nanonext condition
    # variable that the daemon-side caller is waiting on.
    register_pending = function(id, cv, env) {
      if (self$pending_count() >= self$pending_cap) {
        stop(sprintf(
          "pending server->client request cap (%d) reached",
          self$pending_cap))
      }
      assign(as.character(id),
             list(cv = cv, env = env, deadline = NA_real_),
             envir = self$pending)
    },

    resolve_pending = function(id, value, is_error = FALSE) {
      key <- as.character(id)
      if (!exists(key, envir = self$pending, inherits = FALSE)) return(invisible(NULL))
      entry <- get(key, envir = self$pending, inherits = FALSE)
      entry$env$value <- value
      entry$env$is_error <- isTRUE(is_error)
      entry$env$done <- TRUE
      if (!is.null(entry$cv)) {
        safely(nanonext::cv_signal(entry$cv), log = FALSE)
      }
      # Do not rm() the entry here; the caller in `call_client_blocking`
      # owns the lifetime and removes it after observing `done = TRUE`.
      invisible(NULL)
    },

    # Cancel a still-pending client request (e.g. when notifications/cancelled
    # arrives or the session is torn down).
    cancel_request = function(client_id) {
      key <- as.character(client_id)
      if (exists(key, envir = self$cancel, inherits = FALSE)) {
        flag <- get(key, envir = self$cancel, inherits = FALSE)
        flag$cancelled <- TRUE
      }
      invisible(NULL)
    },

    # Send an outgoing JSON-RPC envelope using whatever transport-specific
    # write_fn the session was created with.
    send = function(envelope) {
      self$write_fn(envelope)
    },

    # Append to the SSE event log (capped ring buffer).
    record_event = function(payload) {
      id <- new_uuid()
      self$event_log[[length(self$event_log) + 1L]] <-
        list(id = id, payload = payload)
      if (length(self$event_log) > self$max_event_log) {
        self$event_log <- tail(self$event_log,
                               self$max_event_log)
      }
      id
    },

    # Replay events recorded after `last_id`. If `last_id` is not found
    # (already evicted) we return everything currently held with a
    # `replay-truncated` sentinel id prepended.
    replay_after = function(last_id) {
      ids <- vapply(self$event_log, function(e) e$id, character(1L))
      idx <- which(ids == last_id)
      if (length(idx) == 0L) {
        return(c(list(list(id = "replay-truncated", payload = "")),
                 self$event_log))
      }
      if (idx == length(ids)) return(list())
      self$event_log[(idx + 1L):length(ids)]
    },

    # Tear down: close any open SSE streams, cancel pending requests, free
    # resources.
    close = function() {
      for (k in ls(self$gets, all.names = TRUE)) {
        conn <- get(k, envir = self$gets, inherits = FALSE)
        safely(conn$close(), log = FALSE)
        rm(list = k, envir = self$gets)
      }
      for (k in ls(self$pending, all.names = TRUE)) {
        entry <- get(k, envir = self$pending, inherits = FALSE)
        entry$env$is_error <- TRUE
        entry$env$value <- list(code = -32000L, message = "session closed")
        entry$env$done <- TRUE
        nanonext::cv_signal(entry$cv)
        rm(list = k, envir = self$pending)
      }
    }
  )
)

# Make_ctx builds the small `ctx` object that user handlers receive as
# their second argument. We keep it as a plain environment so users can
# stash state on it; the methods are defined as closures over the session.
make_ctx <- function(session, msg = NULL) {
  ctx <- new.env(parent = emptyenv())
  ctx$session_id <- session$session_id
  ctx$client_capabilities <- session$client_capabilities
  ctx$auth_subject <- session$auth_subject
  ctx$auth_scopes <- session$auth_scopes
  ctx$progress_token <- msg$params$`_meta`$progressToken
  ctx$request_info <- session$request_info
  ctx$.session <- session
  ctx$.msg     <- msg
  class(ctx) <- c("McpCtx", "environment")
  ctx
}
