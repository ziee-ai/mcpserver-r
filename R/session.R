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
    # Resolved user record (mcp_users row) when the subject matches a
    # user in the configured admin store, otherwise NULL.
    user = NULL,
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

    # Per-session resource registry (TS SDK's "session-scoped resources").
    # Tools may register a resource on a given session at call time; the
    # entry is torn down when the session is closed.
    session_resources = NULL,

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
      self$session_resources <- new.env(parent = emptyenv())
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
    # arrives or the session is torn down). Sets the in-process flag,
    # touches the cross-process flag file so daemon-side tool handlers
    # observe the cancel, and fires any `ctx$on_cancel(fn)` callbacks
    # the handler registered.
    cancel_request = function(client_id) {
      key <- as.character(client_id)
      if (exists(key, envir = self$cancel, inherits = FALSE)) {
        entry <- get(key, envir = self$cancel, inherits = FALSE)
        cancel_entry_signal(entry)
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
        if (!is.null(entry$cv)) {
          safely(nanonext::cv_signal(entry$cv), log = FALSE)
        }
        rm(list = k, envir = self$pending)
      }
      if (!is.null(self$session_resources)) {
        for (k in ls(self$session_resources, all.names = TRUE)) {
          rm(list = k, envir = self$session_resources)
        }
      }
      cancel_sweep_session(self)
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
  # User identity fields, derived from session$user when an admin store
  # is configured and the JWT's `sub` resolves to a user record.
  if (!is.null(session$user)) {
    ctx$user_id   <- session$user$id
    ctx$user_name <- session$user$username
    ctx$is_admin  <- isTRUE(session$user$is_admin)
  } else {
    ctx$user_id   <- NULL
    ctx$user_name <- NULL
    ctx$is_admin  <- FALSE
  }
  ctx$progress_token <- msg$params$`_meta`$progressToken
  # Full incoming `_meta` (minus progressToken which is broken out).
  ctx$msg_meta <- {
    meta <- msg$params$`_meta`
    if (is.list(meta)) meta else NULL
  }
  ctx$request_info <- session$request_info
  ctx$.session <- session
  ctx$.msg     <- msg
  class(ctx) <- c("McpCtx", "environment")
  ctx
}

# Resolve the AS-issued subject to a user record via the admin store
# attached to `state`. Returns NULL when no store is configured, or
# the subject isn't a known user. Cached on the session.
session_resolve_user <- function(session, state) {
  store <- if (is.null(state$admin)) NULL else state$admin$store
  if (is.null(store) || is.null(session$auth_subject)) {
    session$user <- NULL
    return(NULL)
  }
  if (!is.null(session$user) &&
      identical(session$user$id, session$auth_subject)) {
    return(session$user)
  }
  u <- safely(store$users$get(session$auth_subject), log = FALSE)
  session$user <- u
  u
}
