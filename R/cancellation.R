# Cancellation -----------------------------------------------------------

# Each in-flight `tools/call` (and any other dispatcher path that wants
# cancellation) allocates an entry in `session$cancel`. The entry is an
# environment so the in-process `cancelled` flag has reference semantics
# (mutations on the transport thread are observed by bidirectional tool
# handlers that share the same R process). The entry also carries a
# `flag_path` — a file path that the transport thread `touch`es on
# cancellation; `mirai` daemons running non-bidirectional handlers can
# observe the cancel by polling `file.exists(flag_path)`, since the
# daemon has only a serialised copy of the env (mutations don't
# propagate) and `nanonext::cv` external pointers don't survive
# serialisation either.

cancel_entry_open <- function(session, msg_id) {
  path <- file.path(tempdir(),
                    sprintf("mcpserver-cancel-%s.flag", new_uuid()))
  entry <- new.env(parent = emptyenv())
  entry$cancelled <- FALSE
  entry$flag_path <- path
  entry$on_cancel_fns <- list()
  assign(as.character(msg_id), entry, envir = session$cancel)
  entry
}

# Mark an entry cancelled: flip the in-process flag, touch the flag
# file (visible cross-process), and fire any registered callbacks
# on the current thread.
cancel_entry_signal <- function(entry) {
  if (is.null(entry)) return(invisible(NULL))
  entry$cancelled <- TRUE
  if (!is.null(entry$flag_path)) {
    safely(file.create(entry$flag_path, showWarnings = FALSE),
           log = FALSE)
  }
  for (fn in entry$on_cancel_fns) {
    safely(fn(), log = TRUE)
  }
  invisible(NULL)
}

# Close an entry: unlink the flag file and remove the entry from
# the session's cancel table. Idempotent — safe to call for an id
# that was never opened or has already been closed.
cancel_entry_close <- function(session, msg_id) {
  key <- as.character(msg_id)
  if (is.null(session$cancel) ||
      !exists(key, envir = session$cancel, inherits = FALSE)) {
    return(invisible(NULL))
  }
  entry <- get(key, envir = session$cancel, inherits = FALSE)
  if (!is.null(entry$flag_path)) {
    safely(unlink(entry$flag_path), log = FALSE)
  }
  rm(list = key, envir = session$cancel)
  invisible(NULL)
}

# Sweep all entries from a session's cancel table; called from
# `Session$close()` so a torn-down session doesn't leave flag files
# behind.
cancel_sweep_session <- function(session) {
  if (is.null(session$cancel)) return(invisible(NULL))
  for (k in ls(session$cancel, all.names = TRUE)) {
    entry <- get(k, envir = session$cancel, inherits = FALSE)
    if (!is.null(entry$flag_path)) {
      safely(unlink(entry$flag_path), log = FALSE)
    }
    rm(list = k, envir = session$cancel)
  }
  invisible(NULL)
}

# Notifications/cancelled handler ----------------------------------------

handle_cancelled <- function(server, session, params) {
  rid <- params$requestId
  if (!is.null(rid)) {
    session$cancel_request(rid)
  }
  invisible(NULL)
}
