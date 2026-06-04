# mirai daemon pool + promise glue ----------------------------------------

# Internal: ensure a mirai daemon pool of at least `n` workers is running.
# Idempotent — calling again with the same or smaller n is a no-op.
#
# If the caller already created a pool directly via mirai::daemons() (e.g. a
# downstream that loads its own packages into the daemons before calling
# serve_io/serve_http), ADOPT that pool instead of re-initialising it.
# mirai::daemons(n) is NOT idempotent: calling it on a live pool tears the
# pool down and rebuilds it on fresh sockets (the dispatcher URL changes).
# That reset breaks the mirai->later completion wiring on Windows, so async
# tool results are never delivered and stdio tools/call hangs. Adopting the
# existing pool avoids the reset and keeps delivery working.
ensure_daemons <- function(n = 4L) {
  n <- as.integer(n)
  if (n <= 0L) return(invisible())
  # Once we own (or have adopted) a pool, never re-init: an adopted pool must
  # never be reset, and our own pool only grows on an explicit larger request.
  if (isTRUE(.mcp_state$daemons_started) &&
      (isTRUE(.mcp_state$daemons_external) ||
       .mcp_state$daemon_count >= n)) return(invisible())
  # A pool we did not create is already running ⇒ adopt it (no daemons(n)).
  external <- !isTRUE(.mcp_state$daemons_started) &&
    isTRUE(tryCatch(mirai::daemons_set(), error = function(e) FALSE))
  if (!external) mirai::daemons(n)
  # Make exported mcpserver helpers (response_text, etc.) available in every
  # daemon so user handlers can reference them. everywhere() runs a task on
  # the live pool; it does not reset sockets, so it is safe on an adopted pool.
  safely(mirai::everywhere(suppressPackageStartupMessages(
    library(mcpserver))), log = TRUE)
  .mcp_state$daemons_started <- TRUE
  .mcp_state$daemons_external <- external
  .mcp_state$daemon_count <- n
  invisible()
}

# Stop the daemon pool we created. Called from .onUnload and teardown helpers.
# A pool the caller created (adopted) is left untouched — its owner stops it.
stop_daemons <- function() {
  if (isTRUE(.mcp_state$daemons_started) &&
      !isTRUE(.mcp_state$daemons_external)) {
    safely(mirai::daemons(0L), log = FALSE)
  }
  .mcp_state$daemons_started <- FALSE
  .mcp_state$daemons_external <- FALSE
  .mcp_state$daemon_count <- 0L
  invisible()
}

# Run `body` inside a mirai daemon, returning a promise that resolves to
# the value (or rejects with the captured error).
#
# `bindings` is a named list of plain R values sent to the daemon as
# locals; avoid names that begin with `.` to keep clear of mirai's
# reserved arguments (`.expr`, `.args`, ...).
with_mirai <- function(body, bindings = list()) {
  ensure_daemons()
  # Wrap the body so we capture errors as a tagged sentinel rather than
  # letting mirai surface them as errorValue.
  wrapped <- substitute({
    res <- tryCatch(body_expr,
                    error = function(e) {
                      list(.mcp_err = TRUE,
                           message = conditionMessage(e),
                           call = format(conditionCall(e)))
                    })
    res
  }, list(body_expr = body))
  m <- do.call(mirai::mirai, c(list(.expr = wrapped), bindings))
  promises::as.promise(m)
}

# Convenience: run `body` *inline* (no daemon) when we don't need
# concurrency, e.g. for trivial synchronous list endpoints. Returns a
# resolved promise so call sites stay uniform.
inline_promise <- function(value) {
  promises::promise(function(resolve, reject) resolve(value))
}
