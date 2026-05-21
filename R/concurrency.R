# mirai daemon pool + promise glue ----------------------------------------

# Internal: ensure a mirai daemon pool of at least `n` workers is running.
# Idempotent — calling again with the same or smaller n is a no-op.
ensure_daemons <- function(n = 4L) {
  n <- as.integer(n)
  if (n <= 0L) return(invisible())
  if (isTRUE(.mcp_state$daemons_started) &&
      .mcp_state$daemon_count >= n) return(invisible())
  mirai::daemons(n)
  # Make exported mcpserver helpers (response_text, etc.) available in
  # every daemon so user handlers can reference them.
  safely(mirai::everywhere(suppressPackageStartupMessages(
    library(mcpserver))), log = TRUE)
  .mcp_state$daemons_started <- TRUE
  .mcp_state$daemon_count <- n
  invisible()
}

# Stop all daemons. Called from .onUnload and from teardown helpers.
stop_daemons <- function() {
  if (isTRUE(.mcp_state$daemons_started)) {
    safely(mirai::daemons(0L), log = FALSE)
    .mcp_state$daemons_started <- FALSE
    .mcp_state$daemon_count <- 0L
  }
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
