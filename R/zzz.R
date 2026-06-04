.onUnload <- function(libpath) {
  # Stop any mirai daemons we spawned. Wrapped in tryCatch because mirai
  # may already have been unloaded.
  tryCatch(
    if (requireNamespace("mirai", quietly = TRUE) &&
        isTRUE(.mcp_state$daemons_started)) {
      mirai::daemons(0L)
    },
    error = function(e) NULL
  )
}

# Package-private mutable state. Used sparingly — long-lived registries
# (sessions, daemon counts) live here. Created at package load.
.mcp_state <- new.env(parent = emptyenv())
.mcp_state$daemons_started <- FALSE
.mcp_state$daemon_count    <- 0L
# TRUE when we adopted a pool the caller created via mirai::daemons()
# directly (rather than creating our own) — we must not reset or stop it.
.mcp_state$daemons_external <- FALSE
