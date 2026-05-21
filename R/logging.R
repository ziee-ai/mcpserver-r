# Logging notifications (notifications/message) --------------------------

# Numeric severity used for filtering against the session's current level.
log_level_rank <- function(level) {
  idx <- match(level, LOG_LEVELS)
  if (is.na(idx)) 0L else as.integer(idx)
}

handle_logging_set_level <- function(server, session, params, msg) {
  lvl <- params$level
  if (is.null(lvl) || !lvl %in% LOG_LEVELS) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params,
                      "invalid log level"))
  }
  session$log_level <- lvl
  list()
}

# Emit a logging notification respecting the session's current level.
# Returns the envelope that was sent (or NULL when filtered) so the
# caller can mirror it into a task message queue.
send_log <- function(session, level, message, logger = NULL,
                     data = NULL) {
  if (log_level_rank(level) < log_level_rank(session$log_level)) {
    return(invisible(NULL))
  }
  params <- drop_nulls(list(
    level = level,
    logger = logger,
    data = if (is.null(data)) list(message = message) else data
  ))
  envelope <- jrpc_notification("notifications/message", params)
  session$send(envelope)
  invisible(envelope)
}
