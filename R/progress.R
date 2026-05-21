# Progress notifications -------------------------------------------------

send_progress <- function(session, token, progress,
                          total = NULL, message = NULL,
                          related_request_id = NULL) {
  if (is.null(token)) return(invisible(NULL))
  params <- drop_nulls(list(
    progressToken = token,
    progress = progress,
    total = total,
    message = message,
    relatedRequestId = related_request_id
  ))
  envelope <- jrpc_notification("notifications/progress", params)
  session$send(envelope)
  invisible(envelope)
}

handle_progress_in <- function(server, session, params) {
  # If this progress notification's token matches an outgoing pending
  # request that registered with `reset_timeout_on_progress = TRUE`,
  # extend that request's deadline. Bound by max_deadline if set.
  token <- params$progressToken
  if (is.null(token) || is.null(session$pending)) return(invisible(NULL))
  now <- as.numeric(Sys.time())
  for (key in ls(session$pending, all.names = TRUE)) {
    entry <- get(key, envir = session$pending, inherits = FALSE)
    if (is.null(entry$progress_token)) next
    if (!identical(entry$progress_token, token)) next
    new_deadline <- now + (entry$base_timeout %||% 0)
    if (!is.na(entry$max_deadline)) {
      new_deadline <- min(new_deadline, entry$max_deadline)
    }
    entry$deadline <- new_deadline
    assign(key, entry, envir = session$pending)
  }
  invisible(NULL)
}
