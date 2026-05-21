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
  session$send(jrpc_notification("notifications/progress", params))
}

handle_progress_in <- function(server, session, params) {
  # Server doesn't act on inbound progress; placeholder for future hooks.
  invisible(NULL)
}
