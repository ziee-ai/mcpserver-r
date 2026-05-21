# Cancellation notifications --------------------------------------------

handle_cancelled <- function(server, session, params) {
  rid <- params$requestId
  if (!is.null(rid)) {
    session$cancel_request(rid)
  }
  invisible(NULL)
}
