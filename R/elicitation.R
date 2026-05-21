# Server-to-client request: elicitation/create ----------------------------

request_elicitation_impl <- function(session, message,
                                     requested_schema,
                                     timeout = 30) {
  caps <- session$client_capabilities %||% list()
  if (is.null(caps$elicitation)) {
    stop("client did not declare elicitation capability")
  }
  call_client_blocking(session, "elicitation/create",
                       list(message = message,
                            requestedSchema = requested_schema),
                       timeout)
}
