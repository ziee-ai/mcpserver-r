# SSE helpers used by the HTTP transport ---------------------------------

# Format an outgoing JSON-RPC envelope as an SSE event and record it in
# the session's replay buffer.
record_and_send_sse <- function(session, conn, envelope) {
  payload <- jrpc_encode(envelope)
  id <- session$record_event(payload)
  safely(conn$send(nanonext::format_sse(payload, id = id)), log = TRUE)
  invisible(id)
}

# Send a single SSE event without recording (used for replay-truncated
# sentinel and keepalives).
send_sse_raw <- function(conn, data, id = NULL, event = NULL) {
  safely(conn$send(nanonext::format_sse(data, id = id, event = event)),
         log = TRUE)
}
