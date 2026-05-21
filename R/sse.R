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

# SSE priming event per SEP-1699 (2025-11-25). The first event on any
# new SSE stream MUST be `id: 0`, `retry: 3000`, empty `data:`. This
# lets the client measure RTT, install its EventSource, and learn the
# server's preferred retry interval before any real payload arrives.
emit_priming_event <- function(conn) {
  safely(conn$send(nanonext::format_sse(data = "",
                                        id = "0",
                                        retry = 3000L)),
         log = FALSE)
  invisible(NULL)
}
