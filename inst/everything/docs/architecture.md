# Architecture notes

The everything-demo is built on **mcpserver**:

* `serve_io()` and `serve_http()` provide the stdio and Streamable
  HTTP transports defined by MCP 2025-06-18.
* Every tool, resource, and prompt handler runs inside a `mirai`
  worker daemon so the transport thread remains responsive. Tools that
  issue server-to-client requests opt into running on the transport
  thread with `bidirectional = TRUE`.
* `on_initialized()` hooks register the sampling, elicitation, and
  roots tools only when the connected client declared the matching
  capability, and schedule a deferred `roots/list` synchronisation
  350 ms after initialization.
