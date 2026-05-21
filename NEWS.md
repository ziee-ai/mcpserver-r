# mcpserver 0.1.0

Initial release.

* Core JSON-RPC 2.0 dispatch (`R/jsonrpc.R`, `R/dispatcher.R`).
* mcpr-style functional API: `new_server()`, `new_tool()`,
  `new_resource()`, `new_resource_template()`, `new_prompt()`,
  `add_capability()`, schema builders, response constructors covering
  text, image, audio, video, resource, resource link, file, error, and
  structured content.
* stdio transport (`serve_io()`) with `mirai` worker daemons for
  non-blocking handler execution and `later::until()`-driven event-loop
  yield.
* Streamable HTTP transport (`serve_http()`) implementing the MCP
  2025-06-18 spec: single endpoint, POST/GET/DELETE, `Mcp-Session-Id`
  lifecycle, `Last-Event-ID` SSE replay, `MCP-Protocol-Version`
  enforcement, `Origin` validation (DNS rebinding defense).
* Notifications: logging (eight levels), progress, cancellation,
  resource subscriptions, list-changed for tools, resources, and
  prompts.
* Bidirectional server-to-client requests: sampling, elicitation, roots
  (tools opt in via `bidirectional = TRUE` to execute on the transport
  thread while the pending-request table is driven via `later`).
* Cascading argument completions and the experimental task store
  scaffolding.
* OAuth 2.1 resource server (`oauth_config()`) with JWT and RFC 7662
  introspection flows, JWKS caching, audience / issuer / scope / expiry
  checks, and RFC 6750 `WWW-Authenticate` challenges.
* Demo server in `inst/everything/` reproducing the MCP reference
  everything-server feature surface (15 tools, 4 resources, 4 prompts,
  cascading completions, conditional sampling/elicitation/roots tools).
* Spec parity gate in `inst/parity/`: a Node script using the official
  `@modelcontextprotocol/sdk` client drives every tool, resource,
  prompt, completion, and the sampling/elicitation/roots handlers.
  Wrapped by `tests/testthat/test-everything-parity.R`.
