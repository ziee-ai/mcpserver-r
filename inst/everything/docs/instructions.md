# mcpserver everything-demo

This server reproduces the MCP reference "everything" feature surface
in R. It exposes every primitive a Model Context Protocol client may
need to exercise:

* Tools that return plain text, structured content, images,
  resource links, embedded resources, annotated content, and errors.
* A long-running tool that emits `notifications/progress`, supports
  cancellation, and honours `_meta.progressToken`.
* Server-to-client requests for sampling, elicitation and roots, plus
  a task-lifecycle research-query tool that streams status updates.
* Two static documentation resources plus two templated dynamic
  resource families (text and binary).
* Four prompts including cascading argument completions and an
  embedded-resource prompt.

Use this server with any MCP client to verify protocol compliance or
as a starting point for your own R-backed MCP server.
