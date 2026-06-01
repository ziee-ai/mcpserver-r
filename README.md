# mcpserver

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![MCP Protocol](https://img.shields.io/badge/MCP-2025--11--25-blue)](https://modelcontextprotocol.io)
[![Status: experimental](https://img.shields.io/badge/status-experimental-orange)](#status)

R server SDK for the [Model Context Protocol][mcp]. Build MCP servers
that expose **tools**, **resources**, and **prompts** to large language
model clients over stdio or Streamable HTTP, with the same shape and
semantics as the reference TypeScript / Python / Rust / Kotlin / Java
SDKs.

```r
# install.packages("remotes")
remotes::install_github("tinnlab/mcpserver-r")
```

[mcp]: https://modelcontextprotocol.io

---

## Hello world

```r
library(mcpserver)

srv <- new_server("hello", version = "0.1.0",
                  title = "Hello Server")

add_capability(srv, new_tool(
  name = "greet",
  title = "Greet",
  description = "Greet someone by name",
  input_schema = schema(list(
    name = property_string("Whom to greet", required = TRUE))),
  handler = function(args, ctx) {
    response_text(sprintf("Hello, %s!", args$name))
  }
))

# stdio (one server per process — Claude Desktop / Inspector pattern)
serve_io(srv)
```

Or over Streamable HTTP:

```r
serve_http(srv, port = 3000L)
# POST / GET / DELETE on http://127.0.0.1:3000/mcp
```

---

## What's implemented

| Area | Status |
|---|---|
| **Protocol revisions** | `2024-11-05`, `2025-03-26`, `2025-06-18`, **`2025-11-25` (default)** |
| **Transports** | stdio (`serve_io`), Streamable HTTP (`serve_http`) — stateful + stateless modes, SSE priming events, `Mcp-Session-Id` lifecycle, `Last-Event-ID` resumability |
| **Tools** | `new_tool()` with `title` / `_meta` / `execution.taskSupport`, annotations, input + output schemas, structured content, SEP-986 name validation |
| **Resources** | `new_resource()` + `new_resource_template()` (full RFC 6570 levels 1-4), session-scoped resources, subscriptions + `notifications/resources/updated` |
| **Prompts** | `new_prompt()` with cascading argument completion |
| **Server → client requests** | sampling, elicitation, roots (with `sampling.tools` + `tool_use`/`tool_result` invariants) |
| **Notifications** | logging (RFC-5424 levels with `setLevel` filter), progress (with `relatedRequestId` + progress-driven outbound-request timeout reset), cancellation, debounced `*list_changed` |
| **Tasks (SEP-1686)** | server-side task store, `tasks/list` `tasks/get` `tasks/result` `tasks/cancel`, per-task message queue delivered alongside the result, in-task elicitation (`input_required`), client-task helper `call_client_task()` |
| **OAuth 2.1 Resource Server** | JWT + RFC 7662 introspection, JWKS cache, audience / issuer / scope / expiry checks, RFC 6750 `WWW-Authenticate` challenges (`oauth_config()`) |
| **OAuth 2.1 Authorization Server (demo-grade)** | `oauth_server_config()`: RFC 8414 metadata, PKCE-S256 `/authorize` with RFC 8252 loopback redirect-uri matching, `/token` (`authorization_code` + `refresh_token` with RFC 6749 §6 scope subset enforcement), `/register` (RFC 7591 DCR), `/revoke` (RFC 7009), `/jwks`. Supports `none`, `client_secret_basic`, `client_secret_post`. CORS headers on every endpoint. **Not for production.** |
| **Security** | Origin / Host validation (DNS-rebinding defense), strict capability gating, URI-template security (Kotlin SDK F1-F7 attack vectors), per-request cancellation channel via `ipc://` for cross-process tool handlers |
| **Pluggable abstractions** | event store, JSON-Schema validator, OAuth stores (client / code / token) |

61 exported R functions, 59 roxygen man pages, 93 testthat files,
**~1190 testthat assertions** at 0 failures.

---

## Building blocks

### Tools

```r
add_capability(srv, new_tool(
  name = "get-weather",
  title = "Get Weather",
  description = "Fetch the current weather for a city",
  input_schema = schema(list(
    city = property_string("City name", required = TRUE)
  )),
  output_schema = schema(list(
    temperature = property_number(required = TRUE),
    conditions  = property_string(required = TRUE)
  )),
  handler = function(args, ctx) {
    # ... fetch real weather here ...
    response_structured(
      content = list(response_text(sprintf("It's sunny in %s.", args$city))),
      structured_content = list(temperature = 22.5, conditions = "Sunny"))
  }
))
```

### Resources

```r
# Static
add_capability(srv, new_resource(
  name = "readme", description = "Server README",
  uri = "demo://readme", mime_type = "text/markdown",
  handler = function(params, ctx) "# Hello\nThis is a resource."))

# Templated
add_capability(srv, new_resource_template(
  name = "user-profile", description = "Per-user profile",
  uri_template = "demo://user/{id}/profile",
  mime_type = "application/json",
  handler = function(params, ctx) {
    sprintf('{"id":"%s","name":"User %s"}',
            params$variables$id, params$variables$id)
  }))
```

### Prompts with cascading completion

```r
add_capability(srv, new_prompt(
  name = "team-promotion",
  title = "Team Management",
  arguments = list(
    new_prompt_argument("department", required = TRUE),
    new_prompt_argument("name", required = TRUE)
  ),
  complete = list(
    department = function(value, ctx, args) {
      grep(paste0("^", value), c("Engineering","Sales","Marketing"),
           value = TRUE)
    },
    name = function(value, ctx, args) {
      # Cascading: filter names by the current `department` value.
      pool <- switch(args$department %||% "",
        "Engineering" = c("Alice","Bob","Charlie"),
        "Sales"       = c("David","Eve","Frank"),
        character(0L))
      grep(paste0("^", value), pool, value = TRUE)
    }
  ),
  handler = function(args, ctx) {
    sprintf("Promote %s to head of %s.", args$name, args$department)
  }))
```

### Server-to-client requests

A tool flagged `bidirectional = TRUE` runs on the transport thread so
it can ask the client for help mid-execution.

```r
add_capability(srv, new_tool(
  name = "ask-llm",
  description = "Sample a completion from the client's LLM",
  input_schema = schema(list(
    prompt = property_string(required = TRUE))),
  bidirectional = TRUE,
  handler = function(args, ctx) {
    res <- ctx$request_sampling(
      messages = list(list(
        role = "user",
        content = list(type = "text", text = args$prompt))),
      max_tokens = 256L)
    response_text(res$content$text)
  }))
```

### Long-running tasks (SEP-1686)

Set `tasks = TRUE` to expose the task lifecycle. Progress and log
notifications are mirrored into the task's message queue and delivered
alongside the final result via `tasks/result`.

```r
new_tool(
  name = "long-research",
  description = "Multi-stage research query",
  input_schema = schema(list(
    topic = property_string(required = TRUE))),
  tasks = TRUE,
  bidirectional = TRUE,
  handler = function(args, ctx) {
    for (i in 1:5) {
      if (ctx$task$cancelled()) return(response_error("cancelled"))
      ctx$send_progress(i, total = 5, message = sprintf("stage %d", i))
      Sys.sleep(0.2)
    }
    response_text(sprintf("Report on %s ready.", args$topic))
  })
```

### Cancellation

`ctx$cancelled()` reads a per-request flag file (cross-process safe
when the tool runs inside a `mirai` daemon). User code calls
`ctx$on_cancel(fn)` to register cleanup hooks.

```r
handler = function(args, ctx) {
  conn <- DBI::dbConnect(...)
  ctx$on_cancel(function() DBI::dbDisconnect(conn))
  while (!ctx$cancelled()) {
    # ... long work ...
  }
}
```

---

## OAuth

### As a resource server

```r
auth <- oauth_config(
  issuer   = "https://issuer.example",
  audience = "https://mcp.example",
  jwks_uri = "https://issuer.example/.well-known/jwks.json",
  required_scopes = c("mcp:read", "mcp:write"))

serve_http(srv, auth = auth)
```

Each request must carry `Authorization: Bearer <jwt>`. Failures emit
RFC 6750 `WWW-Authenticate` with `error=`, `error_description=`,
`scope=`, and `resource_metadata=` parameters.

### As a self-contained authorization server (demo-grade)

For local development, conformance fixtures, and integration tests, R
can run a complete AS alongside the MCP endpoint:

```r
as_cfg <- oauth_server_config(
  issuer   = "http://127.0.0.1:3000",
  audience = "http://127.0.0.1:3000/mcp",
  auto_consent = TRUE,                # demo only
  scopes_supported = c("mcp:read", "mcp:write"))

serve_http(srv, oauth_as = as_cfg)
# Auto-derives a matching `auth` resource-server config from the AS,
# so the same process can both mint AND verify its own tokens.
```

Endpoints exposed: `/.well-known/oauth-authorization-server`,
`/authorize` (PKCE-S256, RFC 8252 loopback matching), `/token`
(`authorization_code` + `refresh_token` with §6 scope subset),
`/register` (RFC 7591 DCR), `/revoke` (RFC 7009), `/jwks`.
Supports `client_secret_basic`, `client_secret_post`, and public
(`none`) clients.

**Demo-grade**: in-memory stores, auto-consent on, SHA-256+salt secret
hashing. Use a real IdP (Auth0, Keycloak, Okta) for production.

#### Surviving a restart

The AS signs tokens with an RS256 key. By default that key is **persisted**
to `.mcpserver-as-key.pem` in the working directory and reloaded on the next
start, so tokens minted before a restart keep validating. Two things matter:

* **Stable signing key.** Pass an explicit `key_path` (recommended for
  production) or rely on the working-dir default. Each server should have its
  own key — give servers launched from the same directory distinct `key_path`s
  (or run them from distinct directories), and add the PEM to `.gitignore`.
  If the key rotates (e.g. a fresh ephemeral key per start) every outstanding
  token silently becomes invalid even though its store row still reads
  `active`.
* **Persistent token store.** Use `new_mcp_store("sqlite", path = ...)`; the
  default `"memory"` store loses all token rows on restart.

```r
store  <- new_mcp_store("sqlite", path = "/var/lib/mcp/state.db")
as_cfg <- oauth_server_config(
  issuer   = "https://mcp.example.com",
  audience = "https://mcp.example.com/mcp",
  store    = store,
  key_path = "/var/lib/mcp/as-signing-key.pem")  # stable across restarts
```

---

## Testing strategy

Three independent gates, all on every change:

| Gate | What it checks | Where |
|---|---|---|
| **testthat** (~1190 assertions) | Unit + integration coverage of every R surface | `tests/testthat/test-*.R` |
| **Parity** (39/39) | Drives the server through the official `@modelcontextprotocol/sdk` Node client, walking every tool / resource / prompt | `inst/parity/run-parity.mjs` |
| **Conformance** (`2025-06-18`: 26/26, `2025-11-25`: 39/39) | The official `@modelcontextprotocol/conformance` CLI's scenario battery | `inst/conformance/server.R` + `test-conformance-external.R` |
| **R CMD check `--as-cran`** | CRAN-clean checks | `R CMD check --as-cran` |

Run locally:

```bash
# testthat
Rscript -e 'NOT_CRAN <- "true"; pkgload::load_all(); testthat::test_dir("tests/testthat")'

# parity
Rscript inst/everything/run-http.R --port 44100 &
node inst/parity/run-parity.mjs --url http://127.0.0.1:44100/mcp

# conformance
Rscript inst/conformance/run.R --port 44101 &
./inst/parity/node_modules/.bin/conformance server \
  --url http://127.0.0.1:44101/mcp --spec-version 2025-11-25
```

### Reference servers

* **`inst/everything/server.R`** — the parity counterpart to the TS
  `everything-server`: 18 tools (incl. SEP-1686 task demos +
  bidirectional sampling/elicitation), 4 prompts (cascading
  completion, embedded resources), templated + session-scoped
  resources, the 13-field reference elicitation schema, gzip with
  `data:` URI + env-var-gated URL fetching.
* **`inst/conformance/server.R`** — the fixture the official
  conformance CLI drives.

---

## Project layout

```
mcpserver-r/
├── R/                       # 35 source files
│   ├── server.R             # McpServer class + new_server()
│   ├── tools.R              # new_tool() + tools/list, tools/call
│   ├── resources.R          # new_resource(), new_resource_template()
│   ├── session-resources.R  # per-session resource registry
│   ├── prompts.R            # new_prompt() + completion
│   ├── dispatcher.R         # JSON-RPC routing, async marker
│   ├── transport_http.R     # Streamable HTTP transport
│   ├── transport_stdio.R    # stdio transport
│   ├── auth.R               # OAuth 2.1 resource server
│   ├── auth-server.R        # OAuth 2.1 authorization server
│   ├── tasks.R              # SEP-1686 task store
│   ├── client_task.R        # Outbound client-task helper
│   ├── cancellation.R       # Per-request ipc:// channel
│   └── util-uri.R           # Full RFC 6570 URI templates
├── inst/
│   ├── everything/          # Reference everything-server (R port)
│   ├── conformance/         # Conformance test fixture
│   ├── parity/              # Node parity harness vs @modelcontextprotocol/sdk
│   └── spec/                # Vendored schema fixtures (rust-sdk, spec examples)
├── tests/testthat/          # 93 test files
└── DESCRIPTION
```

---

## Comparison to other MCP SDKs

The R SDK targets parity with the [TypeScript SDK][ts] for tools,
resources, prompts, sampling, elicitation, roots, completions, tasks,
and OAuth. Audit convergence vs `typescript-sdk@v1.29.0` (server-side
test corpus, ~360 in-scope tests): **0 in-scope gaps remaining.**

[ts]: https://github.com/modelcontextprotocol/typescript-sdk

Notable design choices specific to R:

* **`mirai` daemons** for non-blocking tool execution; the transport
  thread stays responsive to inbound traffic and outbound SSE.
* **`nanonext` ipc://** cancellation channel so a tool running in a
  daemon can observe `notifications/cancelled` mid-flight (cv pointers
  don't survive mirai serialisation).
* **`later`-driven** event loop for `request_sampling()` /
  `request_elicitation()` / `request_roots()` while waiting on the
  client — `bidirectional = TRUE` tools run on the transport thread
  and yield via `later::run_now()`.

Out of scope by design (see [NEWS.md](NEWS.md)):
URL elicitation mode, streaming task helpers
(`createMessageStream`/`elicitInputStream`), async session callbacks,
Web-standard `Request`/`Response` transport, multi-step elicitation
demo, proxy OAuth providers, OIDC `id_token`.

---

## Status

Experimental. Version pinned at **0.1.0** while the API surface
settles. Breaking changes are possible before 1.0.

---

## License

[MIT](LICENSE). Copyright 2026 Phi Bya.
