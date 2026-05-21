# Regression tests adapted from python-sdk/tests/issues/test_*.py.
# Each test_that block references the GitHub issue that motivated it.

# --- python-sdk #176 — progress token = 0 must still emit progress ----

test_that("send_progress fires when progressToken is a falsy zero", {
  srv <- new_server("t")
  sent <- new.env(parent = emptyenv()); sent$msgs <- list()
  s <- mcpserver:::Session$new("t", srv, function(e) {
    sent$msgs <- c(sent$msgs, list(e))
  })
  # progressToken = 0 — the previous bug treated this as missing.
  mcpserver:::send_progress(s, 0L, progress = 1, total = 3,
                            message = "first")
  expect_equal(length(sent$msgs), 1L)
  expect_equal(sent$msgs[[1L]]$params$progressToken, 0L)
})

# --- python-sdk #192 — response id round-trips request id ------------

test_that("response envelope echoes request id exactly (numeric and string)", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    "echo", "echo", schema(list(t = property_string(required = TRUE))),
    handler = function(args, ctx) response_text(args$t)))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "initialize",
    params = list(protocolVersion = "2025-06-18", capabilities = list())))

  # Integer id
  r1 <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 7L, method = "tools/list"))
  expect_equal(r1$id, 7L)
  # String id
  r2 <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = "abc-123", method = "tools/list"))
  expect_equal(r2$id, "abc-123")
  # Numeric (double) id
  r3 <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 42.5, method = "tools/list"))
  expect_equal(r3$id, 42.5)
})

# --- python-sdk #342 — blob resources use standard base64 ------------

test_that("response_image / response_resource use standard base64 (+/) not urlsafe (-_)", {
  # The byte sequence 0xFA 0xFB 0xFC encodes to '+vv8' in standard and
  # '-vv8' in URL-safe base64. The TS SDK uses standard.
  bytes <- as.raw(c(0xFA, 0xFB, 0xFC))
  img <- response_image(bytes, "image/png")
  expect_false(grepl("[-_]", img$data, fixed = FALSE),
               info = paste("got:", img$data))
  expect_true(grepl("[+/]", img$data, fixed = FALSE))
})

# --- python-sdk #973 — URI template percent-decoding ------------------

test_that("URI template variable expansion percent-encodes special chars on the way out", {
  expanded <- mcpserver:::uri_template_expand("demo://x/{id}",
                                              list(id = "a b%c"))
  expect_match(expanded, "demo://x/a%20b%25c", fixed = TRUE)
})

# --- python-sdk #1574 — non-file:// URIs survive round-trip ----------

test_that("custom-scheme and relative-style URIs survive resources/read template matching", {
  srv <- new_server("t")
  add_capability(srv, new_resource_template(
    name = "items",
    description = "x",
    uri_template = "custom://{id}",
    mime_type = "text/plain",
    handler = function(params, ctx) sprintf("item %s",
                                            params$variables$id)))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  marker <- mcpserver:::handle_resources_read(srv, s,
    list(uri = "custom://abc-def_42"), list(id = 1))
  expect_true(isTRUE(marker$.resource_call))
  expect_equal(marker$params$variables$id, "abc-def_42")
})

# --- python-sdk #88 — server survives a timed-out request -----------

test_that("a request that times out doesn't take the server with it", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    "fast", "fast", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "initialize",
    params = list(protocolVersion = "2025-06-18", capabilities = list())))
  # Synthesise a timeout on a server->client request.
  err <- tryCatch(
    mcpserver:::call_client_blocking(s, "test/method",
                                     params = NULL, timeout = 0.05),
    error = function(e) conditionMessage(e))
  expect_match(err, "timed out")
  # And the dispatcher still works.
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 2, method = "tools/list"))
  expect_equal(length(resp$result$tools), 1L)
})
