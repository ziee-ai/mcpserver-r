# Mines server-relevant assertions from the TS SDK's
# packages/client/test/client/streamableHttp.test.ts and auth.test.ts.

test_that("session id persists across HTTP requests on the server side", {
  srv <- new_server("t")
  add_capability(srv, new_tool("echo", "echo",
    schema(list(text = property_string(required = TRUE))),
    handler = function(args, ctx) response_text(args$text)))
  s <- mcpserver:::Session$new("session-abc", srv,
                                function(e) NULL)
  expect_equal(s$session_id, "session-abc")
  # Init negotiates and stores client capabilities.
  mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1L, method = "initialize",
    params = list(protocolVersion = "2025-06-18",
                  capabilities = list(sampling = list()))))
  expect_true(!is.null(s$client_capabilities))
  expect_true(!is.null(s$client_capabilities$sampling))
  # Same session id is retained across subsequent dispatch.
  mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 2L, method = "tools/list"))
  expect_equal(s$session_id, "session-abc")
})

test_that("negotiated protocol version is echoed back on initialize", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("s", srv, function(e) NULL)
  # Latest version we support.
  r <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1L, method = "initialize",
    params = list(protocolVersion = "2025-06-18",
                  capabilities = list())))
  expect_equal(r$result$protocolVersion, "2025-06-18")
  expect_equal(s$protocol_version, "2025-06-18")
  # An older supported revision should also round-trip.
  s2 <- mcpserver:::Session$new("s2", srv, function(e) NULL)
  r2 <- mcpserver:::route_message(srv, s2, list(
    jsonrpc = "2.0", id = 1L, method = "initialize",
    params = list(protocolVersion = "2024-11-05",
                  capabilities = list())))
  expect_equal(r2$result$protocolVersion, "2024-11-05")
})

test_that("server replies to ping with an empty result", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("s", srv, function(e) NULL)
  r <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1L, method = "ping"))
  expect_equal(r$id, 1L)
  # Empty object result, not [] — clients require {}.
  expect_true(length(r$result) == 0L)
})

test_that("WWW-Authenticate challenge format mirrors what TS clients expect", {
  cfg <- oauth_config(
    issuer = "https://issuer.example",
    audience = "https://api.example",
    jwks_uri = "https://issuer.example/jwks",
    required_scopes = c("mcp:tools"))
  state <- new.env(parent = emptyenv())
  state$auth <- cfg
  # Invalid-token (401) challenge
  challenge_401 <- mcpserver:::www_authenticate_challenge(state,
    error = "invalid_token",
    description = "Bearer token missing")
  expect_match(challenge_401, "Bearer realm=", fixed = TRUE)
  expect_match(challenge_401, 'resource_metadata="https://issuer.example"',
               fixed = TRUE)
  expect_match(challenge_401, 'error="invalid_token"', fixed = TRUE)
  # Insufficient-scope (403) challenge includes the required scopes.
  challenge_403 <- mcpserver:::www_authenticate_challenge(state,
    error = "insufficient_scope",
    description = "more scope please",
    scope = c("mcp:tools"))
  expect_match(challenge_403, 'scope="mcp:tools"', fixed = TRUE)
})

test_that("Last-Event-ID replay returns events strictly after the cursor", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("s", srv, function(e) NULL,
                               max_event_log = 10L)
  ids <- vapply(seq_len(5L), function(i)
    s$record_event(sprintf("payload-%d", i)),
    character(1L))
  replay <- s$replay_after(ids[[3L]])
  payloads <- vapply(replay, function(e) e$payload, character(1L))
  expect_equal(payloads, c("payload-4", "payload-5"))
})
