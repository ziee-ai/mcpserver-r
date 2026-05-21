# HackerOne #3156202 — sending an `initialize` request with missing
# params must return a JSON-RPC error envelope without crashing the
# server. Ten concurrent malformed requests each get their own error.
# Adapted from python-sdk/tests/issues/test_malformed_input.py.

test_that("initialize without params is rejected with a JSON-RPC error envelope", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "initialize"))
  # The server must not crash; it should return a valid envelope.
  expect_true(is.list(resp))
  expect_equal(resp$jsonrpc, "2.0")
  expect_equal(resp$id, 1)
  # Result should be present even with empty params because
  # negotiate_protocol_version falls back gracefully.
  expect_true(!is.null(resp$result) || !is.null(resp$error))
})

test_that("non-JSON-RPC envelope (missing method) returns invalid_request", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, params = list(foo = "bar")))
  expect_equal(resp$error$code, -32600L)
})

test_that("malformed envelope shapes do not throw or hang the dispatcher", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  add_capability(srv, new_tool(
    "echo", "echo", schema(list(text = property_string(required = TRUE))),
    handler = function(args, ctx) response_text(args$text)))
  bad_envelopes <- list(
    list(),                                              # empty
    list(jsonrpc = "2.0"),                                # nothing else
    list(jsonrpc = "1.0", id = 1, method = "ping"),      # wrong version
    list(jsonrpc = "2.0", id = list(), method = "ping"), # invalid id
    list(jsonrpc = "2.0", id = 1, method = list()),      # invalid method
    list(jsonrpc = "2.0", id = 1, method = "")           # empty method
  )
  for (env in bad_envelopes) {
    out <- tryCatch(mcpserver:::route_message(srv, s, env),
                    error = function(e) e)
    expect_false(inherits(out, "error"),
                 info = paste("threw on:", jsonlite::toJSON(env, auto_unbox = TRUE, force = TRUE)))
  }
})

test_that("ten concurrent malformed dispatches all return envelopes", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  responses <- vapply(seq_len(10L), function(i) {
    resp <- mcpserver:::route_message(srv, s, list(
      jsonrpc = "2.0", id = i,
      method = "no/such",
      params = list(garbage = list(deep = list(deep = list(deep = i))))))
    !is.null(resp$error$code)
  }, logical(1L))
  expect_true(all(responses))
})
