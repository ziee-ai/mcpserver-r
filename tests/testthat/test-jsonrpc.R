test_that("jrpc_kind classifies messages", {
  expect_equal(mcpserver:::jrpc_kind(
    list(jsonrpc = "2.0", id = 1, method = "ping")),
    "request")
  expect_equal(mcpserver:::jrpc_kind(
    list(jsonrpc = "2.0", method = "notifications/initialized")),
    "notification")
  expect_equal(mcpserver:::jrpc_kind(
    list(jsonrpc = "2.0", id = 1, result = list())),
    "response")
  expect_equal(mcpserver:::jrpc_kind(list()), "invalid")
})

test_that("encode/decode round-trip", {
  env <- mcpserver:::jrpc_response(1, list(answer = 42L))
  text <- mcpserver:::jrpc_encode(env)
  expect_type(text, "character")
  parsed <- mcpserver:::jrpc_decode(text)
  expect_equal(parsed$id, 1L)
  expect_equal(parsed$result$answer, 42L)
})

test_that("error envelopes carry MCP codes", {
  e <- mcpserver:::jrpc_error(7, mcpserver:::jrpc_codes$method_not_found,
                              "nope")
  expect_equal(e$error$code, -32601L)
  expect_equal(e$error$message, "nope")
})
