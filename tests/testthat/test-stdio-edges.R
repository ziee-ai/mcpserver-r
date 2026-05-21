# Ports core/test/shared/stdio.test.ts assertions about line-buffered
# NDJSON parsing edge cases.

test_that("jrpc_decode handles a valid single-line envelope", {
  out <- mcpserver:::jrpc_decode('{"jsonrpc":"2.0","id":1,"method":"ping"}')
  expect_equal(out$id, 1L)
  expect_equal(out$method, "ping")
})

test_that("jrpc_decode returns NULL on unbalanced braces", {
  expect_null(mcpserver:::jrpc_decode('{"jsonrpc":"2.0","id":1'))
  expect_null(mcpserver:::jrpc_decode('{"jsonrpc":"2.0","id":1}}'))
})

test_that("jrpc_decode returns NULL on a non-JSON line", {
  expect_null(mcpserver:::jrpc_decode("not json"))
  expect_null(mcpserver:::jrpc_decode("   "))
})

test_that("jrpc_decode tolerates JSON with leading/trailing whitespace", {
  out <- mcpserver:::jrpc_decode('   {"jsonrpc":"2.0","id":1,"method":"ping"}  ')
  expect_equal(out$method, "ping")
})

test_that("jrpc_decode returns NULL on a partial envelope missing closing", {
  expect_null(mcpserver:::jrpc_decode('{"jsonrpc":"2.0","method":"p"'))
})

test_that("jrpc_encode round-trips a result envelope", {
  env <- mcpserver:::jrpc_response(1L, list(answer = 42L))
  text <- mcpserver:::jrpc_encode(env)
  back <- mcpserver:::jrpc_decode(text)
  expect_equal(back$id, 1L)
  expect_equal(back$result$answer, 42L)
})

test_that("jrpc_encode round-trips an error envelope with data field", {
  env <- mcpserver:::jrpc_error(1L,
                                mcp_error_codes()$invalid_params,
                                "bad input",
                                data = list(arg = "x"))
  text <- mcpserver:::jrpc_encode(env)
  back <- mcpserver:::jrpc_decode(text)
  expect_equal(back$error$code, -32602L)
  expect_equal(back$error$data$arg, "x")
})

test_that("jrpc_decode parses a batch (top-level array) into a list of envelopes", {
  out <- mcpserver:::jrpc_decode(
    '[{"jsonrpc":"2.0","id":1,"method":"ping"},
       {"jsonrpc":"2.0","id":2,"method":"ping"}]')
  expect_true(is.list(out) && is.null(names(out)))
  expect_equal(length(out), 2L)
})

test_that("jrpc_decode rejects an envelope with non-string method", {
  out <- mcpserver:::jrpc_decode(
    '{"jsonrpc":"2.0","id":1,"method":["a","b"]}')
  # Decoded but classified invalid (method is an array)
  expect_equal(mcpserver:::jrpc_kind(out), "invalid")
})
