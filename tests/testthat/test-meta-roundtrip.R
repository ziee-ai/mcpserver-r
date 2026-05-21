# Ports the `_meta` round-trip assertions from protocol.test.ts.

test_that("incoming _meta.progressToken reaches ctx$progress_token", {
  srv <- new_server("t")
  msg <- list(jsonrpc = "2.0", id = 1L, method = "tools/call",
              params = list(name = "echo",
                            arguments = list(text = "x"),
                            `_meta` = list(progressToken = "pt-1")))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  ctx <- mcpserver:::make_ctx(s, msg)
  expect_equal(ctx$progress_token, "pt-1")
})

test_that("arbitrary _meta keys reach ctx$msg_meta", {
  srv <- new_server("t")
  msg <- list(jsonrpc = "2.0", id = 1L, method = "tools/call",
              params = list(name = "x", arguments = list(),
                            `_meta` = list(progressToken = 7L,
                                           customKey = "value",
                                           nested = list(deep = TRUE))))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  ctx <- mcpserver:::make_ctx(s, msg)
  expect_equal(ctx$msg_meta$customKey, "value")
  expect_true(isTRUE(ctx$msg_meta$nested$deep))
  # progressToken stays exposed both as a top-level convenience and
  # as part of the full meta blob.
  expect_equal(ctx$msg_meta$progressToken, 7L)
})

test_that("missing _meta leaves both progress_token and msg_meta NULL", {
  srv <- new_server("t")
  msg <- list(jsonrpc = "2.0", id = 1L, method = "tools/list",
              params = list())
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  ctx <- mcpserver:::make_ctx(s, msg)
  expect_null(ctx$progress_token)
  expect_null(ctx$msg_meta)
})
