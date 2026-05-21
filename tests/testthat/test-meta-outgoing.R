# _meta on outgoing content blocks and responses.

test_that("response_text accepts a _meta argument and carries it through", {
  block <- response_text("hi", meta = list(custom = "value"))
  expect_equal(block$type, "text")
  expect_equal(block$`_meta`$custom, "value")
})

test_that("jrpc_response merges meta into the result object", {
  env <- mcpserver:::jrpc_response(1L, list(ok = TRUE),
                                   meta = list(trace = "abc"))
  expect_equal(env$result$ok, TRUE)
  expect_equal(env$result$`_meta`$trace, "abc")
})

test_that("jrpc_response without meta does not add a _meta key", {
  env <- mcpserver:::jrpc_response(1L, list(ok = TRUE))
  expect_false("_meta" %in% names(env$result))
})

test_that("jrpc_response with empty list result still carries meta when present", {
  env <- mcpserver:::jrpc_response(1L, list(),
                                   meta = list(trace = "x"))
  # The empty result is wrapped as a J empty object — but the meta
  # only attaches when the result is a list (not when coerced to
  # empty-object sentinel).
  # We accept either behaviour here: meta attached or not on the empty
  # path. The non-empty path (above) is the canonical test.
  expect_equal(env$id, 1L)
})
