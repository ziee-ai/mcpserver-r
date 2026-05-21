# Ports from packages/core/test/types/guards.test.ts

test_that("is_jsonrpc_response accepts well-formed result and error envelopes", {
  expect_true(is_jsonrpc_response(list(
    jsonrpc = "2.0", id = 1L, result = list(ok = TRUE))))
  expect_true(is_jsonrpc_response(list(
    jsonrpc = "2.0", id = "abc", error = list(code = -32601L,
                                              message = "x"))))
})

test_that("is_jsonrpc_response rejects requests and notifications", {
  expect_false(is_jsonrpc_response(list(
    jsonrpc = "2.0", id = 1L, method = "ping")))
  expect_false(is_jsonrpc_response(list(
    jsonrpc = "2.0", method = "notifications/x")))
})

test_that("is_jsonrpc_response rejects envelopes with both result and error", {
  expect_false(is_jsonrpc_response(list(
    jsonrpc = "2.0", id = 1L,
    result = list(), error = list(code = -1, message = "x"))))
})

test_that("is_jsonrpc_response rejects non-2.0 envelopes", {
  expect_false(is_jsonrpc_response(list(
    jsonrpc = "1.0", id = 1L, result = list())))
})

test_that("is_jsonrpc_response rejects non-list inputs", {
  expect_false(is_jsonrpc_response("not a list"))
  expect_false(is_jsonrpc_response(NULL))
  expect_false(is_jsonrpc_response(42))
})

test_that("is_call_tool_result accepts a non-empty content array", {
  expect_true(is_call_tool_result(list(
    content = list(list(type = "text", text = "ok")))))
  expect_true(is_call_tool_result(list(
    content = list(list(type = "text", text = "a"),
                   list(type = "image", data = "...",
                        mimeType = "image/png")),
    isError = FALSE)))
})

test_that("is_call_tool_result accepts structuredContent alongside content", {
  expect_true(is_call_tool_result(list(
    content = list(list(type = "text", text = "ok")),
    structuredContent = list(answer = 42L))))
})

test_that("is_call_tool_result rejects empty object and missing content", {
  expect_false(is_call_tool_result(list()))
  expect_false(is_call_tool_result(list(isError = FALSE)))
})

test_that("is_call_tool_result rejects content items with no type", {
  expect_false(is_call_tool_result(list(
    content = list(list(text = "no type")))))
  expect_false(is_call_tool_result(list(
    content = list(list(type = 42L)))))
})

test_that("is_call_tool_result rejects non-list inputs", {
  expect_false(is_call_tool_result("nope"))
  expect_false(is_call_tool_result(NULL))
  expect_false(is_call_tool_result(list(content = "not a list")))
})
