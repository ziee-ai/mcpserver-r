test_that("response_text builds text content blocks", {
  out <- response_text("hi")
  expect_equal(out$type, "text")
  expect_equal(out$text, "hi")
})

test_that("response_image base64-encodes raw bytes", {
  raw <- charToRaw("abc")
  out <- response_image(raw, "image/png")
  expect_equal(out$type, "image")
  expect_type(out$data, "character")
  expect_equal(out$mimeType, "image/png")
})

test_that("response_resource_link emits a resource_link block", {
  out <- response_resource_link("demo://x", name = "x")
  expect_equal(out$type, "resource_link")
  expect_equal(out$uri, "demo://x")
  expect_equal(out$name, "x")
})

test_that("response_error sets isError and wraps message", {
  out <- response_error("boom")
  expect_true(isTRUE(out$isError))
  expect_equal(out$content[[1L]]$text, "boom")
})

test_that("normalize_tool_result handles common return shapes", {
  expect_equal(mcpserver:::normalize_tool_result("plain")$content[[1L]]$text,
               "plain")
  expect_equal(
    mcpserver:::normalize_tool_result(response_text("x"))$content[[1L]]$type,
    "text"
  )
})
