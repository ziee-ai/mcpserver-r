# Ports from packages/core/test/types.test.ts ContentBlock coverage.

test_that("response_text returns a valid TextContent block", {
  block <- response_text("hello")
  expect_equal(block$type, "text")
  expect_equal(block$text, "hello")
})

test_that("response_image carries a mimeType and base64 data", {
  block <- response_image(charToRaw("not actually an image"),
                          "image/png")
  expect_equal(block$type, "image")
  expect_equal(block$mimeType, "image/png")
  expect_type(block$data, "character")
})

test_that("response_audio carries a mimeType and base64 data", {
  block <- response_audio(charToRaw("not actually audio"),
                          "audio/wav")
  expect_equal(block$type, "audio")
  expect_equal(block$mimeType, "audio/wav")
  expect_type(block$data, "character")
})

test_that("response_resource_link is a minimal valid block", {
  block <- response_resource_link("test://x")
  expect_equal(block$type, "resource_link")
  expect_equal(block$uri, "test://x")
})

test_that("response_resource_link with all optional fields round-trips", {
  block <- response_resource_link("test://x",
                                  name = "X",
                                  description = "A test resource",
                                  mime_type = "text/plain")
  expect_equal(block$name, "X")
  expect_equal(block$description, "A test resource")
  expect_equal(block$mimeType, "text/plain")
})

test_that("response_resource embeds either text or blob with a uri", {
  text_block <- response_resource("test://t", text = "hi",
                                   mime_type = "text/plain")
  expect_equal(text_block$type, "resource")
  expect_equal(text_block$resource$uri, "test://t")
  expect_equal(text_block$resource$text, "hi")

  blob_block <- response_resource("test://b",
                                  blob = charToRaw("xyz"),
                                  mime_type = "application/octet-stream")
  expect_equal(blob_block$type, "resource")
  expect_type(blob_block$resource$blob, "character")
})

test_that("CallToolResult shape: content array of valid blocks", {
  result <- list(content = list(
    response_text("hi"),
    response_image(charToRaw("x"), "image/png")
  ), isError = FALSE)
  expect_true(is_call_tool_result(result))
})

test_that("normalize_tool_result wraps a bare string", {
  r <- mcpserver:::normalize_tool_result("plain")
  expect_equal(r$content[[1L]]$type, "text")
  expect_equal(r$content[[1L]]$text, "plain")
})

test_that("normalize_tool_result preserves a list of content blocks", {
  r <- mcpserver:::normalize_tool_result(list(
    response_text("a"), response_text("b")))
  expect_equal(length(r$content), 2L)
})

test_that("response_error sets isError TRUE and wraps a text block", {
  r <- response_error("boom")
  expect_true(isTRUE(r$isError))
  expect_equal(r$content[[1L]]$type, "text")
})

test_that("response_structured carries both content and structuredContent", {
  r <- response_structured(
    list(response_text("ok")),
    list(value = 42L))
  expect_equal(r$content[[1L]]$type, "text")
  expect_equal(r$structuredContent$value, 42L)
})
