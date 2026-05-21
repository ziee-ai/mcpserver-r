# Extra response constructors not previously covered.

test_that("response_audio emits a base64-encoded audio block", {
  out <- response_audio(charToRaw("not actually audio"), "audio/mpeg")
  expect_equal(out$type, "audio")
  expect_type(out$data, "character")
  expect_equal(out$mimeType, "audio/mpeg")
})

test_that("response_video emits a video block with mimeType", {
  out <- response_video(charToRaw("not actually video"), "video/mp4")
  expect_equal(out$type, "video")
  expect_equal(out$mimeType, "video/mp4")
})

test_that("response_file reads file bytes and infers MIME", {
  tmp <- tempfile(fileext = ".txt")
  writeLines("hi", tmp)
  withr::defer(unlink(tmp))
  out <- response_file(tmp)
  expect_equal(out$type, "resource")
  expect_true(startsWith(out$resource$mimeType, "text/"))
  expect_true(nzchar(out$resource$blob))
})

test_that("response_structured carries both content and structuredContent", {
  out <- response_structured(
    list(response_text("ok")),
    list(value = 42L))
  expect_equal(out$content[[1L]]$type, "text")
  expect_equal(out$structuredContent$value, 42L)
})
