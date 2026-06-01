# Issue 3: bodyless HTTP responses must advertise Content-Length: 0,
# otherwise nanonext omits the header and HTTP/1.1 keep-alive clients hang
# waiting for a body that never arrives (e.g. the 202 for
# notifications/initialized, 204 for DELETE, admin 204/403).

test_that("bodyless responses get Content-Length: 0", {
  r <- mcpserver:::http_make_response(
    202L, headers = c("Content-Type" = "application/json"))
  expect_equal(unname(r$headers[["Content-Length"]]), "0")
  expect_null(r$body)
})

test_that("empty-string and empty-raw bodies also get Content-Length: 0", {
  r1 <- mcpserver:::http_make_response(202L, body = "")
  expect_equal(unname(r1$headers[["Content-Length"]]), "0")

  r2 <- mcpserver:::http_make_response(204L, body = raw(0))
  expect_equal(unname(r2$headers[["Content-Length"]]), "0")
})

test_that("non-empty bodies do NOT get an auto Content-Length", {
  # nanonext sets Content-Length itself when a body is present; adding our
  # own would risk a duplicate header.
  r <- mcpserver:::http_make_response(
    200L, body = '{"ok":true}', json = TRUE)
  expect_false("Content-Length" %in% names(r$headers))
  expect_equal(r$body, '{"ok":true}')
})

test_that("a caller-supplied Content-Length is preserved, not duplicated", {
  r <- mcpserver:::http_make_response(
    204L, headers = c("Content-Length" = "5"))
  expect_equal(sum(names(r$headers) == "Content-Length"), 1L)
  expect_equal(unname(r$headers[["Content-Length"]]), "5")
})

test_that("json = TRUE still sets Content-Type and, when empty, Content-Length", {
  r <- mcpserver:::http_make_response(202L, json = TRUE)
  expect_equal(unname(r$headers[["Content-Type"]]), "application/json")
  expect_equal(unname(r$headers[["Content-Length"]]), "0")
})
