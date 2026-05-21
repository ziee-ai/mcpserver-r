# SEP-1034 defaults: when the elicitation response is missing a field
# that the requestedSchema declares with a `default`, the default is
# applied before the value reaches the tool handler.

test_that("SEP-1034: missing fields are populated from schema defaults", {
  rs <- list(type = "object",
             properties = list(
               name = list(type = "string", default = "John Doe"),
               age = list(type = "integer", default = 30L),
               score = list(type = "number", default = 95.5),
               verified = list(type = "boolean", default = TRUE)))
  res <- list(action = "accept", content = list(name = "Alice"))
  out <- mcpserver:::validate_elicit_response(rs, res)
  expect_true(out$ok)
  expect_equal(out$value$content$name, "Alice")
  expect_equal(out$value$content$age, 30L)
  expect_equal(out$value$content$score, 95.5)
  expect_true(isTRUE(out$value$content$verified))
})

test_that("SEP-1034: defaults are not applied when the field is present", {
  rs <- list(type = "object",
             properties = list(
               status = list(type = "string", default = "active",
                             enum = c("active", "inactive"))))
  res <- list(action = "accept",
              content = list(status = "inactive"))
  out <- mcpserver:::validate_elicit_response(rs, res)
  expect_equal(out$value$content$status, "inactive")
})

test_that("decline / cancel actions skip validation", {
  rs <- list(type = "object",
             properties = list(
               name = list(type = "string")),
             required = c("name"))
  for (action in c("decline", "cancel")) {
    res <- list(action = action)
    out <- mcpserver:::validate_elicit_response(rs, res)
    expect_true(out$ok)
  }
})

test_that("missing required field without default is rejected", {
  rs <- list(type = "object",
             properties = list(name = list(type = "string")),
             required = c("name"))
  res <- list(action = "accept", content = list())
  out <- mcpserver:::validate_elicit_response(rs, res)
  expect_false(out$ok)
  expect_match(out$errors[[1L]], "missing required field 'name'")
})

test_that("type mismatch is rejected with a descriptive error", {
  rs <- list(type = "object",
             properties = list(age = list(type = "integer")))
  res <- list(action = "accept", content = list(age = "not-an-int"))
  out <- mcpserver:::validate_elicit_response(rs, res)
  expect_false(out$ok)
  expect_match(out$errors[[1L]], "must be an integer")
})

test_that("email format rejects malformed inputs", {
  rs <- list(type = "object",
             properties = list(addr = list(type = "string",
                                           format = "email")))
  expect_true(mcpserver:::validate_elicit_response(
    rs, list(action = "accept",
             content = list(addr = "a@b.co")))$ok)
  expect_false(mcpserver:::validate_elicit_response(
    rs, list(action = "accept",
             content = list(addr = "not-an-email")))$ok)
})

test_that("integer range enforces minimum / maximum", {
  rs <- list(type = "object",
             properties = list(n = list(type = "integer",
                                        minimum = 1L,
                                        maximum = 10L)))
  expect_true(mcpserver:::validate_elicit_response(
    rs, list(action = "accept", content = list(n = 5L)))$ok)
  expect_false(mcpserver:::validate_elicit_response(
    rs, list(action = "accept", content = list(n = 0L)))$ok)
  expect_false(mcpserver:::validate_elicit_response(
    rs, list(action = "accept", content = list(n = 99L)))$ok)
})

test_that("string length bounds are enforced", {
  rs <- list(type = "object",
             properties = list(name = list(type = "string",
                                           minLength = 2L,
                                           maxLength = 5L)))
  expect_true(mcpserver:::validate_elicit_response(
    rs, list(action = "accept", content = list(name = "abc")))$ok)
  expect_false(mcpserver:::validate_elicit_response(
    rs, list(action = "accept", content = list(name = "a")))$ok)
  expect_false(mcpserver:::validate_elicit_response(
    rs, list(action = "accept", content = list(name = "abcdef")))$ok)
})
