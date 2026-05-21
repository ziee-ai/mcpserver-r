# Integration-style coverage of validate_elicit_response across the
# spec's primitive field types and constraint combinations.

valid <- function(rs, value) {
  out <- mcpserver:::validate_elicit_response(
    rs, list(action = "accept", content = value))
  out$ok
}

errors_of <- function(rs, value) {
  out <- mcpserver:::validate_elicit_response(
    rs, list(action = "accept", content = value))
  out$errors
}

test_that("string field round-trips with no constraints", {
  rs <- list(type = "object",
             properties = list(name = list(type = "string")))
  expect_true(valid(rs, list(name = "Alice")))
  expect_false(valid(rs, list(name = 42L)))
})

test_that("integer field rejects non-integer numerics and strings", {
  rs <- list(type = "object",
             properties = list(age = list(type = "integer")))
  expect_true(valid(rs, list(age = 30L)))
  expect_false(valid(rs, list(age = 3.5)))
  expect_false(valid(rs, list(age = "30")))
})

test_that("number field accepts integers and floats", {
  rs <- list(type = "object",
             properties = list(score = list(type = "number")))
  expect_true(valid(rs, list(score = 7L)))
  expect_true(valid(rs, list(score = 3.14)))
  expect_false(valid(rs, list(score = "high")))
})

test_that("boolean field rejects strings and numbers", {
  rs <- list(type = "object",
             properties = list(yes = list(type = "boolean")))
  expect_true(valid(rs, list(yes = TRUE)))
  expect_true(valid(rs, list(yes = FALSE)))
  expect_false(valid(rs, list(yes = "true")))
  expect_false(valid(rs, list(yes = 1L)))
})

test_that("optional fields are tolerated when missing", {
  rs <- list(type = "object",
             properties = list(opt = list(type = "string")))
  expect_true(valid(rs, list()))
})

test_that("URI format validates a basic scheme", {
  rs <- list(type = "object",
             properties = list(u = list(type = "string",
                                        format = "uri")))
  expect_true(valid(rs, list(u = "https://a.example/x")))
  expect_false(valid(rs, list(u = "not a uri")))
})

test_that("date format requires YYYY-MM-DD", {
  rs <- list(type = "object",
             properties = list(d = list(type = "string",
                                        format = "date")))
  expect_true(valid(rs, list(d = "2026-05-20")))
  expect_false(valid(rs, list(d = "May 20, 2026")))
})

test_that("date-time format requires ISO 8601", {
  rs <- list(type = "object",
             properties = list(d = list(type = "string",
                                        format = "date-time")))
  expect_true(valid(rs, list(d = "2026-05-20T12:34:56Z")))
  expect_false(valid(rs, list(d = "2026-05-20")))
})

test_that("range constraints stack with type", {
  rs <- list(type = "object",
             properties = list(n = list(type = "integer",
                                        minimum = 0L, maximum = 100L)))
  expect_true(valid(rs, list(n = 50L)))
  expect_false(valid(rs, list(n = -1L)))
  expect_false(valid(rs, list(n = 101L)))
})

test_that("multiple errors surface when several fields are wrong", {
  rs <- list(type = "object",
             properties = list(
               a = list(type = "integer", minimum = 0L),
               b = list(type = "string", maxLength = 3L)))
  errs <- errors_of(rs, list(a = -5L, b = "longstring"))
  expect_true(length(errs) >= 2L)
})
