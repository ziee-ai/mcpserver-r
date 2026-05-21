# SEP-1330 enum shapes: server validates incoming elicitation responses
# against the five enum variants.

test_that("untitled single-select enum accepts allowed values", {
  rs <- list(type = "object", properties = list(
    pick = list(type = "string", enum = c("a", "b", "c"))))
  expect_true(mcpserver:::validate_elicit_response(
    rs, list(action = "accept",
             content = list(pick = "b")))$ok)
  expect_false(mcpserver:::validate_elicit_response(
    rs, list(action = "accept",
             content = list(pick = "z")))$ok)
})

test_that("titled single-select (oneOf+const+title) accepts matching const", {
  rs <- list(type = "object", properties = list(
    pick = list(type = "string",
                oneOf = list(
                  list(const = "v1", title = "First"),
                  list(const = "v2", title = "Second")))))
  expect_true(mcpserver:::validate_elicit_response(
    rs, list(action = "accept",
             content = list(pick = "v1")))$ok)
  expect_false(mcpserver:::validate_elicit_response(
    rs, list(action = "accept",
             content = list(pick = "wrong")))$ok)
})

test_that("legacy titled enum (enum + enumNames) still validates against the enum", {
  rs <- list(type = "object", properties = list(
    pick = list(type = "string",
                enum = c("o1", "o2"),
                enumNames = c("One", "Two"))))
  expect_true(mcpserver:::validate_elicit_response(
    rs, list(action = "accept",
             content = list(pick = "o1")))$ok)
  expect_false(mcpserver:::validate_elicit_response(
    rs, list(action = "accept",
             content = list(pick = "missing")))$ok)
})

test_that("untitled multi-select (items.enum) accepts subsets only", {
  rs <- list(type = "object", properties = list(
    picks = list(type = "array",
                 items = list(type = "string",
                              enum = c("a", "b", "c")))))
  expect_true(mcpserver:::validate_elicit_response(
    rs, list(action = "accept",
             content = list(picks = c("a", "b"))))$ok)
  expect_false(mcpserver:::validate_elicit_response(
    rs, list(action = "accept",
             content = list(picks = c("a", "z"))))$ok)
})

test_that("titled multi-select (items.anyOf+const+title) accepts subsets only", {
  rs <- list(type = "object", properties = list(
    picks = list(type = "array",
                 items = list(
                   anyOf = list(
                     list(const = "v1", title = "First"),
                     list(const = "v2", title = "Second"))))))
  expect_true(mcpserver:::validate_elicit_response(
    rs, list(action = "accept",
             content = list(picks = c("v1", "v2"))))$ok)
  expect_false(mcpserver:::validate_elicit_response(
    rs, list(action = "accept",
             content = list(picks = c("v1", "wrong"))))$ok)
})
