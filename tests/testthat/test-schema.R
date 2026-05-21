test_that("schema builders produce JSON-Schema shaped lists", {
  s <- schema(list(
    name = property_string("Name", required = TRUE),
    age  = property_integer("Age", minimum = 0)
  ))
  expect_equal(s$type, "object")
  expect_true("name" %in% names(s$properties))
  expect_true("required" %in% names(s))
  expect_true("name" %in% as.character(s$required))
  expect_false(s$additionalProperties)
})

test_that("required is preserved when single", {
  s <- schema(list(text = property_string(required = TRUE)))
  expect_true(identical(as.character(s$required), "text"))
})

test_that("validate_args accepts valid input", {
  s <- schema(list(x = property_integer(required = TRUE)))
  r <- mcpserver:::validate_args(s, list(x = 42L))
  expect_true(r$ok)
})

test_that("validate_args rejects missing required", {
  s <- schema(list(x = property_integer(required = TRUE)))
  r <- mcpserver:::validate_args(s, list())
  expect_false(r$ok)
  expect_true(length(r$errors) >= 1L)
})
