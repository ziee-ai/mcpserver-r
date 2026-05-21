# Coverage tests for every property_* helper.

test_that("property_string honours format and pattern + array enum", {
  p <- property_string(description = "n", min_length = 1L,
                       max_length = 8L, pattern = "^[a-z]+$",
                       format = "email", enum = c("a", "b"),
                       required = TRUE)
  expect_equal(p$type, "string")
  expect_equal(p$minLength, 1L)
  expect_equal(p$maxLength, 8L)
  expect_equal(p$pattern, "^[a-z]+$")
  expect_equal(p$format, "email")
  expect_true(isTRUE(p$.required))
  expect_true(inherits(p$enum, "AsIs"))  # forced JSON array
})

test_that("property_number / property_integer carry bounds", {
  pn <- property_number(minimum = 0, maximum = 1)
  expect_equal(pn$type, "number")
  expect_equal(pn$minimum, 0)
  expect_equal(pn$maximum, 1)
  pi <- property_integer(minimum = 1L, maximum = 10L)
  expect_equal(pi$type, "integer")
})

test_that("property_boolean has type boolean and optional default", {
  pb <- property_boolean(default = TRUE)
  expect_equal(pb$type, "boolean")
  expect_true(isTRUE(pb$default))
})

test_that("property_array wraps items + bounds", {
  pa <- property_array(property_string(), min_items = 1L,
                       max_items = 3L)
  expect_equal(pa$type, "array")
  expect_equal(pa$items$type, "string")
  expect_equal(pa$minItems, 1L)
  expect_equal(pa$maxItems, 3L)
})

test_that("property_object encodes nested object schema", {
  po <- property_object(list(
    n = property_string(required = TRUE)
  ), description = "obj")
  expect_equal(po$type, "object")
  expect_true("n" %in% as.character(po$required))
})

test_that("property_enum builds a string with enum array", {
  pe <- property_enum(c("low", "high"), default = "low")
  expect_equal(pe$type, "string")
  expect_true(inherits(pe$enum, "AsIs"))
  expect_equal(pe$default, "low")
})

test_that("schema rejects with informative errors", {
  s <- schema(list(
    n = property_integer(minimum = 0L, required = TRUE)
  ))
  v <- mcpserver:::validate_args(s, list(n = -5L))
  expect_false(v$ok)
  expect_true(length(v$errors) >= 1L)
  v2 <- mcpserver:::validate_args(s, list())
  expect_false(v2$ok)
})
