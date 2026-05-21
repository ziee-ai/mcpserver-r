# Pluggable validator tests.

test_that("new_validator(engine='ajv') returns a working validator", {
  v <- new_validator()
  check <- v(schema(list(x = property_integer(required = TRUE))))
  expect_true(check(list(x = 42L))$ok)
  expect_false(check(list())$ok)
})

test_that("new_validator(engine='none') accepts everything (stub)", {
  v <- new_validator(engine = "none")
  check <- v(schema(list(x = property_integer(required = TRUE))))
  expect_true(check(list())$ok)
  expect_true(check(list(x = "not-int"))$ok)
})

test_that("a custom validator can be plugged into new_server", {
  always_fail <- function(sch)
    function(args) list(ok = FALSE, errors = "custom")
  srv <- new_server("t", schema_validator = always_fail)
  expect_identical(srv$schema_validator, always_fail)
})
