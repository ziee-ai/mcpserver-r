# JSON Schema `pattern` regex enforcement on elicitation string fields.
# Mirrors TS SDK elicitation.test.ts > "should reject invalid pattern".

accept_with <- function(rs, content) {
  mcpserver:::validate_elicit_response(rs,
    list(action = "accept", content = content))
}

test_that("pattern accepts strings matching the regex", {
  rs <- list(type = "object",
             properties = list(
               username = list(type = "string",
                               pattern = "^[a-z_][a-z0-9_]{2,15}$")))
  out <- accept_with(rs, list(username = "alice"))
  expect_true(out$ok)
})

test_that("pattern rejects strings not matching the regex", {
  rs <- list(type = "object",
             properties = list(
               username = list(type = "string",
                               pattern = "^[a-z_][a-z0-9_]{2,15}$")))
  out <- accept_with(rs, list(username = "ALICE!"))
  expect_false(out$ok)
  expect_match(paste(out$errors, collapse = " "), "does not match pattern")
})

test_that("pattern coexists with format validators", {
  rs <- list(type = "object",
             properties = list(
               code = list(type = "string",
                           format = "email",
                           pattern = "@example\\.com$")))
  # Wrong domain — fails pattern even though format would accept it.
  out <- accept_with(rs, list(code = "user@gmail.com"))
  expect_false(out$ok)
})

test_that("pattern is skipped for non-string fields", {
  rs <- list(type = "object",
             properties = list(
               n = list(type = "integer", pattern = "irrelevant")))
  out <- accept_with(rs, list(n = 42L))
  expect_true(out$ok)
})

test_that("pattern is enforced on array items via items.pattern", {
  rs <- list(type = "object",
             properties = list(
               tags = list(type = "array",
                           items = list(type = "string",
                                        pattern = "^[a-z]+$"))))
  ok <- accept_with(rs, list(tags = list("alpha", "beta")))
  expect_true(ok$ok)
})
