# Ports from packages/core/test/shared/auth.test.ts (SafeUrlSchema)
# and packages/core/test/shared/authUtils.test.ts.

test_that("is_safe_url accepts http and https", {
  expect_true(is_safe_url("https://example.com"))
  expect_true(is_safe_url("http://example.com/x?y=1"))
  expect_true(is_safe_url("https://issuer.example/.well-known/oauth-authorization-server"))
})

test_that("is_safe_url rejects javascript:, data:, file:, about: and friends", {
  expect_false(is_safe_url("javascript:alert(1)"))
  expect_false(is_safe_url("JAVASCRIPT:alert(1)"))
  expect_false(is_safe_url("data:text/plain,hello"))
  expect_false(is_safe_url("vbscript:msgbox(1)"))
  expect_false(is_safe_url("file:///etc/passwd"))
  expect_false(is_safe_url("about:blank"))
})

test_that("is_safe_url rejects non-URL or empty inputs", {
  expect_false(is_safe_url(""))
  expect_false(is_safe_url(NA_character_))
  expect_false(is_safe_url(NULL))
  expect_false(is_safe_url(c("https://a", "https://b")))
  expect_false(is_safe_url("nope no scheme"))
})

test_that("resource_url_from_server_url strips fragment", {
  expect_equal(resource_url_from_server_url("https://api.example/mcp#x"),
               "https://api.example/mcp")
})

test_that("resource_url_from_server_url is a no-op when no fragment", {
  expect_equal(resource_url_from_server_url("https://api.example/mcp"),
               "https://api.example/mcp")
})

test_that("resource_matches: identity with and without fragment", {
  expect_true(resource_matches("https://a/x", "https://a/x"))
  expect_true(resource_matches("https://a/x", "https://a/x#frag"))
})

test_that("resource_matches: differs by path / port / domain", {
  expect_false(resource_matches("https://a/x", "https://a/y"))
  expect_false(resource_matches("https://a/x", "https://b/x"))
  expect_false(resource_matches("https://a:443/x", "https://a:8443/x"))
})

test_that("resource_matches: trailing slash is significant", {
  expect_false(resource_matches("https://a/x", "https://a/x/"))
})
