# Issuer URL validation per RFC 8414 §3 (with RFC 8252 loopback
# exception for HTTP).

test_that("issuer = HTTPS URL is accepted", {
  expect_silent(oauth_server_config(
    issuer = "https://as.example",
    audience = "https://api.example"))
})

test_that("issuer = http://localhost is accepted (RFC 8252 loopback)", {
  expect_silent(oauth_server_config(
    issuer = "http://localhost:9090",
    audience = "http://localhost:9090/mcp"))
})

test_that("issuer = http://127.0.0.1 is accepted", {
  expect_silent(oauth_server_config(
    issuer = "http://127.0.0.1:44400",
    audience = "http://127.0.0.1:44400/mcp"))
})

test_that("issuer = http://[::1] is accepted", {
  expect_silent(oauth_server_config(
    issuer = "http://[::1]:44400",
    audience = "http://[::1]:44400/mcp"))
})

test_that("issuer = http://non-loopback is rejected", {
  expect_error(oauth_server_config(
    issuer = "http://as.example",
    audience = "https://api.example"),
    "HTTPS")
})

test_that("issuer with fragment is rejected", {
  expect_error(oauth_server_config(
    issuer = "https://as.example#frag",
    audience = "https://api.example"),
    "fragment")
})

test_that("issuer with query string is rejected", {
  expect_error(oauth_server_config(
    issuer = "https://as.example?foo=bar",
    audience = "https://api.example"),
    "query")
})

test_that("issuer without scheme is rejected", {
  expect_error(oauth_server_config(
    issuer = "as.example",
    audience = "https://api.example"),
    "http")
})

test_that("trailing slash is stripped before validation", {
  expect_silent(oauth_server_config(
    issuer = "https://as.example/",
    audience = "https://api.example"))
  expect_silent(oauth_server_config(
    issuer = "https://as.example///",
    audience = "https://api.example"))
})
