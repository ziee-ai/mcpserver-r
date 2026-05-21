# RFC 8414 authorization-server metadata document.

new_test_as <- function() {
  oauth_server_config(
    issuer = "https://as.example",
    audience = "https://mcp.example",
    scopes_supported = c("mcp:read", "mcp:write"))
}

decode <- function(resp) {
  jsonlite::fromJSON(rawToChar(charToRaw(resp$body)),
                     simplifyVector = FALSE)
}

test_that("metadata exposes the five required endpoints", {
  cfg <- new_test_as()
  resp <- mcpserver:::oauth_as_metadata_handler(cfg)(list())
  body <- decode(resp)
  expect_equal(resp$status, 200L)
  expect_equal(body$issuer, "https://as.example")
  expect_equal(body$authorization_endpoint,
               "https://as.example/authorize")
  expect_equal(body$token_endpoint, "https://as.example/token")
  expect_equal(body$registration_endpoint,
               "https://as.example/register")
  expect_equal(body$jwks_uri, "https://as.example/jwks")
})

test_that("metadata advertises PKCE-S256, code, and pub-client auth", {
  cfg <- new_test_as()
  resp <- mcpserver:::oauth_as_metadata_handler(cfg)(list())
  body <- decode(resp)
  expect_equal(unlist(body$response_types_supported), "code")
  expect_setequal(unlist(body$grant_types_supported),
                  c("authorization_code", "refresh_token"))
  expect_equal(unlist(body$code_challenge_methods_supported), "S256")
  expect_equal(unlist(body$token_endpoint_auth_methods_supported),
               "none")
})

test_that("metadata reflects the configured scopes_supported", {
  cfg <- oauth_server_config(
    issuer = "https://as.example",
    audience = "https://mcp.example",
    scopes_supported = c("a:read", "b:write", "c:admin"))
  resp <- mcpserver:::oauth_as_metadata_handler(cfg)(list())
  body <- decode(resp)
  expect_setequal(unlist(body$scopes_supported),
                  c("a:read", "b:write", "c:admin"))
})

test_that("trailing slashes on issuer are normalised", {
  cfg <- oauth_server_config(
    issuer = "https://as.example/",
    audience = "https://mcp.example")
  resp <- mcpserver:::oauth_as_metadata_handler(cfg)(list())
  body <- decode(resp)
  expect_equal(body$issuer, "https://as.example")
  expect_false(grepl("/$", body$issuer))
})

test_that("Content-Type is application/json on the metadata response", {
  cfg <- new_test_as()
  resp <- mcpserver:::oauth_as_metadata_handler(cfg)(list())
  expect_match(paste(resp$headers, collapse = ","),
               "application/json")
})
