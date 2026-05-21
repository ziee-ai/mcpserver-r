# /jwks endpoint shape + integration with the resource-server verifier.

test_that("/jwks returns a JWKS document with a single RS256 key", {
  cfg <- oauth_server_config(
    issuer = "https://as.example",
    audience = "https://mcp.example",
    kid = "spec-kid-1")
  resp <- mcpserver:::oauth_as_jwks_handler(cfg)(list())
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(resp$status, 200L)
  expect_length(body$keys, 1L)
  jwk <- body$keys[[1L]]
  expect_equal(jwk$kid, "spec-kid-1")
  expect_equal(jwk$use, "sig")
  expect_equal(jwk$alg, "RS256")
  expect_equal(jwk$kty, "RSA")
  expect_true(nzchar(jwk$n))
  expect_true(nzchar(jwk$e))
})

test_that("an AS-minted access token verifies through the matching oauth_config", {
  cfg <- oauth_server_config(
    issuer = "https://as.example",
    audience = "https://mcp.example",
    subject = "user-1")
  rs <- mcpserver:::oauth_config_from_server(cfg)
  tok <- mcpserver:::oauth_as_mint_jwt(cfg,
    list(subject = "user-1", scopes = c("mcp:read")),
    cfg$ttl_access, kind = "access")
  res <- mcpserver:::oauth_verify_jwt(rs, tok)
  expect_true(isTRUE(res$ok))
  expect_equal(res$subject, "user-1")
  expect_setequal(res$scopes, "mcp:read")
})

test_that("an AS-minted token with insufficient scope is rejected", {
  cfg <- oauth_server_config(
    issuer = "https://as.example",
    audience = "https://mcp.example")
  rs <- mcpserver:::oauth_config_from_server(cfg,
    required_scopes = c("mcp:write"))
  tok <- mcpserver:::oauth_as_mint_jwt(cfg,
    list(subject = "u", scopes = c("mcp:read")),
    cfg$ttl_access)
  res <- mcpserver:::oauth_verify_jwt(rs, tok)
  expect_false(isTRUE(res$ok))
  expect_equal(res$reason, "insufficient_scope")
})

test_that("tokens minted against a different issuer fail aud/iss checks", {
  cfg <- oauth_server_config(
    issuer = "https://as.example",
    audience = "https://mcp.example")
  other_cfg <- oauth_server_config(
    issuer = "https://different.example",
    audience = "https://mcp.example")
  rs <- mcpserver:::oauth_config_from_server(cfg)
  tok <- mcpserver:::oauth_as_mint_jwt(other_cfg,
    list(subject = "u", scopes = c("mcp:read")),
    other_cfg$ttl_access)
  res <- mcpserver:::oauth_verify_jwt(rs, tok)
  expect_false(isTRUE(res$ok))
})
