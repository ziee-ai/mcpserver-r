# Ports assertions from packages/middleware/express/test/auth/resourceServer.test.ts
# that match our current resource-server implementation.

test_that("WWW-Authenticate challenge for an invalid token carries error + description", {
  cfg <- oauth_config(
    issuer = "https://issuer.example",
    audience = "https://api.example",
    jwks_uri = "https://issuer.example/jwks")
  state <- new.env(parent = emptyenv()); state$auth <- cfg
  challenge <- mcpserver:::www_authenticate_challenge(state,
    error = "invalid_token",
    description = "Bearer token missing or invalid")
  expect_match(challenge, '^Bearer realm=', fixed = FALSE)
  expect_match(challenge, 'error="invalid_token"', fixed = TRUE)
  expect_match(challenge, 'error_description=', fixed = TRUE)
  expect_match(challenge, 'resource_metadata=', fixed = TRUE)
})

test_that("WWW-Authenticate challenge for insufficient scope carries scope=", {
  cfg <- oauth_config(
    issuer = "https://issuer.example",
    audience = "https://api.example",
    jwks_uri = "https://issuer.example/jwks",
    required_scopes = c("mcp:read", "mcp:write"))
  state <- new.env(parent = emptyenv()); state$auth <- cfg
  challenge <- mcpserver:::www_authenticate_challenge(state,
    error = "insufficient_scope",
    description = "Need more scope",
    scope = cfg$required_scopes)
  expect_match(challenge, 'error="insufficient_scope"', fixed = TRUE)
  expect_match(challenge, 'scope="mcp:read mcp:write"', fixed = TRUE)
})

test_that("WWW-Authenticate challenge without auth is the bare 'Bearer' string", {
  state <- new.env(parent = emptyenv()); state$auth <- NULL
  expect_equal(mcpserver:::www_authenticate_challenge(state), "Bearer")
})

test_that("oauth_extract_bearer handles case-insensitive scheme + extra spacing", {
  expect_equal(mcpserver:::oauth_extract_bearer("Bearer abc"), "abc")
  expect_equal(mcpserver:::oauth_extract_bearer("bearer abc"), "abc")
  expect_equal(mcpserver:::oauth_extract_bearer("BEARER abc"), "abc")
  expect_null(mcpserver:::oauth_extract_bearer("Basic abc"))
})

test_that("parse_scope accepts both space-delimited scope and array scp claims", {
  expect_equal(mcpserver:::parse_scope("read write"),
               c("read", "write"))
  expect_equal(mcpserver:::parse_scope(NULL, c("read", "write")),
               c("read", "write"))
  expect_equal(mcpserver:::parse_scope(NULL, NULL),
               character(0L))
})

test_that("expired-token JWT path returns reason invalid_token", {
  rsa <- openssl::rsa_keygen(2048L)
  jwk <- jose::write_jwk(rsa)
  jwk_id <- jsonlite::fromJSON(jwk, simplifyVector = FALSE)
  jwk_id$kid <- "k1"; jwk_id$use <- "sig"
  jwks_json <- jsonlite::toJSON(list(keys = list(jwk_id)),
                                auto_unbox = TRUE)
  now <- as.integer(Sys.time())
  claim <- jose::jwt_claim(iss = "https://issuer.example",
                           aud = "https://api.example",
                           sub = "u",
                           iat = now - 600L,
                           exp = now - 300L,
                           scope = "read")
  expired <- jose::jwt_encode_sig(claim, key = rsa,
    header = list(typ = "JWT", alg = "RS256", kid = "k1"))
  cfg <- oauth_config(issuer = "https://issuer.example",
                      audience = "https://api.example",
                      jwks_uri = "https://issuer.example/jwks")
  mcpserver:::oauth_set_jwks(cfg, jwks_json)
  res <- mcpserver:::oauth_verify_jwt(cfg, expired)
  expect_false(isTRUE(res$ok))
  expect_equal(res$reason, "invalid_token")
})

test_that("audience mismatch returns reason invalid_token", {
  rsa <- openssl::rsa_keygen(2048L)
  jwk <- jose::write_jwk(rsa)
  jwk_id <- jsonlite::fromJSON(jwk, simplifyVector = FALSE)
  jwk_id$kid <- "k1"; jwk_id$use <- "sig"
  jwks_json <- jsonlite::toJSON(list(keys = list(jwk_id)),
                                auto_unbox = TRUE)
  now <- as.integer(Sys.time())
  claim <- jose::jwt_claim(iss = "https://issuer.example",
                           aud = "https://different.example",
                           sub = "u", iat = now, exp = now + 60L,
                           scope = "read")
  token <- jose::jwt_encode_sig(claim, key = rsa,
    header = list(typ = "JWT", alg = "RS256", kid = "k1"))
  cfg <- oauth_config(issuer = "https://issuer.example",
                      audience = "https://api.example",
                      jwks_uri = "https://issuer.example/jwks")
  mcpserver:::oauth_set_jwks(cfg, jwks_json)
  res <- mcpserver:::oauth_verify_jwt(cfg, token)
  expect_false(isTRUE(res$ok))
  expect_equal(res$reason, "invalid_token")
})
