test_that("missing bearer header is rejected", {
  cfg <- oauth_config(
    issuer = "https://issuer.example",
    audience = "https://api.example",
    jwks_uri = "https://issuer.example/.well-known/jwks.json"
  )
  expect_false(isTRUE(mcpserver:::oauth_verify_bearer(cfg, NULL)$ok))
  expect_false(isTRUE(mcpserver:::oauth_verify_bearer(cfg, "Basic abc")$ok))
})

test_that("Bearer token parsing is case-insensitive and trims whitespace", {
  expect_equal(mcpserver:::oauth_extract_bearer("Bearer xyz"), "xyz")
  expect_equal(mcpserver:::oauth_extract_bearer("bearer xyz"), "xyz")
  expect_equal(mcpserver:::oauth_extract_bearer("Bearer  xyz"), "xyz")
})

test_that("parse_scope handles both scope and scp claims", {
  expect_equal(mcpserver:::parse_scope("read write"),
               c("read", "write"))
  expect_equal(mcpserver:::parse_scope(NULL, list("read", "write")),
               c("read", "write"))
  expect_equal(mcpserver:::parse_scope(NULL, NULL), character(0L))
})

test_that("JWT validation accepts valid signed token, rejects bad aud", {
  # Generate an RSA keypair and produce a JWT with jose, then validate.
  rsa <- openssl::rsa_keygen(2048L)
  jwk <- jose::write_jwk(rsa)
  jwk_id <- jsonlite::fromJSON(jwk, simplifyVector = FALSE)
  jwk_id$kid <- "k1"
  jwk_id$use <- "sig"
  jwks_json <- jsonlite::toJSON(list(keys = list(jwk_id)),
                                auto_unbox = TRUE)

  now <- as.integer(Sys.time())
  claim <- jose::jwt_claim(iss = "https://issuer.example",
                           aud = "https://api.example",
                           sub = "user-1",
                           iat = now, exp = now + 60L,
                           scope = "read write")
  hdr <- list(typ = "JWT", alg = "RS256", kid = "k1")
  token <- jose::jwt_encode_sig(claim, key = rsa, header = hdr)

  cfg <- oauth_config(
    issuer = "https://issuer.example",
    audience = "https://api.example",
    jwks_uri = "https://issuer.example/.well-known/jwks.json"
  )
  mcpserver:::oauth_set_jwks(cfg, jwks_json)

  good <- mcpserver:::oauth_verify_jwt(cfg, token)
  expect_true(good$ok)
  expect_equal(good$subject, "user-1")
  expect_true("read" %in% good$scopes)

  bad_aud_cfg <- oauth_config(
    issuer = "https://issuer.example",
    audience = "https://other.example",
    jwks_uri = "https://issuer.example/.well-known/jwks.json"
  )
  mcpserver:::oauth_set_jwks(bad_aud_cfg, jwks_json)
  expect_false(isTRUE(mcpserver:::oauth_verify_jwt(bad_aud_cfg, token)$ok))

  # Required scope missing
  scope_cfg <- oauth_config(
    issuer = "https://issuer.example",
    audience = "https://api.example",
    jwks_uri = "https://issuer.example/.well-known/jwks.json",
    required_scopes = c("admin")
  )
  mcpserver:::oauth_set_jwks(scope_cfg, jwks_json)
  expect_false(isTRUE(mcpserver:::oauth_verify_jwt(scope_cfg, token)$ok))

  # Expired
  exp_claim <- jose::jwt_claim(iss = "https://issuer.example",
                               aud = "https://api.example",
                               sub = "user-1",
                               iat = now - 600L, exp = now - 300L,
                               scope = "read")
  exp_token <- jose::jwt_encode_sig(exp_claim, key = rsa, header = hdr)
  expect_false(isTRUE(mcpserver:::oauth_verify_jwt(cfg, exp_token)$ok))
})
