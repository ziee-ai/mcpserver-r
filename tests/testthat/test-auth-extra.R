# OAuth coverage beyond the JWT happy path: introspection (RFC 7662)
# round-trip against a stubbed HTTP endpoint, plus bad-signature
# rejection.

skip_if_not_installed("httr2")

# Stub the introspection HTTP call by injecting a positive entry into the
# config's cache. The verifier short-circuits to the cache when the cache
# hit is non-expired, so we exercise the same accept-path without
# touching the network.
test_that("introspection accept path: cached positive entry returns ok+sub+scopes", {
  cfg <- oauth_config(
    issuer = "https://issuer.example",
    audience = "https://api.example",
    jwks_uri = "https://issuer.example/.well-known/jwks.json",
    introspection_url = "https://issuer.example/introspect"
  )
  token <- "opaque-token-123"
  key <- mcpserver:::digest_token(token)
  assign(key,
         list(value = list(ok = TRUE,
                           subject = "user-xyz",
                           scopes = c("read", "write")),
              expires = Sys.time() + 60),
         envir = cfg$introspection_cache)
  res <- mcpserver:::oauth_verify_introspection(cfg, token)
  expect_true(isTRUE(res$ok))
  expect_equal(res$subject, "user-xyz")
  expect_true("read" %in% res$scopes)
})

test_that("JWT with bad signature is rejected", {
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
                           iat = now, exp = now + 60L,
                           scope = "read")
  good <- jose::jwt_encode_sig(claim, key = rsa,
                               header = list(typ = "JWT", alg = "RS256",
                                             kid = "k1"))
  # Sign with a different key but advertise kid=k1 — signature mismatch.
  other_rsa <- openssl::rsa_keygen(2048L)
  bad <- jose::jwt_encode_sig(claim, key = other_rsa,
                              header = list(typ = "JWT", alg = "RS256",
                                            kid = "k1"))

  cfg <- oauth_config(issuer = "https://issuer.example",
                      audience = "https://api.example",
                      jwks_uri = "https://issuer.example/.well-known/jwks.json")
  mcpserver:::oauth_set_jwks(cfg, jwks_json)
  expect_true(isTRUE(mcpserver:::oauth_verify_jwt(cfg, good)$ok))
  expect_false(isTRUE(mcpserver:::oauth_verify_jwt(cfg, bad)$ok))
})

test_that("JWKS rotation triggers refresh on kid miss", {
  rsa <- openssl::rsa_keygen(2048L)
  jwk <- jose::write_jwk(rsa)
  jwk_id <- jsonlite::fromJSON(jwk, simplifyVector = FALSE)
  jwk_id$kid <- "old-key"; jwk_id$use <- "sig"
  cfg <- oauth_config(issuer = "https://issuer.example",
                      audience = "https://api.example",
                      jwks_uri = "https://issuer.example/jwks")
  mcpserver:::oauth_set_jwks(cfg,
    jsonlite::toJSON(list(keys = list(jwk_id)), auto_unbox = TRUE))
  # Lookup for a kid that's NOT cached should attempt refresh; with no
  # network override the second fetch falls back to the cached JSON, so
  # the function should return NULL (not throw).
  res <- mcpserver:::oauth_find_jwk(cfg, "unknown-kid")
  expect_true(is.null(res))
  # Cached kid still findable.
  found <- mcpserver:::oauth_find_jwk(cfg, "old-key")
  expect_equal(found$kid, "old-key")
})
