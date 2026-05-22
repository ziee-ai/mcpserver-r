# Verifier-side: revocation_store gates accepted JWTs by jti.

skip_on_cran()

# Build a self-signed JWT against an RS256 key, and an oauth_config()
# pinned to the matching JWKS. The verifier flow we exercise is:
#   parse + signature check + claim check + revocation_store lookup.

mk_keypair <- function() {
  key <- openssl::rsa_keygen(2048L)
  jwk_json <- jose::write_jwk(key$pubkey)
  jwk <- jsonlite::fromJSON(jwk_json, simplifyVector = FALSE)
  jwk$kid <- "test-kid"
  jwk$use <- "sig"
  jwk$alg <- "RS256"
  list(
    priv = key,
    jwks_json = jsonlite::toJSON(list(keys = list(jwk)),
                                 auto_unbox = TRUE, force = TRUE)
  )
}

mk_jwt <- function(priv, payload) {
  # `jose::jwt_encode_sig()` wants a `jwt_claim`. Pass payload via do.call
  # so we can include arbitrary fields (jti, scope, sub, aud, ...).
  claim <- do.call(jose::jwt_claim, payload)
  jose::jwt_encode_sig(claim, key = priv,
                       header = list(kid = "test-kid"))
}

base_payload <- function(jti = NULL, scopes = "mcp:read") {
  now <- as.integer(Sys.time())
  out <- list(
    iss   = "https://issuer.test",
    aud   = "mcp",
    sub   = "u_alice",
    exp   = now + 600L,
    nbf   = now - 60L,
    iat   = now - 60L,
    scope = paste(scopes, collapse = " ")
  )
  if (!is.null(jti) && nzchar(jti)) out$jti <- jti
  out
}

mk_cfg <- function(kp, revocation_store = NULL) {
  cfg <- oauth_config(
    issuer = "https://issuer.test",
    audience = "mcp",
    jwks_uri = "https://issuer.test/jwks",
    required_scopes = character(0L),
    leeway = 30L,
    revocation_store = revocation_store
  )
  # short-circuit network fetch
  oauth_set_jwks(cfg, kp$jwks_json)
  cfg
}

test_that("verifier without revocation_store ignores jti (external-IdP parity)", {
  kp <- mk_keypair()
  cfg <- mk_cfg(kp, revocation_store = NULL)
  # Token has no jti at all
  jwt <- mk_jwt(kp$priv, base_payload(jti = NULL))
  res <- oauth_verify_bearer(cfg, paste("Bearer", jwt))
  expect_true(res$ok)
  expect_equal(res$subject, "u_alice")
})

test_that("verifier with revocation_store rejects missing jti", {
  kp <- mk_keypair()
  store <- new_mcp_store("memory")
  cfg <- mk_cfg(kp, revocation_store = store$tokens)
  jwt <- mk_jwt(kp$priv, base_payload(jti = NULL))
  res <- oauth_verify_bearer(cfg, paste("Bearer", jwt))
  expect_false(res$ok)
  expect_equal(res$reason, "invalid_token")
})

test_that("verifier with revocation_store rejects unknown jti", {
  kp <- mk_keypair()
  store <- new_mcp_store("memory")
  cfg <- mk_cfg(kp, revocation_store = store$tokens)
  jwt <- mk_jwt(kp$priv, base_payload(jti = "j_unknown"))
  res <- oauth_verify_bearer(cfg, paste("Bearer", jwt))
  expect_false(res$ok)
})

test_that("verifier with revocation_store accepts known active jti", {
  kp <- mk_keypair()
  store <- new_mcp_store("memory")
  store$users$add(list(id = "u_alice", username = "alice"))
  store$tokens$add(list(jti = "j_ok", user_id = "u_alice", name = "ci",
                        scopes = "mcp:read",
                        expires_at = "2099-01-01T00:00:00Z"))
  cfg <- mk_cfg(kp, revocation_store = store$tokens)
  jwt <- mk_jwt(kp$priv, base_payload(jti = "j_ok"))
  res <- oauth_verify_bearer(cfg, paste("Bearer", jwt))
  expect_true(res$ok)
  expect_equal(res$subject, "u_alice")
  # last_used_at was updated as a side effect
  expect_true(nzchar(store$tokens$get("j_ok")$last_used_at))
})

test_that("verifier rejects an expired JWT (exp + leeway in the past)", {
  kp <- mk_keypair()
  store <- new_mcp_store("memory")
  store$users$add(list(id = "u_alice", username = "alice"))
  store$tokens$add(list(jti = "j_old", user_id = "u_alice",
                        name = "old", scopes = "mcp:read",
                        expires_at = "2099-01-01T00:00:00Z"))
  cfg <- mk_cfg(kp, revocation_store = store$tokens)
  now <- as.integer(Sys.time())
  payload <- base_payload(jti = "j_old")
  payload$exp <- now - 120L     # past
  payload$nbf <- now - 240L
  payload$iat <- now - 240L
  jwt <- mk_jwt(kp$priv, payload)
  res <- oauth_verify_bearer(cfg, paste("Bearer", jwt))
  expect_false(res$ok)
  expect_equal(res$reason, "invalid_token")
})

test_that("verifier returns insufficient_scope when required_scopes not met", {
  kp <- mk_keypair()
  cfg <- oauth_config(
    issuer   = "https://issuer.test",
    audience = "mcp",
    jwks_uri = "https://issuer.test/jwks",
    required_scopes = c("mcp:write"),
    leeway   = 30L
  )
  oauth_set_jwks(cfg, kp$jwks_json)
  jwt <- mk_jwt(kp$priv, base_payload(scopes = "mcp:read"))
  res <- oauth_verify_bearer(cfg, paste("Bearer", jwt))
  expect_false(res$ok)
  expect_equal(res$reason, "insufficient_scope")
})

test_that("verifier rejects after revocation", {
  kp <- mk_keypair()
  store <- new_mcp_store("memory")
  store$users$add(list(id = "u_alice", username = "alice"))
  store$tokens$add(list(jti = "j_kill", user_id = "u_alice", name = "ci",
                        scopes = "mcp:read",
                        expires_at = "2099-01-01T00:00:00Z"))
  cfg <- mk_cfg(kp, revocation_store = store$tokens)
  jwt <- mk_jwt(kp$priv, base_payload(jti = "j_kill"))
  expect_true(oauth_verify_bearer(cfg, paste("Bearer", jwt))$ok)
  store$tokens$revoke("j_kill")
  res <- oauth_verify_bearer(cfg, paste("Bearer", jwt))
  expect_false(res$ok)
  expect_equal(res$reason, "invalid_token")
})
