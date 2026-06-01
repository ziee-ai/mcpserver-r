# Issue 1: the OAuth AS signing key must persist across restarts, otherwise
# every restart rotates the key and silently invalidates all previously
# minted tokens (their store rows still read `active`). oauth_server_config()
# loads/persists the key at `key_path`.

# ---- load_or_create_signing_key() --------------------------------------

test_that("key is generated and written (0600) when key_path is absent", {
  kp <- tempfile(fileext = ".pem")
  expect_false(file.exists(kp))
  key <- mcpserver:::load_or_create_signing_key(kp)
  expect_true(file.exists(kp))
  expect_true(inherits(key, "key") || inherits(key, "rsa"))
  # 0600 == owner read/write only
  expect_equal(as.character(file.mode(kp)), "600")
})

test_that("nested key_path directories are created", {
  kp <- file.path(tempfile(), "nested", "as-key.pem")
  mcpserver:::load_or_create_signing_key(kp)
  expect_true(file.exists(kp))
})

test_that("an existing key_path is loaded, not regenerated", {
  kp <- tempfile(fileext = ".pem")
  k1 <- mcpserver:::load_or_create_signing_key(kp)
  k2 <- mcpserver:::load_or_create_signing_key(kp)
  expect_identical(
    jose::write_jwk(k1$pubkey),
    jose::write_jwk(k2$pubkey))
})

test_that("default key_path is a dotfile in the working directory", {
  d <- tempfile()
  dir.create(d)
  withr::with_dir(d, {
    mcpserver:::load_or_create_signing_key(NULL)
    expect_true(file.exists(file.path(d, ".mcpserver-as-key.pem")))
  })
})

# ---- oauth_server_config() wiring --------------------------------------

test_that("two configs sharing key_path publish an identical JWKS", {
  kp <- tempfile(fileext = ".pem")
  mk <- function() oauth_server_config(
    issuer = "https://as.example", audience = "mcp", key_path = kp)
  expect_identical(
    mcpserver:::oauth_as_jwks_json(mk()),
    mcpserver:::oauth_as_jwks_json(mk()))
})

test_that("an explicit signing_key wins and key_path is not written", {
  kp <- tempfile(fileext = ".pem")
  key <- openssl::rsa_keygen(2048L)
  cfg <- oauth_server_config(issuer = "https://as.example", audience = "mcp",
                             signing_key = key, key_path = kp)
  expect_false(file.exists(kp))
  expect_identical(jose::write_jwk(cfg$signing_key$pubkey),
                   jose::write_jwk(key$pubkey))
})

# ---- restart survival (the core regression) ----------------------------

# Mirror serve_http()'s wiring: build the resource-server validator from
# the AS config and thread the token store in for revocation checks.
verifier_for <- function(as_cfg) {
  v <- mcpserver:::oauth_config_from_server(as_cfg)
  v$revocation_store <- as_cfg$store$tokens
  v
}

mint_for <- function(as_cfg, store) {
  u <- store$users$add(list(username = paste0("u", as.integer(runif(1, 1, 1e6)))))
  tok <- oauth_mint_user_token(as_cfg, user_id = u$id,
                               scopes = c("mcp:read"), ttl = 3600L,
                               name = "t")
  list(user = u, tok = tok)
}

test_that("a token minted before a restart still validates after it", {
  db <- tempfile(fileext = ".sqlite")
  kp <- tempfile(fileext = ".pem")
  store <- new_mcp_store("sqlite", path = db)
  on.exit(store$close(), add = TRUE)

  cfg1 <- oauth_server_config(issuer = "https://as.example", audience = "mcp",
                              store = store, key_path = kp)
  m <- mint_for(cfg1, store)

  # "Restart": brand-new config + store handle, SAME key_path + db.
  store2 <- new_mcp_store("sqlite", path = db)
  on.exit(store2$close(), add = TRUE)
  cfg2 <- oauth_server_config(issuer = "https://as.example", audience = "mcp",
                              store = store2, key_path = kp)

  res <- mcpserver:::oauth_verify_bearer(
    verifier_for(cfg2), paste("Bearer", m$tok$token))
  expect_true(isTRUE(res$ok))
  expect_equal(res$subject, m$user$id)
})

test_that("negative control: a restart with a fresh key rejects old tokens", {
  db <- tempfile(fileext = ".sqlite")
  store <- new_mcp_store("sqlite", path = db)
  on.exit(store$close(), add = TRUE)

  cfg1 <- oauth_server_config(issuer = "https://as.example", audience = "mcp",
                              store = store, key_path = tempfile(fileext = ".pem"))
  m <- mint_for(cfg1, store)

  # Fresh key path => fresh key (the pre-fix behavior).
  cfg2 <- oauth_server_config(issuer = "https://as.example", audience = "mcp",
                              store = store, key_path = tempfile(fileext = ".pem"))
  res <- mcpserver:::oauth_verify_bearer(
    verifier_for(cfg2), paste("Bearer", m$tok$token))
  expect_false(isTRUE(res$ok))
})

test_that("a revoked token stays rejected across a restart", {
  db <- tempfile(fileext = ".sqlite")
  kp <- tempfile(fileext = ".pem")
  store <- new_mcp_store("sqlite", path = db)
  on.exit(store$close(), add = TRUE)
  cfg1 <- oauth_server_config(issuer = "https://as.example", audience = "mcp",
                              store = store, key_path = kp)
  m <- mint_for(cfg1, store)
  store$tokens$revoke(m$tok$jti)

  store2 <- new_mcp_store("sqlite", path = db)
  on.exit(store2$close(), add = TRUE)
  cfg2 <- oauth_server_config(issuer = "https://as.example", audience = "mcp",
                              store = store2, key_path = kp)
  res <- mcpserver:::oauth_verify_bearer(
    verifier_for(cfg2), paste("Bearer", m$tok$token))
  expect_false(isTRUE(res$ok))
})

test_that("an expired token is rejected after a restart", {
  db <- tempfile(fileext = ".sqlite")
  kp <- tempfile(fileext = ".pem")
  store <- new_mcp_store("sqlite", path = db)
  on.exit(store$close(), add = TRUE)
  cfg1 <- oauth_server_config(issuer = "https://as.example", audience = "mcp",
                              store = store, key_path = kp)
  # Register a (non-revoked) jti, then craft a JWT signed by the SAME
  # persisted key but with exp in the past, so only expiry can reject it.
  u <- store$users$add(list(username = "exp-user"))
  now <- as.integer(Sys.time())
  iso <- function(t) format(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"),
                            "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  store$tokens$add(list(jti = "expired-jti", user_id = u$id, name = "expired",
                        scopes = c("mcp:read"), expires_at = iso(now - 3600)))
  claim <- jose::jwt_claim(iss = "https://as.example", aud = "mcp", sub = u$id,
                           exp = now - 3600, nbf = now - 7200, iat = now - 7200,
                           jti = "expired-jti", scope = "mcp:read")
  tok <- jose::jwt_encode_sig(claim, key = cfg1$signing_key,
                              header = list(kid = cfg1$kid))

  store2 <- new_mcp_store("sqlite", path = db)
  on.exit(store2$close(), add = TRUE)
  cfg2 <- oauth_server_config(issuer = "https://as.example", audience = "mcp",
                              store = store2, key_path = kp)
  res <- mcpserver:::oauth_verify_bearer(
    verifier_for(cfg2), paste("Bearer", tok))
  expect_false(isTRUE(res$ok))
})

test_that("memory-store caveat: tokens do not survive (jti row is lost)", {
  kp <- tempfile(fileext = ".pem")
  store <- new_mcp_store("memory")
  cfg1 <- oauth_server_config(issuer = "https://as.example", audience = "mcp",
                              store = store, key_path = kp)
  m <- mint_for(cfg1, store)

  # "Restart" with the SAME key but a fresh (empty) memory store: the
  # signature still checks out, but the jti row is gone so the revocation
  # check rejects the token.
  store2 <- new_mcp_store("memory")
  cfg2 <- oauth_server_config(issuer = "https://as.example", audience = "mcp",
                              store = store2, key_path = kp)
  res <- mcpserver:::oauth_verify_bearer(
    verifier_for(cfg2), paste("Bearer", m$tok$token))
  expect_false(isTRUE(res$ok))
})
