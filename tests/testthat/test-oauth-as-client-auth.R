# Confidential client authentication (client_secret_basic +
# client_secret_post) on /token and /revoke.

setup_cfg <- function() {
  oauth_server_config(
    issuer = "https://as.example",
    audience = "https://mcp.example",
    subject = "user-x")
}

challenge_for <- function(verifier) {
  raw <- openssl::sha256(charToRaw(verifier))
  sub("=+$", "", jose::base64url_encode(raw))
}

call_register <- function(cfg, body_obj) {
  body <- jsonlite::toJSON(body_obj, auto_unbox = TRUE,
                           force = TRUE)
  req <- list(body = charToRaw(body),
              headers = c("Content-Type" = "application/json"))
  jsonlite::fromJSON(
    mcpserver:::oauth_as_register_handler(cfg)(req)$body,
    simplifyVector = FALSE)
}

mint_code <- function(cfg, client_id, verifier,
                     redirect_uri = "http://app.example/cb",
                     scope = c("mcp:read")) {
  code <- new_uuid()
  cfg$code_store$add(code, list(
    client_id = client_id,
    redirect_uri = redirect_uri,
    code_challenge = challenge_for(verifier),
    scope = scope,
    subject = cfg$subject,
    expires = as.numeric(Sys.time()) + 60))
  code
}

call_token <- function(cfg, body_params, headers = character()) {
  body <- paste(vapply(names(body_params), function(n) {
    sprintf("%s=%s", utils::URLencode(n, reserved = TRUE),
            utils::URLencode(body_params[[n]], reserved = TRUE))
  }, character(1L)), collapse = "&")
  req <- list(body = charToRaw(body), headers = headers)
  mcpserver:::oauth_as_token_handler(cfg)(req)
}

basic_header <- function(client_id, secret) {
  raw <- charToRaw(paste0(client_id, ":", secret))
  c("Authorization" = paste0("Basic ", jsonlite::base64_enc(raw)))
}

# /register --------------------------------------------------------------

test_that("/register mints + returns a client_secret for client_secret_basic", {
  cfg <- setup_cfg()
  reg <- call_register(cfg, list(
    client_name = "Conf",
    redirect_uris = list("http://app.example/cb"),
    token_endpoint_auth_method = "client_secret_basic"))
  expect_equal(reg$token_endpoint_auth_method, "client_secret_basic")
  expect_true(nzchar(reg$client_secret))
  stored <- cfg$client_store$get(reg$client_id)
  # The clear secret must NOT be persisted.
  expect_null(stored[["client_secret"]])
  expect_false(is.null(stored$client_secret_hash))
})

test_that("/register mints a secret for client_secret_post too", {
  cfg <- setup_cfg()
  reg <- call_register(cfg, list(
    redirect_uris = list("http://app.example/cb"),
    token_endpoint_auth_method = "client_secret_post"))
  expect_true(nzchar(reg$client_secret))
})

test_that("/register defaults to 'none' (public client, no secret)", {
  cfg <- setup_cfg()
  reg <- call_register(cfg, list(
    redirect_uris = list("http://app.example/cb")))
  expect_equal(reg$token_endpoint_auth_method, "none")
  expect_null(reg$client_secret)
})

test_that("/register rejects an unknown token_endpoint_auth_method", {
  cfg <- setup_cfg()
  body <- jsonlite::toJSON(list(
    redirect_uris = list("http://app.example/cb"),
    token_endpoint_auth_method = "private_key_jwt"),
    auto_unbox = TRUE)
  req <- list(body = charToRaw(body),
              headers = c("Content-Type" = "application/json"))
  resp <- mcpserver:::oauth_as_register_handler(cfg)(req)
  expect_equal(resp$status, 400L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$error, "invalid_client_metadata")
})

# /token client_secret_basic happy path ---------------------------------

test_that("client_secret_basic happy path: code grant succeeds", {
  cfg <- setup_cfg()
  reg <- call_register(cfg, list(
    redirect_uris = list("http://app.example/cb"),
    token_endpoint_auth_method = "client_secret_basic"))
  verifier <- paste(rep("b", 64L), collapse = "")
  code <- mint_code(cfg, reg$client_id, verifier)
  resp <- call_token(cfg,
    list(grant_type = "authorization_code",
         code = code,
         redirect_uri = "http://app.example/cb",
         code_verifier = verifier),
    headers = basic_header(reg$client_id, reg$client_secret))
  expect_equal(resp$status, 200L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_true(nzchar(body$access_token))
})

test_that("client_secret_basic with wrong secret is rejected as 401 invalid_client", {
  cfg <- setup_cfg()
  reg <- call_register(cfg, list(
    redirect_uris = list("http://app.example/cb"),
    token_endpoint_auth_method = "client_secret_basic"))
  verifier <- paste(rep("c", 64L), collapse = "")
  code <- mint_code(cfg, reg$client_id, verifier)
  resp <- call_token(cfg,
    list(grant_type = "authorization_code",
         code = code,
         redirect_uri = "http://app.example/cb",
         code_verifier = verifier),
    headers = basic_header(reg$client_id, "wrong-secret"))
  expect_equal(resp$status, 401L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$error, "invalid_client")
})

# /token client_secret_post happy path ----------------------------------

test_that("client_secret_post happy path: code grant succeeds", {
  cfg <- setup_cfg()
  reg <- call_register(cfg, list(
    redirect_uris = list("http://app.example/cb"),
    token_endpoint_auth_method = "client_secret_post"))
  verifier <- paste(rep("p", 64L), collapse = "")
  code <- mint_code(cfg, reg$client_id, verifier)
  resp <- call_token(cfg, list(
    grant_type = "authorization_code",
    code = code,
    client_id = reg$client_id,
    client_secret = reg$client_secret,
    redirect_uri = "http://app.example/cb",
    code_verifier = verifier))
  expect_equal(resp$status, 200L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_true(nzchar(body$access_token))
})

test_that("client_secret_post without client_secret is rejected", {
  cfg <- setup_cfg()
  reg <- call_register(cfg, list(
    redirect_uris = list("http://app.example/cb"),
    token_endpoint_auth_method = "client_secret_post"))
  verifier <- paste(rep("q", 64L), collapse = "")
  code <- mint_code(cfg, reg$client_id, verifier)
  resp <- call_token(cfg, list(
    grant_type = "authorization_code",
    code = code,
    client_id = reg$client_id,
    redirect_uri = "http://app.example/cb",
    code_verifier = verifier))
  expect_equal(resp$status, 401L)
})

test_that("client_secret_basic falls back to body params (RFC 6749 §2.3.1)", {
  cfg <- setup_cfg()
  reg <- call_register(cfg, list(
    redirect_uris = list("http://app.example/cb"),
    token_endpoint_auth_method = "client_secret_basic"))
  verifier <- paste(rep("f", 64L), collapse = "")
  code <- mint_code(cfg, reg$client_id, verifier)
  # No Authorization header — supply client_id + client_secret in
  # the body instead. RFC permits this fallback.
  resp <- call_token(cfg, list(
    grant_type = "authorization_code",
    code = code,
    client_id = reg$client_id,
    client_secret = reg$client_secret,
    redirect_uri = "http://app.example/cb",
    code_verifier = verifier))
  expect_equal(resp$status, 200L)
})

test_that("public client (none) still works without secret", {
  cfg <- setup_cfg()
  reg <- call_register(cfg, list(
    redirect_uris = list("http://app.example/cb")))
  expect_equal(reg$token_endpoint_auth_method, "none")
  verifier <- paste(rep("n", 64L), collapse = "")
  code <- mint_code(cfg, reg$client_id, verifier)
  resp <- call_token(cfg, list(
    grant_type = "authorization_code",
    code = code,
    client_id = reg$client_id,
    redirect_uri = "http://app.example/cb",
    code_verifier = verifier))
  expect_equal(resp$status, 200L)
})

# Secret hashing --------------------------------------------------------

test_that("client_secret is stored as a salt+digest hash, not cleartext", {
  cfg <- setup_cfg()
  reg <- call_register(cfg, list(
    redirect_uris = list("http://app.example/cb"),
    token_endpoint_auth_method = "client_secret_basic"))
  stored <- cfg$client_store$get(reg$client_id)
  expect_null(stored[["client_secret"]])
  expect_type(stored$client_secret_hash$salt, "character")
  expect_type(stored$client_secret_hash$digest, "character")
  expect_true(nchar(stored$client_secret_hash$digest) >= 32L)
})

test_that("oauth_verify_secret returns TRUE iff secret matches", {
  hashed <- mcpserver:::oauth_hash_secret("hunter2")
  expect_true(mcpserver:::oauth_verify_secret("hunter2", hashed))
  expect_false(mcpserver:::oauth_verify_secret("wrong", hashed))
})
