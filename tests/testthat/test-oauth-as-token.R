# /token handler — authorization_code + refresh_token grants with PKCE.

setup_cfg <- function() {
  cfg <- oauth_server_config(
    issuer = "https://as.example",
    audience = "https://mcp.example",
    subject = "user-x")
  cfg$client_store$add("c-1", list(
    client_id = "c-1",
    client_name = "Demo",
    redirect_uris = c("http://app.example/cb")))
  cfg
}

challenge_for <- function(verifier) {
  raw <- openssl::sha256(charToRaw(verifier))
  sub("=+$", "", jose::base64url_encode(raw))
}

mint_code <- function(cfg, verifier, scope = c("mcp:read")) {
  code <- new_uuid()
  cfg$code_store$add(code, list(
    client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_challenge = challenge_for(verifier),
    scope = scope,
    subject = cfg$subject,
    expires = as.numeric(Sys.time()) + 60))
  code
}

call_token <- function(cfg, params) {
  body <- paste(vapply(names(params), function(n) {
    sprintf("%s=%s", utils::URLencode(n, reserved = TRUE),
            utils::URLencode(params[[n]], reserved = TRUE))
  }, character(1L)), collapse = "&")
  req <- list(body = charToRaw(body),
              headers = c("Content-Type" = "application/x-www-form-urlencoded"))
  mcpserver:::oauth_as_token_handler(cfg)(req)
}

# authorization_code grant happy path -----------------------------------

test_that("authorization_code grant returns access + refresh tokens", {
  cfg <- setup_cfg()
  verifier <- paste(rep("v", 64L), collapse = "")
  code <- mint_code(cfg, verifier)
  resp <- call_token(cfg, list(
    grant_type = "authorization_code",
    code = code,
    client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_verifier = verifier))
  expect_equal(resp$status, 200L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_true(nzchar(body$access_token))
  expect_true(nzchar(body$refresh_token))
  expect_equal(body$token_type, "Bearer")
  expect_equal(body$expires_in, cfg$ttl_access)
})

test_that("authorization_code is one-shot — second exchange fails", {
  cfg <- setup_cfg()
  verifier <- "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv"
  code <- mint_code(cfg, verifier)
  ok <- call_token(cfg, list(
    grant_type = "authorization_code",
    code = code, client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_verifier = verifier))
  expect_equal(ok$status, 200L)
  again <- call_token(cfg, list(
    grant_type = "authorization_code",
    code = code, client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_verifier = verifier))
  expect_equal(again$status, 400L)
  body <- jsonlite::fromJSON(again$body, simplifyVector = FALSE)
  expect_equal(body$error, "invalid_grant")
})

test_that("PKCE verifier mismatch is rejected as invalid_grant", {
  cfg <- setup_cfg()
  code <- mint_code(cfg, "right-verifier")
  resp <- call_token(cfg, list(
    grant_type = "authorization_code",
    code = code, client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_verifier = "wrong-verifier"))
  expect_equal(resp$status, 400L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$error, "invalid_grant")
})

test_that("redirect_uri mismatch is rejected", {
  cfg <- setup_cfg()
  verifier <- "v" |> rep(64L) |> paste(collapse = "")
  code <- mint_code(cfg, verifier)
  resp <- call_token(cfg, list(
    grant_type = "authorization_code",
    code = code, client_id = "c-1",
    redirect_uri = "http://app.example/other",
    code_verifier = verifier))
  expect_equal(resp$status, 400L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$error, "invalid_grant")
})

test_that("missing required fields after auth are rejected as invalid_request", {
  cfg <- setup_cfg()
  resp <- call_token(cfg, list(grant_type = "authorization_code",
                               client_id = "c-1"))
  expect_equal(resp$status, 400L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$error, "invalid_request")
})

# refresh_token grant ---------------------------------------------------

test_that("refresh_token grant mints a fresh access token", {
  cfg <- setup_cfg()
  verifier <- paste(rep("r", 64L), collapse = "")
  code <- mint_code(cfg, verifier, scope = c("mcp:read"))
  first <- call_token(cfg, list(
    grant_type = "authorization_code",
    code = code, client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_verifier = verifier))
  first_body <- jsonlite::fromJSON(first$body, simplifyVector = FALSE)
  refreshed <- call_token(cfg, list(
    grant_type = "refresh_token",
    client_id = "c-1",
    refresh_token = first_body$refresh_token))
  expect_equal(refreshed$status, 200L)
  body <- jsonlite::fromJSON(refreshed$body, simplifyVector = FALSE)
  expect_true(nzchar(body$access_token))
  expect_false(identical(body$access_token, first_body$access_token))
})

test_that("refresh_token rejects unknown tokens", {
  cfg <- setup_cfg()
  resp <- call_token(cfg, list(
    grant_type = "refresh_token",
    client_id = "c-1",
    refresh_token = "not-a-real-token"))
  expect_equal(resp$status, 400L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$error, "invalid_grant")
})

test_that("unsupported grant_type is rejected", {
  cfg <- setup_cfg()
  resp <- call_token(cfg, list(grant_type = "password",
                               username = "u", password = "p"))
  expect_equal(resp$status, 400L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$error, "unsupported_grant_type")
})

# Issued tokens are usable against the matching resource server --------

test_that("the issued access token verifies through oauth_config_from_server", {
  cfg <- setup_cfg()
  verifier <- paste(rep("k", 64L), collapse = "")
  code <- mint_code(cfg, verifier, scope = c("mcp:read"))
  resp <- call_token(cfg, list(
    grant_type = "authorization_code",
    code = code, client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_verifier = verifier))
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  rs <- mcpserver:::oauth_config_from_server(cfg)
  v <- mcpserver:::oauth_verify_jwt(rs, body$access_token)
  expect_true(isTRUE(v$ok))
  expect_equal(v$subject, "user-x")
  expect_setequal(v$scopes, "mcp:read")
})
