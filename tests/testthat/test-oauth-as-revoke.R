# RFC 7009 token revocation endpoint.

setup_cfg <- function() {
  cfg <- oauth_server_config(
    issuer = "https://as.example",
    audience = "https://mcp.example",
    subject = "user-x")
  cfg$client_store$add("c-1", list(
    client_id = "c-1",
    client_name = "Public",
    redirect_uris = c("http://app.example/cb"),
    token_endpoint_auth_method = "none"))
  cfg
}

challenge_for <- function(verifier) {
  raw <- openssl::sha256(charToRaw(verifier))
  sub("=+$", "", jose::base64url_encode(raw))
}

mint_refresh_token <- function(cfg) {
  verifier <- paste(rep("v", 64L), collapse = "")
  code <- new_uuid()
  cfg$code_store$add(code, list(
    client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_challenge = challenge_for(verifier),
    scope = c("mcp:read"),
    subject = cfg$subject,
    expires = as.numeric(Sys.time()) + 60))
  body <- paste(c("grant_type=authorization_code",
                  sprintf("code=%s", code),
                  "client_id=c-1",
                  "redirect_uri=http%3A%2F%2Fapp.example%2Fcb",
                  sprintf("code_verifier=%s", verifier)),
                collapse = "&")
  req <- list(body = charToRaw(body), headers = character())
  resp <- mcpserver:::oauth_as_token_handler(cfg)(req)
  jsonlite::fromJSON(resp$body, simplifyVector = FALSE)$refresh_token
}

call_revoke <- function(cfg, params, headers = character()) {
  body <- paste(vapply(names(params), function(n) {
    sprintf("%s=%s", utils::URLencode(n, reserved = TRUE),
            utils::URLencode(params[[n]], reserved = TRUE))
  }, character(1L)), collapse = "&")
  req <- list(body = charToRaw(body), headers = headers)
  mcpserver:::oauth_as_revoke_handler(cfg)(req)
}

test_that("/revoke 200s and removes an active refresh token", {
  cfg <- setup_cfg()
  refresh <- mint_refresh_token(cfg)
  expect_false(is.null(cfg$token_store$get(refresh)))
  resp <- call_revoke(cfg, list(token = refresh,
                                client_id = "c-1"))
  expect_equal(resp$status, 200L)
  expect_null(cfg$token_store$get(refresh))
})

test_that("/revoke followed by refresh_token grant rejects the revoked token", {
  cfg <- setup_cfg()
  refresh <- mint_refresh_token(cfg)
  call_revoke(cfg, list(token = refresh, client_id = "c-1"))
  body <- paste(c("grant_type=refresh_token",
                  "client_id=c-1",
                  sprintf("refresh_token=%s",
                          utils::URLencode(refresh, reserved = TRUE))),
                collapse = "&")
  req <- list(body = charToRaw(body), headers = character())
  resp <- mcpserver:::oauth_as_token_handler(cfg)(req)
  expect_equal(resp$status, 400L)
  payload <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(payload$error, "invalid_grant")
})

test_that("/revoke 200s on an unknown token (per RFC 7009 §2.2)", {
  cfg <- setup_cfg()
  resp <- call_revoke(cfg, list(token = "never-issued",
                                client_id = "c-1"))
  expect_equal(resp$status, 200L)
})

test_that("/revoke requires a token parameter", {
  cfg <- setup_cfg()
  resp <- call_revoke(cfg, list(client_id = "c-1"))
  expect_equal(resp$status, 400L)
  payload <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(payload$error, "invalid_request")
})

test_that("/revoke accepts the optional token_type_hint parameter", {
  cfg <- setup_cfg()
  refresh <- mint_refresh_token(cfg)
  resp <- call_revoke(cfg, list(token = refresh,
                                token_type_hint = "refresh_token",
                                client_id = "c-1"))
  expect_equal(resp$status, 200L)
})

test_that("/revoke is authenticated: public client requires only client_id", {
  cfg <- setup_cfg()
  refresh <- mint_refresh_token(cfg)
  resp <- call_revoke(cfg, list(token = refresh,
                                client_id = "c-1"))
  expect_equal(resp$status, 200L)
})

test_that("/revoke rejects an unknown client_id with 401", {
  cfg <- setup_cfg()
  refresh <- mint_refresh_token(cfg)
  resp <- call_revoke(cfg, list(token = refresh,
                                client_id = "not-a-client"))
  expect_equal(resp$status, 401L)
  payload <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(payload$error, "invalid_client")
})
