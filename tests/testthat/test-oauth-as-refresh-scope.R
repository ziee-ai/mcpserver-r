# RFC 6749 §6: requested scope on refresh_token grant MUST be a subset
# of the original grant. Broader scope is rejected.

setup_cfg <- function() {
  cfg <- oauth_server_config(
    issuer = "https://as.example",
    audience = "https://mcp.example",
    subject = "user-x",
    scopes_supported = c("mcp:read", "mcp:write", "mcp:admin"))
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

mint_refresh <- function(cfg, scope = c("mcp:read", "mcp:write")) {
  verifier <- paste(rep("v", 64L), collapse = "")
  code <- new_uuid()
  cfg$code_store$add(code, list(
    client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_challenge = challenge_for(verifier),
    scope = scope,
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

call_refresh <- function(cfg, refresh_token, scope = NULL) {
  parts <- c("grant_type=refresh_token",
             "client_id=c-1",
             sprintf("refresh_token=%s",
                     utils::URLencode(refresh_token, reserved = TRUE)))
  if (!is.null(scope)) {
    parts <- c(parts,
               sprintf("scope=%s", utils::URLencode(scope, reserved = TRUE)))
  }
  body <- paste(parts, collapse = "&")
  req <- list(body = charToRaw(body), headers = character())
  mcpserver:::oauth_as_token_handler(cfg)(req)
}

# Happy paths -----------------------------------------------------------

test_that("refresh without `scope` reuses the originally-granted scope", {
  cfg <- setup_cfg()
  rt <- mint_refresh(cfg, scope = c("mcp:read", "mcp:write"))
  resp <- call_refresh(cfg, rt)
  expect_equal(resp$status, 200L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$scope, "mcp:read mcp:write")
})

test_that("refresh with a narrower scope is accepted and honoured", {
  cfg <- setup_cfg()
  rt <- mint_refresh(cfg, scope = c("mcp:read", "mcp:write"))
  resp <- call_refresh(cfg, rt, scope = "mcp:read")
  expect_equal(resp$status, 200L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$scope, "mcp:read")
})

test_that("refresh with the same scope is a no-op", {
  cfg <- setup_cfg()
  rt <- mint_refresh(cfg, scope = c("mcp:read"))
  resp <- call_refresh(cfg, rt, scope = "mcp:read")
  expect_equal(resp$status, 200L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$scope, "mcp:read")
})

# Rejection paths -------------------------------------------------------

test_that("refresh with a BROADER scope is rejected as invalid_scope", {
  cfg <- setup_cfg()
  rt <- mint_refresh(cfg, scope = c("mcp:read"))
  resp <- call_refresh(cfg, rt,
                       scope = "mcp:read mcp:write")
  expect_equal(resp$status, 400L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$error, "invalid_scope")
})

test_that("refresh with a scope not originally granted is rejected", {
  cfg <- setup_cfg()
  rt <- mint_refresh(cfg, scope = c("mcp:read"))
  resp <- call_refresh(cfg, rt, scope = "mcp:admin")
  expect_equal(resp$status, 400L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$error, "invalid_scope")
})
