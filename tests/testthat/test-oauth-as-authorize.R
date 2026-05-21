# /authorize handler — PKCE-S256 happy & sad paths.

setup_cfg <- function(auto_consent = TRUE) {
  cfg <- oauth_server_config(
    issuer = "https://as.example",
    audience = "https://mcp.example",
    auto_consent = auto_consent)
  cfg$client_store$add("c-1", list(
    client_id = "c-1",
    client_name = "Demo",
    redirect_uris = c("http://app.example/cb")))
  cfg
}

call_authorize <- function(cfg, query = list()) {
  qs <- paste(vapply(names(query), function(n) {
    sprintf("%s=%s", utils::URLencode(n, reserved = TRUE),
            utils::URLencode(query[[n]], reserved = TRUE))
  }, character(1L)), collapse = "&")
  req <- list(uri = sprintf("/authorize?%s", qs),
              headers = c("Host" = "as.example"))
  mcpserver:::oauth_as_authorize_handler(cfg)(req)
}

challenge_for <- function(verifier) {
  raw <- openssl::sha256(charToRaw(verifier))
  sub("=+$", "", jose::base64url_encode(raw))
}

test_that("/authorize 302-redirects with code + state on a valid PKCE request", {
  cfg <- setup_cfg(auto_consent = TRUE)
  verifier <- paste(rep("a", 64L), collapse = "")
  resp <- call_authorize(cfg, list(
    response_type = "code",
    client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_challenge = challenge_for(verifier),
    code_challenge_method = "S256",
    scope = "mcp:read mcp:write",
    state = "abc123"))
  expect_equal(resp$status, 302L)
  loc <- unname(resp$headers["Location"])
  expect_match(loc, "^http://app.example/cb\\?code=")
  expect_match(loc, "state=abc123")
})

test_that("/authorize stores the code with the supplied challenge", {
  cfg <- setup_cfg(auto_consent = TRUE)
  verifier <- "vvv-verifier-vvv"
  resp <- call_authorize(cfg, list(
    response_type = "code",
    client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_challenge = challenge_for(verifier),
    code_challenge_method = "S256"))
  expect_equal(resp$status, 302L)
  loc <- unname(resp$headers["Location"])
  code <- sub(".*[?&]code=([^&]+).*", "\\1", loc)
  stored <- cfg$code_store$get(utils::URLdecode(code))
  expect_false(is.null(stored))
  expect_equal(stored$client_id, "c-1")
  expect_equal(stored$code_challenge, challenge_for(verifier))
})

test_that("/authorize rejects missing code_challenge", {
  cfg <- setup_cfg()
  resp <- call_authorize(cfg, list(
    response_type = "code",
    client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_challenge_method = "S256"))
  expect_equal(resp$status, 400L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$error, "invalid_request")
})

test_that("/authorize rejects non-S256 code_challenge_method", {
  cfg <- setup_cfg()
  resp <- call_authorize(cfg, list(
    response_type = "code",
    client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_challenge = "anything",
    code_challenge_method = "plain"))
  expect_equal(resp$status, 400L)
})

test_that("/authorize rejects response_type other than 'code'", {
  cfg <- setup_cfg()
  resp <- call_authorize(cfg, list(
    response_type = "token",
    client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_challenge = "x",
    code_challenge_method = "S256"))
  expect_equal(resp$status, 400L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$error, "unsupported_response_type")
})

test_that("/authorize rejects an unknown client_id with 401", {
  cfg <- setup_cfg()
  resp <- call_authorize(cfg, list(
    response_type = "code",
    client_id = "nope",
    redirect_uri = "http://app.example/cb",
    code_challenge = "x",
    code_challenge_method = "S256"))
  expect_equal(resp$status, 401L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$error, "invalid_client")
})

test_that("/authorize rejects an unregistered redirect_uri", {
  cfg <- setup_cfg()
  resp <- call_authorize(cfg, list(
    response_type = "code",
    client_id = "c-1",
    redirect_uri = "http://evil.example/cb",
    code_challenge = "x",
    code_challenge_method = "S256"))
  expect_equal(resp$status, 400L)
})

test_that("auto_consent = FALSE returns an HTML consent page on first GET", {
  cfg <- setup_cfg(auto_consent = FALSE)
  resp <- call_authorize(cfg, list(
    response_type = "code",
    client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_challenge = "x",
    code_challenge_method = "S256"))
  expect_equal(resp$status, 200L)
  ct <- unname(resp$headers["Content-Type"])
  expect_match(ct, "text/html")
  expect_match(resp$body, "<form", fixed = FALSE)
})

setup_cfg_loopback <- function(reg_uri) {
  cfg <- oauth_server_config(
    issuer = "https://as.example",
    audience = "https://mcp.example",
    auto_consent = TRUE)
  cfg$client_store$add("c-loop", list(
    client_id = "c-loop",
    client_name = "Loopback CLI",
    redirect_uris = c(reg_uri),
    token_endpoint_auth_method = "none"))
  cfg
}

test_that("RFC 8252: loopback 127.0.0.1 redirect_uri matches any port", {
  cfg <- setup_cfg_loopback("http://127.0.0.1/cb")
  resp <- call_authorize(cfg, list(
    response_type = "code",
    client_id = "c-loop",
    redirect_uri = "http://127.0.0.1:54321/cb",
    code_challenge = "x",
    code_challenge_method = "S256"))
  expect_equal(resp$status, 302L)
  expect_match(unname(resp$headers["Location"]),
               "^http://127.0.0.1:54321/cb\\?code=")
})

test_that("RFC 8252: loopback localhost redirect_uri matches any port", {
  cfg <- setup_cfg_loopback("http://localhost/cb")
  resp <- call_authorize(cfg, list(
    response_type = "code",
    client_id = "c-loop",
    redirect_uri = "http://localhost:48000/cb",
    code_challenge = "x",
    code_challenge_method = "S256"))
  expect_equal(resp$status, 302L)
})

test_that("RFC 8252: loopback [::1] redirect_uri matches any port", {
  cfg <- setup_cfg_loopback("http://[::1]/cb")
  resp <- call_authorize(cfg, list(
    response_type = "code",
    client_id = "c-loop",
    redirect_uri = "http://[::1]:48000/cb",
    code_challenge = "x",
    code_challenge_method = "S256"))
  expect_equal(resp$status, 302L)
})

test_that("RFC 8252: registered loopback with port still accepts other port", {
  cfg <- setup_cfg_loopback("http://127.0.0.1:1000/cb")
  resp <- call_authorize(cfg, list(
    response_type = "code",
    client_id = "c-loop",
    redirect_uri = "http://127.0.0.1:48000/cb",
    code_challenge = "x",
    code_challenge_method = "S256"))
  expect_equal(resp$status, 302L)
})

test_that("RFC 8252: loopback with different path is rejected", {
  cfg <- setup_cfg_loopback("http://127.0.0.1/cb")
  resp <- call_authorize(cfg, list(
    response_type = "code",
    client_id = "c-loop",
    redirect_uri = "http://127.0.0.1:48000/different",
    code_challenge = "x",
    code_challenge_method = "S256"))
  expect_equal(resp$status, 400L)
})

test_that("non-loopback redirect_uri still requires exact match (no port relaxation)", {
  cfg <- setup_cfg_loopback("http://app.example/cb")
  resp <- call_authorize(cfg, list(
    response_type = "code",
    client_id = "c-loop",
    redirect_uri = "http://app.example:9000/cb",
    code_challenge = "x",
    code_challenge_method = "S256"))
  expect_equal(resp$status, 400L)
})

test_that("auto_consent = FALSE accepts the resubmitted form and mints a code", {
  cfg <- setup_cfg(auto_consent = FALSE)
  resp <- call_authorize(cfg, list(
    response_type = "code",
    client_id = "c-1",
    redirect_uri = "http://app.example/cb",
    code_challenge = "x",
    code_challenge_method = "S256",
    `_consented` = "1"))
  expect_equal(resp$status, 302L)
})
