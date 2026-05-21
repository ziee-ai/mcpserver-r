# CORS headers on OAuth AS endpoints — browsers driving the OAuth flow
# need permissive CORS so the SPA can post to /token and /revoke.

setup_cfg <- function() {
  cfg <- oauth_server_config(
    issuer = "https://as.example",
    audience = "https://mcp.example")
  cfg$client_store$add("c-1", list(
    client_id = "c-1", client_name = "x",
    redirect_uris = c("http://app.example/cb"),
    token_endpoint_auth_method = "none"))
  cfg
}

expect_cors <- function(resp) {
  vals <- resp$headers
  expect_equal(unname(vals["Access-Control-Allow-Origin"]), "*")
  expect_match(unname(vals["Access-Control-Allow-Methods"]),
               "POST")
  expect_match(unname(vals["Access-Control-Allow-Headers"]),
               "Authorization")
  expect_equal(unname(vals["Access-Control-Max-Age"]), "86400")
}

test_that("/.well-known/oauth-authorization-server carries CORS headers", {
  cfg <- setup_cfg()
  resp <- mcpserver:::oauth_as_metadata_handler(cfg)(list())
  expect_cors(resp)
})

test_that("/jwks carries CORS headers", {
  cfg <- setup_cfg()
  resp <- mcpserver:::oauth_as_jwks_handler(cfg)(list())
  expect_cors(resp)
})

test_that("/authorize carries CORS headers", {
  cfg <- setup_cfg()
  req <- list(uri = "/authorize", headers = character())
  resp <- mcpserver:::oauth_as_authorize_handler(cfg)(req)
  # Even an error response surfaces CORS headers (so the browser can
  # actually read the error body in a fetch() call).
  expect_equal(unname(resp$headers["Access-Control-Allow-Origin"]),
               "*")
})

test_that("/token carries CORS headers", {
  cfg <- setup_cfg()
  body <- "grant_type=refresh_token&client_id=c-1&refresh_token=missing"
  req <- list(body = charToRaw(body), headers = character())
  resp <- mcpserver:::oauth_as_token_handler(cfg)(req)
  expect_equal(unname(resp$headers["Access-Control-Allow-Origin"]),
               "*")
})

test_that("/register carries CORS headers", {
  cfg <- setup_cfg()
  req <- list(body = charToRaw('{"redirect_uris":["http://app/cb"]}'),
              headers = c("Content-Type" = "application/json"))
  resp <- mcpserver:::oauth_as_register_handler(cfg)(req)
  expect_equal(unname(resp$headers["Access-Control-Allow-Origin"]),
               "*")
})

test_that("/revoke carries CORS headers", {
  cfg <- setup_cfg()
  body <- "token=anything&client_id=c-1"
  req <- list(body = charToRaw(body), headers = character())
  resp <- mcpserver:::oauth_as_revoke_handler(cfg)(req)
  expect_equal(unname(resp$headers["Access-Control-Allow-Origin"]),
               "*")
})
