# RFC 7591 dynamic client registration.

new_test_as <- function() {
  oauth_server_config(
    issuer = "https://as.example",
    audience = "https://mcp.example")
}

call_register <- function(cfg, body_obj) {
  body <- jsonlite::toJSON(body_obj, auto_unbox = TRUE,
                           force = TRUE)
  req <- list(body = charToRaw(body),
              headers = c("Content-Type" = "application/json"))
  mcpserver:::oauth_as_register_handler(cfg)(req)
}

test_that("/register assigns a client_id and persists redirect_uris", {
  cfg <- new_test_as()
  resp <- call_register(cfg, list(
    client_name = "Demo Client",
    redirect_uris = list("http://localhost/cb",
                          "http://localhost/alt")))
  expect_equal(resp$status, 201L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_true(nzchar(body$client_id))
  expect_equal(body$client_name, "Demo Client")
  expect_setequal(unlist(body$redirect_uris),
                  c("http://localhost/cb", "http://localhost/alt"))
  expect_equal(body$token_endpoint_auth_method, "none")
  expect_true(!is.null(cfg$client_store$get(body$client_id)))
})

test_that("/register rejects an empty redirect_uris array", {
  cfg <- new_test_as()
  resp <- call_register(cfg, list(client_name = "X",
                                  redirect_uris = list()))
  expect_equal(resp$status, 400L)
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_equal(body$error, "invalid_redirect_uri")
})

test_that("/register defaults grant_types and response_types", {
  cfg <- new_test_as()
  resp <- call_register(cfg, list(client_name = "Y",
                                  redirect_uris = list("http://x/cb")))
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_setequal(unlist(body$grant_types),
                  c("authorization_code", "refresh_token"))
  expect_equal(unlist(body$response_types), "code")
})

test_that("/register accepts a missing client_name and picks one", {
  cfg <- new_test_as()
  resp <- call_register(cfg, list(redirect_uris = list("http://x/cb")))
  body <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
  expect_true(nzchar(body$client_name))
})

test_that("/register issues unique client_ids on repeated calls", {
  cfg <- new_test_as()
  a <- jsonlite::fromJSON(call_register(cfg, list(
    redirect_uris = list("http://x/cb")))$body, simplifyVector = FALSE)
  b <- jsonlite::fromJSON(call_register(cfg, list(
    redirect_uris = list("http://x/cb")))$body, simplifyVector = FALSE)
  expect_false(identical(a$client_id, b$client_id))
})
