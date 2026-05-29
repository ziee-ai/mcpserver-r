# Direct unit tests for the token admin handlers, covering a case the
# SQLite-backed integration suite can't reach: reactivating a revoked
# token whose name is now held by an active token. That precondition
# (an active + revoked token sharing a (user_id, name)) is only possible
# with the memory driver -- SQLite's UNIQUE(user_id, name) forbids it.

skip_on_cran()

resp_json <- function(resp) {
  if (is.null(resp$body)) return(NULL)
  jsonlite::fromJSON(rawToChar_if_raw(resp$body), simplifyVector = FALSE)
}
rawToChar_if_raw <- function(x) if (is.raw(x)) rawToChar(x) else x

far_future <- "2099-01-01T00:00:00Z"

test_that("reactivate -> 409 when an active token already holds the name", {
  store <- new_mcp_store("memory")
  withr::defer(store$close())
  u <- store$users$add(list(username = "alice"))
  # original "ci", then revoke it
  store$tokens$add(list(jti = "old", user_id = u$id, name = "ci",
                        scopes = "mcp:read", expires_at = far_future))
  store$tokens$revoke("old")
  # a fresh active "ci" is allowed once the first is revoked (memory driver)
  store$tokens$add(list(jti = "new", user_id = u$id, name = "ci",
                        scopes = "mcp:read", expires_at = far_future))

  resp <- handle_tokens_reactivate(store, "old")
  expect_equal(resp$status, 409L)
  expect_match(resp_json(resp)$message, "already in use")
  # the revoked token stays revoked
  expect_true(store$tokens$get("old")$revoked)
})

test_that("reactivate -> 200 + revoked:false on the happy path", {
  store <- new_mcp_store("memory")
  withr::defer(store$close())
  u <- store$users$add(list(username = "bob"))
  store$tokens$add(list(jti = "j1", user_id = u$id, name = "ci",
                        scopes = "mcp:read", expires_at = far_future))
  store$tokens$revoke("j1")

  resp <- handle_tokens_reactivate(store, "j1")
  expect_equal(resp$status, 200L)
  expect_false(resp_json(resp)$revoked)
  expect_false(store$tokens$get("j1")$revoked)
})

test_that("reactivate/delete -> 404 for an unknown jti", {
  store <- new_mcp_store("memory")
  withr::defer(store$close())
  expect_equal(handle_tokens_reactivate(store, "nope")$status, 404L)
  expect_equal(handle_tokens_delete(store, "nope")$status, 404L)
})

test_that("delete -> 204 and removes the row", {
  store <- new_mcp_store("memory")
  withr::defer(store$close())
  u <- store$users$add(list(username = "carol"))
  store$tokens$add(list(jti = "j1", user_id = u$id, name = "ci",
                        scopes = "mcp:read", expires_at = far_future))
  resp <- handle_tokens_delete(store, "j1")
  expect_equal(resp$status, 204L)
  expect_null(store$tokens$get("j1"))
})
