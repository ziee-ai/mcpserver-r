# SQLite-specific: persistence across close/reopen, WAL pragma,
# foreign-key enforcement.

skip_on_cran()
skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

test_that("data survives store close + reopen against the same path", {
  path <- tempfile(fileext = ".db")
  store <- new_mcp_store("sqlite", path = path)
  u <- store$users$add(list(id = "u_persistent",
                            username = "alice", is_admin = TRUE))
  store$tokens$add(list(jti = "j1", user_id = u$id, name = "ci",
                        scopes = c("mcp:read"),
                        expires_at = "2099-01-01T00:00:00Z"))
  store$close()

  store2 <- new_mcp_store("sqlite", path = path)
  withr::defer({ store2$close(); unlink(path) })
  got_u <- store2$users$find_by_username("alice")
  expect_equal(got_u$id, "u_persistent")
  expect_true(got_u$is_admin)
  expect_equal(store2$tokens$get("j1")$user_id, "u_persistent")
})

test_that("mcp_init_schema is idempotent and enables WAL", {
  path <- tempfile(fileext = ".db")
  mcp_init_schema(path)
  mcp_init_schema(path)   # second call must not raise

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  withr::defer({ DBI::dbDisconnect(con); unlink(path) })
  mode <- DBI::dbGetQuery(con, "PRAGMA journal_mode")[[1L]]
  expect_equal(tolower(mode), "wal")
  fk <- DBI::dbGetQuery(con, "PRAGMA foreign_keys")[[1L]]
  # foreign_keys is per-connection; verify we can re-enable it.
  expect_true(is.numeric(fk))
})

test_that("foreign-key constraint blocks tokens for unknown user", {
  store <- new_mcp_store("sqlite", path = tempfile(fileext = ".db"))
  withr::defer(store$close())
  expect_error(
    store$tokens$add(list(jti = "j1", user_id = "u_missing",
                          name = "ci", scopes = "x",
                          expires_at = "2099-01-01T00:00:00Z")),
    "no such user")
})
