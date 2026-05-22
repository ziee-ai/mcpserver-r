# Tokens namespace of new_mcp_store(), both drivers.

skip_on_cran()

drivers <- list(
  memory = function() new_mcp_store("memory"),
  sqlite = function() {
    skip_if_not_installed("DBI")
    skip_if_not_installed("RSQLite")
    new_mcp_store("sqlite", path = tempfile(fileext = ".db"))
  }
)

far_future <- "2099-01-01T00:00:00Z"

for (drv in names(drivers)) {
  local({
    drv_name <- drv
    factory  <- drivers[[drv_name]]

    test_that(sprintf("[%s] add+get roundtrips scopes", drv_name), {
      store <- factory()
      withr::defer(store$close())
      u <- store$users$add(list(username = "alice"))
      t <- store$tokens$add(list(jti = "j1", user_id = u$id, name = "ci",
                                 scopes = c("mcp:read", "mcp:write"),
                                 expires_at = far_future))
      g <- store$tokens$get("j1")
      expect_equal(g$jti, "j1")
      expect_equal(g$user_id, u$id)
      expect_equal(sort(g$scopes), sort(c("mcp:read", "mcp:write")))
      expect_false(g$revoked)
    })

    test_that(sprintf("[%s] (user_id, name) uniqueness", drv_name), {
      store <- factory()
      withr::defer(store$close())
      u <- store$users$add(list(username = "alice"))
      store$tokens$add(list(jti = "j1", user_id = u$id, name = "ci",
                            scopes = "mcp:read",
                            expires_at = far_future))
      expect_error(store$tokens$add(list(jti = "j2", user_id = u$id,
                                          name = "ci",
                                          scopes = "mcp:read",
                                          expires_at = far_future)),
                   "ci")
    })

    test_that(sprintf("[%s] list_for_user respects include_revoked",
                      drv_name), {
      store <- factory()
      withr::defer(store$close())
      u <- store$users$add(list(username = "alice"))
      store$tokens$add(list(jti = "j1", user_id = u$id, name = "a",
                            scopes = "x", expires_at = far_future))
      store$tokens$add(list(jti = "j2", user_id = u$id, name = "b",
                            scopes = "x", expires_at = far_future))
      store$tokens$revoke("j1")
      expect_equal(length(store$tokens$list_for_user(u$id)), 1L)
      expect_equal(length(store$tokens$list_for_user(u$id,
                                                    include_revoked = TRUE)),
                   2L)
    })

    test_that(sprintf("[%s] revoke is idempotent + reflected in get",
                      drv_name), {
      store <- factory()
      withr::defer(store$close())
      u <- store$users$add(list(username = "alice"))
      store$tokens$add(list(jti = "j1", user_id = u$id, name = "ci",
                            scopes = "x", expires_at = far_future))
      expect_true(store$tokens$revoke("j1"))
      expect_true(store$tokens$get("j1")$revoked)
      # Revoking an already-revoked token still returns TRUE (row exists);
      # revoking a non-existent token returns FALSE.
      expect_true(store$tokens$revoke("j1"))
      expect_false(store$tokens$revoke("nope"))
    })

    test_that(sprintf("[%s] touch_last_used updates timestamp",
                      drv_name), {
      store <- factory()
      withr::defer(store$close())
      u <- store$users$add(list(username = "alice"))
      store$tokens$add(list(jti = "j1", user_id = u$id, name = "ci",
                            scopes = "x", expires_at = far_future))
      expect_null(store$tokens$get("j1")$last_used_at)
      expect_true(store$tokens$touch_last_used("j1"))
      expect_true(nzchar(store$tokens$get("j1")$last_used_at))
      expect_false(store$tokens$touch_last_used("nope"))
    })
  })
}
