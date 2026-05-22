# Users namespace of new_mcp_store(), both drivers.

skip_on_cran()

drivers <- list(
  memory = function() new_mcp_store("memory"),
  sqlite = function() {
    skip_if_not_installed("DBI")
    skip_if_not_installed("RSQLite")
    new_mcp_store("sqlite", path = tempfile(fileext = ".db"))
  }
)

for (drv in names(drivers)) {
  local({
    drv_name <- drv
    factory  <- drivers[[drv_name]]

    test_that(sprintf("[%s] add+get roundtrips fields", drv_name), {
      store <- factory()
      withr::defer(store$close())
      u <- store$users$add(list(
        username = "alice", email = "a@x.com",
        is_admin = TRUE, groups = c("dev", "ops"),
        metadata = list(team = "platform")))
      expect_true(nzchar(u$id))
      expect_equal(u$username, "alice")
      g <- store$users$get(u$id)
      expect_equal(g$username, "alice")
      expect_equal(g$email, "a@x.com")
      expect_true(g$is_admin)
      expect_equal(sort(unlist(g$groups)), sort(c("dev", "ops")))
      expect_equal(unlist(g$metadata$team), "platform")
    })

    test_that(sprintf("[%s] find_by_username and list", drv_name), {
      store <- factory()
      withr::defer(store$close())
      store$users$add(list(username = "alice"))
      store$users$add(list(username = "bob"))
      expect_equal(store$users$find_by_username("alice")$username, "alice")
      expect_null(store$users$find_by_username("nobody"))
      expect_equal(length(store$users$list()), 2L)
    })

    test_that(sprintf("[%s] username uniqueness", drv_name), {
      store <- factory()
      withr::defer(store$close())
      store$users$add(list(username = "alice"))
      expect_error(store$users$add(list(username = "alice")),
                   "alice")
    })

    test_that(sprintf("[%s] update rewrites fields and refreshes index",
                      drv_name), {
      store <- factory()
      withr::defer(store$close())
      u <- store$users$add(list(username = "alice"))
      u2 <- store$users$update(u$id, list(username = "alice2",
                                          email = "x@y.com"))
      expect_equal(u2$username, "alice2")
      expect_equal(u2$email, "x@y.com")
      expect_null(store$users$find_by_username("alice"))
      expect_equal(store$users$find_by_username("alice2")$id, u$id)
    })

    test_that(sprintf("[%s] update detects username collisions",
                      drv_name), {
      store <- factory()
      withr::defer(store$close())
      a <- store$users$add(list(username = "alice"))
      b <- store$users$add(list(username = "bob"))
      expect_error(store$users$update(b$id, list(username = "alice")),
                   "alice")
    })

    test_that(sprintf("[%s] delete cascades tokens", drv_name), {
      store <- factory()
      withr::defer(store$close())
      u <- store$users$add(list(username = "alice"))
      store$tokens$add(list(jti = "j1", user_id = u$id, name = "ci",
                            scopes = "mcp:read",
                            expires_at = "2099-01-01T00:00:00Z"))
      expect_true(store$users$delete(u$id))
      expect_null(store$users$get(u$id))
      expect_null(store$tokens$get("j1"))
      expect_false(store$users$delete(u$id))   # idempotent
    })
  })
}
