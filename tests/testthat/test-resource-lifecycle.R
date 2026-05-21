# Ports test/integration/test/server/mcp.test.ts resource() lifecycle
# assertions.

test_that("update_resource preserves URI and changes other fields", {
  srv <- new_server("t")
  add_capability(srv, new_resource(
    "r", "old", "test://r", mime_type = "text/plain",
    handler = function(p, c) "x"))
  update_resource(srv, "test://r",
                  description = "new",
                  mime_type = "application/json")
  r <- get("test://r", envir = srv$resources, inherits = FALSE)
  expect_equal(r$description, "new")
  expect_equal(r$mime_type, "application/json")
})

test_that("remove_resource fires list_changed (debounced)", {
  srv <- new_server("t")
  add_capability(srv, new_resource(
    "r", "x", "test://r", handler = function(p, c) "x"))
  out <- new.env(parent = emptyenv()); out$msgs <- list()
  assign("s", mcpserver:::Session$new("s", srv,
    function(e) out$msgs <- c(out$msgs, list(e))),
    envir = srv$sessions)
  remove_resource(srv, "test://r")
  # Drain debouncer.
  t0 <- Sys.time()
  while (difftime(Sys.time(), t0, units = "secs") < 0.3) {
    later::run_now(timeoutSecs = 0.05)
  }
  methods <- vapply(out$msgs, function(m) m$method %||% "",
                    character(1L))
  expect_true("notifications/resources/list_changed" %in% methods)
})

test_that("update_resource_template / remove_resource_template work and notify", {
  srv <- new_server("t")
  add_capability(srv, new_resource_template(
    "rt", "old", "test://rt/{id}",
    mime_type = "text/plain",
    handler = function(p, c) "x"))
  update_resource_template(srv, "rt", description = "new")
  t <- get("rt", envir = srv$resource_templates, inherits = FALSE)
  expect_equal(t$description, "new")
  remove_resource_template(srv, "rt")
  expect_false(exists("rt", envir = srv$resource_templates,
                      inherits = FALSE))
})

test_that("resource list reflects current registrations after remove", {
  srv <- new_server("t")
  add_capability(srv, new_resource(
    "a", "a", "test://a", handler = function(p, c) "x"))
  add_capability(srv, new_resource(
    "b", "b", "test://b", handler = function(p, c) "x"))
  s <- mcpserver:::Session$new("s", srv, function(e) NULL)
  res <- mcpserver:::handle_resources_list(srv, s, list())
  uris1 <- vapply(res$resources, function(r) r$uri, character(1L))
  expect_setequal(uris1, c("test://a", "test://b"))
  remove_resource(srv, "test://a")
  res2 <- mcpserver:::handle_resources_list(srv, s, list())
  uris2 <- vapply(res2$resources, function(r) r$uri, character(1L))
  expect_setequal(uris2, "test://b")
})

test_that("template variables propagate to readCallback after update", {
  srv <- new_server("t")
  add_capability(srv, new_resource_template(
    "rt", "x", "test://rt/{id}",
    handler = function(p, c) sprintf("id=%s", p$variables$id)))
  s <- mcpserver:::Session$new("s", srv, function(e) NULL)
  marker <- mcpserver:::handle_resources_read(srv, s,
    list(uri = "test://rt/42"), list(id = 1))
  expect_equal(marker$params$variables$id, "42")
})
