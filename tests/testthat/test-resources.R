# Unit tests for static + templated resources, subscribe/unsubscribe.

test_that("resources/list returns registered static resources", {
  srv <- new_server("t")
  add_capability(srv, new_resource(
    name = "doc", description = "d",
    uri = "demo://doc", mime_type = "text/plain",
    handler = function(params, ctx) "body"))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  res <- mcpserver:::handle_resources_list(srv, s, list())
  uris <- vapply(res$resources, function(r) r$uri, character(1L))
  expect_true("demo://doc" %in% uris)
})

test_that("resources/read on a static URI dispatches the handler", {
  srv <- new_server("t")
  add_capability(srv, new_resource(
    name = "doc", description = "d",
    uri = "demo://doc", mime_type = "text/plain",
    handler = function(params, ctx) "body"))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  marker <- mcpserver:::handle_resources_read(srv, s,
    list(uri = "demo://doc"), list(id = 1))
  expect_true(isTRUE(marker$.resource_call))
  expect_equal(marker$params$uri, "demo://doc")
})

test_that("resources/read on a templated URI matches and extracts variables", {
  srv <- new_server("t")
  add_capability(srv, new_resource_template(
    name = "items", description = "d",
    uri_template = "demo://item/{id}",
    mime_type = "text/plain",
    handler = function(params, ctx)
      sprintf("item %s", params$variables$id)))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  marker <- mcpserver:::handle_resources_read(srv, s,
    list(uri = "demo://item/42"), list(id = 1))
  expect_true(isTRUE(marker$.resource_call))
  expect_equal(marker$params$variables$id, "42")
})

test_that("resources/read on unknown URI returns -32602", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::handle_resources_read(srv, s,
    list(uri = "demo://nope"), list(id = 1))
  expect_equal(resp$error$code, -32602)
})

test_that("subscribe/unsubscribe round-trip", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  mcpserver:::handle_resources_subscribe(srv, s,
    list(uri = "demo://x"), NULL)
  expect_true(exists("demo://x", envir = s$subs, inherits = FALSE))
  mcpserver:::handle_resources_unsubscribe(srv, s,
    list(uri = "demo://x"), NULL)
  expect_false(exists("demo://x", envir = s$subs, inherits = FALSE))
})

test_that("templates/list returns registered URI templates", {
  srv <- new_server("t")
  add_capability(srv, new_resource_template(
    name = "items", description = "d",
    uri_template = "demo://item/{id}",
    handler = function(params, ctx) "body"))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  res <- mcpserver:::handle_resources_templates_list(srv, s, list())
  tpls <- vapply(res$resourceTemplates,
                 function(t) t$uriTemplate, character(1L))
  expect_true("demo://item/{id}" %in% tpls)
})
