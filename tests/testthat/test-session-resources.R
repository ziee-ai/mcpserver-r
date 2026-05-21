# Session-scoped resource registry: tools register transient resources
# at call time; entries live on the session and are swept by
# `Session$close()`.

new_demo_session <- function() {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("s", srv, function(env) NULL)
  assign("s", s, envir = srv$sessions)
  list(server = srv, session = s)
}

test_that("session_resource_register stores a resource on the session", {
  ctx <- new_demo_session()
  mcpserver:::session_resource_register(
    ctx$session,
    uri = "demo://resource/session/x",
    mime_type = "text/plain",
    handler = function(params, ctx) "session body")
  out <- mcpserver:::session_resource_get(ctx$session,
                                          "demo://resource/session/x")
  expect_false(is.null(out))
  expect_equal(out$uri, "demo://resource/session/x")
  expect_equal(out$mime_type, "text/plain")
})

test_that("re-registering the same URI replaces the previous entry", {
  ctx <- new_demo_session()
  mcpserver:::session_resource_register(
    ctx$session, uri = "demo://resource/session/x",
    mime_type = "text/plain",
    handler = function(params, ctx) "first")
  mcpserver:::session_resource_register(
    ctx$session, uri = "demo://resource/session/x",
    mime_type = "application/gzip",
    handler = function(params, ctx) "second")
  out <- mcpserver:::session_resource_get(ctx$session,
                                          "demo://resource/session/x")
  expect_equal(out$mime_type, "application/gzip")
})

test_that("session_resource_get returns NULL for an unknown URI", {
  ctx <- new_demo_session()
  expect_null(mcpserver:::session_resource_get(ctx$session,
                                               "demo://resource/missing"))
})

test_that("Session$close sweeps the per-session registry", {
  ctx <- new_demo_session()
  mcpserver:::session_resource_register(
    ctx$session, uri = "demo://resource/session/y",
    handler = function(params, ctx) "x")
  expect_length(ls(ctx$session$session_resources,
                   all.names = TRUE), 1L)
  ctx$session$close()
  expect_length(ls(ctx$session$session_resources,
                   all.names = TRUE), 0L)
})

test_that("handle_resources_read prefers session resources over server-wide", {
  srv <- new_server("t")
  # Same URI registered server-wide (returns "server-side") and
  # session-wide (returns "session-side") — session wins.
  add_capability(srv, new_resource(
    "shared", "shared", "demo://shared", mime_type = "text/plain",
    handler = function(p, c) "server-side"))
  s <- mcpserver:::Session$new("s", srv, function(env) NULL)
  assign("s", s, envir = srv$sessions)
  mcpserver:::session_resource_register(
    s, uri = "demo://shared", mime_type = "text/plain",
    handler = function(p, c) "session-side")
  marker <- mcpserver:::handle_resources_read(srv, s,
    list(uri = "demo://shared"), list(id = 1L))
  # The marker carries the resource record — pull `name` to know which
  # side won.
  expect_equal(marker$resource$handler(list(uri = "demo://shared"), NULL),
               "session-side")
})

test_that("session-scoped resource is readable through resources/read", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("s", srv, function(env) NULL)
  assign("s", s, envir = srv$sessions)
  mcpserver:::session_resource_register(
    s, uri = "demo://resource/session/hello",
    mime_type = "text/plain",
    handler = function(p, c) "hello session")
  marker <- mcpserver:::handle_resources_read(srv, s,
    list(uri = "demo://resource/session/hello"), list(id = 1L))
  expect_true(isTRUE(marker$.resource_call))
  expect_equal(marker$mime_type, "text/plain")
})

test_that("resources/read on an unregistered session URI falls through", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("s", srv, function(env) NULL)
  assign("s", s, envir = srv$sessions)
  resp <- mcpserver:::handle_resources_read(srv, s,
    list(uri = "demo://resource/session/nope"), list(id = 1L))
  # Returns an error envelope, not a resource_call marker.
  expect_equal(resp$error$code, -32602)
})
