test_that("set_request_handler installs a custom request method", {
  srv <- new_server("t")
  set_request_handler(srv, "experimental/echo",
    function(server, session, params, msg) {
      list(echoed = params$text %||% "")
    })
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "experimental/echo",
    params = list(text = "hi")))
  expect_equal(resp$result$echoed, "hi")
})

test_that("set_notification_handler observes incoming notifications", {
  srv <- new_server("t")
  seen <- new.env(parent = emptyenv()); seen$value <- NULL
  set_notification_handler(srv, "notifications/x",
    function(server, session, params) {
      seen$value <- params$payload
    })
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", method = "notifications/x",
    params = list(payload = "ok")))
  expect_equal(seen$value, "ok")
})

test_that("custom request handlers override built-in method-not-found", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  before <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "no/such"))
  expect_equal(before$error$code, -32601)
  set_request_handler(srv, "no/such",
    function(server, session, params, msg) list(ok = TRUE))
  after <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "no/such"))
  expect_true(isTRUE(after$result$ok))
})

test_that("fallback handler catches otherwise-unknown methods", {
  srv <- new_server("t")
  set_fallback_request_handler(srv,
    function(server, session, params, msg) {
      list(method = msg$method)
    })
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  res <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "rand/x"))
  expect_equal(res$result$method, "rand/x")
})
