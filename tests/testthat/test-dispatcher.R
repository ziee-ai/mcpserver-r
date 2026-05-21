make_test_server <- function() {
  srv <- new_server("test", version = "0.0.0")
  add_capability(srv, new_tool(
    name = "echo",
    description = "Echo",
    input_schema = schema(list(text = property_string(required = TRUE))),
    handler = function(args, ctx) response_text(args$text)))
  add_capability(srv, new_resource(
    name = "g", description = "g", uri = "demo://g",
    handler = function(params, ctx) "ok"))
  add_capability(srv, new_prompt(
    name = "simple", description = "p",
    arguments = list(),
    handler = function(args, ctx) "hi"))
  srv
}

test_that("initialize handshake returns capabilities", {
  srv <- make_test_server()
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "initialize",
    params = list(protocolVersion = "2025-06-18", capabilities = list())))
  expect_equal(resp$id, 1)
  expect_equal(resp$result$protocolVersion, "2025-06-18")
  expect_true(!is.null(resp$result$capabilities$tools))
  expect_equal(resp$result$serverInfo$name, "test")
})

test_that("unknown method returns -32601", {
  srv <- make_test_server()
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "no/such/thing"))
  expect_equal(resp$error$code, -32601)
})

test_that("tools/list returns registered tools", {
  srv <- make_test_server()
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "tools/list"))
  names <- vapply(resp$result$tools, function(t) t$name, character(1L))
  expect_true("echo" %in% names)
})

test_that("tools/call returns async marker", {
  srv <- make_test_server()
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "tools/call",
    params = list(name = "echo", arguments = list(text = "x"))))
  expect_true(isTRUE(resp$.async))
})

test_that("notifications return NULL", {
  srv <- make_test_server()
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", method = "notifications/initialized"))
  expect_null(resp)
  expect_true(s$initialized)
})
