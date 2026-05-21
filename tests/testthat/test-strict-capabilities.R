# strict_capabilities: refuse to register handlers for methods whose
# capability hasn't been declared.

test_that("strict mode rejects handlers for undeclared capability roots", {
  srv <- new_server("t", strict_capabilities = TRUE)
  expect_error(
    set_request_handler(srv, "tools/myCustomCall",
                        function(server, session, params, msg)
                          list(ok = TRUE)),
    "strict_capabilities")
})

test_that("strict mode allows handlers for declared capabilities", {
  srv <- new_server("t", strict_capabilities = TRUE)
  # Adding a tool surfaces the 'tools' capability.
  add_capability(srv, new_tool(
    "x", "x", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  expect_no_error(
    set_request_handler(srv, "tools/myCustomCall",
                        function(server, session, params, msg)
                          list(ok = TRUE)))
})

test_that("strict mode allows handlers for built-in methods (initialize/ping/notifications)", {
  srv <- new_server("t", strict_capabilities = TRUE)
  expect_no_error(
    set_request_handler(srv, "ping",
                        function(server, session, params, msg)
                          list()))
  expect_no_error(
    set_request_handler(srv, "notifications/customClient",
                        function(server, session, params, msg)
                          list()))
})

test_that("non-strict mode (default) allows any custom handler", {
  srv <- new_server("t")
  expect_no_error(
    set_request_handler(srv, "anything/at/all",
                        function(server, session, params, msg)
                          list(ok = TRUE)))
})
