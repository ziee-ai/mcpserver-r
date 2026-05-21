test_that("mcp_error round-trips code and data through the dispatcher", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    name = "boom",
    description = "throws an mcp_error",
    input_schema = schema(list()),
    handler = function(args, ctx) {
      stop(mcp_error("bad params",
                     code = mcp_error_codes()$invalid_params,
                     data = list(arg = "x")))
    }))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "initialize",
    params = list(protocolVersion = "2025-06-18", capabilities = list())))
  marker <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 2, method = "tools/call",
    params = list(name = "boom", arguments = list())))
  p <- mcpserver:::finalize_async(marker, srv, s)
  env <- new.env(parent = emptyenv()); env$done <- FALSE
  promises::then(p, function(v) { env$val <- v; env$done <- TRUE },
                    function(e) { env$err <- e; env$done <- TRUE })
  t0 <- Sys.time()
  while (!env$done && difftime(Sys.time(), t0, units = "secs") < 10) {
    later::run_now(timeoutSecs = 0.05)
  }
  expect_true(env$done)
  expect_equal(env$val$error$code, mcp_error_codes()$invalid_params)
  expect_equal(env$val$error$message, "bad params")
  expect_equal(env$val$error$data$arg, "x")
  mirai::daemons(0)
})

test_that("mcp_error_codes() exports the canonical JSON-RPC codes", {
  codes <- mcp_error_codes()
  expect_equal(codes$parse_error, -32700L)
  expect_equal(codes$invalid_request, -32600L)
  expect_equal(codes$method_not_found, -32601L)
  expect_equal(codes$invalid_params, -32602L)
  expect_equal(codes$internal_error, -32603L)
})

test_that("cancellation flag is cleared after a tool finishes (no leak)", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    name = "k", description = "k", input_schema = schema(list()),
    handler = function(args, ctx) response_text("ok")))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "initialize",
    params = list(protocolVersion = "2025-06-18", capabilities = list())))
  marker <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 7, method = "tools/call",
    params = list(name = "k", arguments = list())))
  # Flag was registered when the marker was built.
  expect_true(exists("7", envir = s$cancel, inherits = FALSE))
  p <- mcpserver:::finalize_async(marker, srv, s)
  env <- new.env(); env$done <- FALSE
  promises::then(p, function(v) env$done <- TRUE,
                    function(e) env$done <- TRUE)
  t0 <- Sys.time()
  while (!env$done && difftime(Sys.time(), t0, units = "secs") < 10) {
    later::run_now(timeoutSecs = 0.05)
  }
  # After resolution the flag should be gone.
  expect_false(exists("7", envir = s$cancel, inherits = FALSE))
  mirai::daemons(0)
})
