# Unit tests for tool registration + dispatch helpers.

test_that("tool_descriptor includes optional output_schema and annotations", {
  t <- new_tool(
    name = "t",
    description = "t",
    input_schema = schema(list(x = property_integer())),
    output_schema = schema(list(y = property_integer())),
    annotations = list(readOnlyHint = TRUE),
    handler = function(args, ctx) response_text("ok"))
  d <- mcpserver:::tool_descriptor(t)
  expect_equal(d$name, "t")
  expect_true(!is.null(d$outputSchema))
  expect_true(!is.null(d$annotations))
})

test_that("unknown tool name returns -32602", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::handle_tools_call(srv, s,
    list(name = "no-such-tool"), list(id = 1))
  expect_equal(resp$error$code, -32602)
})

test_that("invalid arguments are rejected with -32602", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    name = "k",
    description = "k",
    input_schema = schema(list(x = property_integer(required = TRUE))),
    handler = function(args, ctx) response_text("ok")))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  # x missing
  resp <- mcpserver:::handle_tools_call(srv, s,
    list(name = "k", arguments = list()), list(id = 1))
  expect_equal(resp$error$code, -32602)
  expect_match(resp$error$message, "invalid")
})

test_that("finalize_tool_result rejects structuredContent that fails output_schema", {
  t <- new_tool(
    name = "t",
    description = "t",
    input_schema = schema(list()),
    output_schema = schema(list(
      ok = property_boolean(required = TRUE))),
    handler = function(args, ctx) response_text("ok"))
  # value carries wrong-typed structuredContent
  bad <- list(content = list(response_text("x")),
              structuredContent = list(ok = "not-a-bool"))
  out <- mcpserver:::finalize_tool_result(t, bad)
  expect_true(isTRUE(out$isError))
  expect_match(out$content[[1L]]$text, "output_schema",
               fixed = TRUE)
})
