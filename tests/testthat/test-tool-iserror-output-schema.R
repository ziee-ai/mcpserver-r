# When a tool returns isError = TRUE, the outputSchema MUST NOT be
# applied to the result's structuredContent — the error payload is
# allowed to violate the success schema. Mirrors TS SDK
# mcp.test.ts > "should skip outputSchema validation when isError is true".

new_typed_tool_server <- function() {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    name = "k", description = "k",
    input_schema = schema(list()),
    output_schema = schema(list(
      temperature = property_number(required = TRUE))),
    handler = function(args, ctx) response_error("boom")))
  srv
}

test_that("isError result does NOT trip outputSchema validation", {
  srv <- new_typed_tool_server()
  t <- get("k", envir = srv$tools, inherits = FALSE)
  # The handler returns a response_error which sets isError = TRUE
  # and content with no structuredContent.
  res <- mcpserver:::finalize_tool_result(t,
    mcpserver::response_error("boom"))
  expect_true(isTRUE(res$isError))
  # The error message is in content; structuredContent is absent.
  expect_null(res$structuredContent)
  # The outputSchema validator was not invoked (no validation-fail
  # message in content).
  text <- res$content[[1L]]$text %||% ""
  expect_false(grepl("output_schema", text))
})

test_that("non-isError result with mismatched structuredContent IS still validated", {
  srv <- new_typed_tool_server()
  t <- get("k", envir = srv$tools, inherits = FALSE)
  res <- mcpserver:::finalize_tool_result(t,
    mcpserver::response_structured(list(),
      list(temperature = "not-a-number")))
  # Should have flipped to isError = TRUE with the validation message.
  expect_true(isTRUE(res$isError))
  text <- res$content[[1L]]$text %||% ""
  expect_match(text, "output_schema")
})

test_that("isError result with structuredContent passes through unchanged", {
  srv <- new_typed_tool_server()
  t <- get("k", envir = srv$tools, inherits = FALSE)
  # Manually craft a result that has BOTH isError = TRUE and a
  # structuredContent that wouldn't satisfy the output_schema. This
  # is what a smart tool returns when it wants to surface a typed
  # error payload alongside the human-readable error message.
  res <- mcpserver:::finalize_tool_result(t,
    list(content = list(mcpserver::response_text("error: boom")),
         structuredContent = list(reason = "boom"),
         isError = TRUE))
  expect_true(isTRUE(res$isError))
  expect_equal(res$structuredContent$reason, "boom")
})
