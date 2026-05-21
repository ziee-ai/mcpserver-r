test_that("tool annotation hints must be scalar logicals", {
  expect_error(new_tool(
    name = "x", description = "x",
    input_schema = schema(list()),
    annotations = list(readOnlyHint = "yes"),
    handler = function(args, ctx) response_text("ok")),
    "readOnlyHint")
  expect_error(new_tool(
    name = "x", description = "x",
    input_schema = schema(list()),
    annotations = list(destructiveHint = c(TRUE, FALSE)),
    handler = function(args, ctx) response_text("ok")),
    "destructiveHint")
})

test_that("tool annotation title must be a scalar character", {
  expect_error(new_tool(
    name = "x", description = "x",
    input_schema = schema(list()),
    annotations = list(title = 42),
    handler = function(args, ctx) response_text("ok")),
    "title")
})

test_that("valid annotations are accepted and round-tripped via tool_descriptor", {
  t <- new_tool(
    name = "x", description = "x",
    input_schema = schema(list()),
    annotations = list(title = "X",
                       readOnlyHint = TRUE,
                       idempotentHint = TRUE,
                       destructiveHint = FALSE,
                       openWorldHint = FALSE),
    handler = function(args, ctx) response_text("ok"))
  d <- mcpserver:::tool_descriptor(t)
  expect_equal(d$annotations$title, "X")
  expect_true(isTRUE(d$annotations$readOnlyHint))
  expect_false(isTRUE(d$annotations$destructiveHint))
})
