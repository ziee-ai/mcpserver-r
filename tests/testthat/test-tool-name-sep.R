test_that("new_tool rejects names that violate SEP-986", {
  expect_error(new_tool(
    name = "tool with spaces",
    description = "x", input_schema = schema(list()),
    handler = function(args, ctx) response_text("ok")),
    "SEP-986")
  expect_error(new_tool(
    name = "tool$weird",
    description = "x", input_schema = schema(list()),
    handler = function(args, ctx) response_text("ok")),
    "SEP-986")
  expect_error(new_tool(
    name = paste0(rep("a", 65L), collapse = ""),
    description = "x", input_schema = schema(list()),
    handler = function(args, ctx) response_text("ok")),
    "SEP-986")
})

test_that("new_tool accepts names matching the SEP-986 character set", {
  for (nm in c("echo", "get-sum", "ns.tool", "get_env", "ns/sub",
               "a.b/c-d_e")) {
    t <- new_tool(name = nm, description = "x",
                  input_schema = schema(list()),
                  handler = function(args, ctx) response_text("ok"))
    expect_equal(t$name, nm)
  }
})
