# 2025-11-25 (SEP-1613): JSON Schema 2020-12 keyword passthrough.
# tools/list MUST preserve $schema, $id, $defs, $ref,
# additionalProperties:false, allOf, anyOf, oneOf, not, const.

test_that("schema preserves $schema, $id, $defs, $ref keywords", {
  s <- list(
    `$schema` = "https://json-schema.org/draft/2020-12/schema",
    `$id` = "https://example.com/test.json",
    type = "object",
    `$defs` = list(
      Name = list(type = "string", minLength = 1L)),
    properties = list(
      first = list(`$ref` = "#/$defs/Name"),
      last  = list(`$ref` = "#/$defs/Name")))
  json <- mcpserver:::to_json(s)
  back <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(back$`$schema`,
               "https://json-schema.org/draft/2020-12/schema")
  expect_equal(back$`$id`, "https://example.com/test.json")
  expect_true(!is.null(back$`$defs`$Name))
  expect_equal(back$properties$first$`$ref`, "#/$defs/Name")
})

test_that("schema preserves allOf / anyOf / oneOf / not / const", {
  s <- list(
    type = "object",
    properties = list(
      a = list(allOf = list(list(type = "string"),
                             list(minLength = 1L))),
      b = list(anyOf = list(list(type = "string"),
                             list(type = "integer"))),
      c = list(oneOf = list(list(const = "yes"),
                             list(const = "no"))),
      d = list(not = list(type = "null")),
      e = list(const = "fixed")))
  json <- mcpserver:::to_json(s)
  back <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(length(back$properties$a$allOf), 2L)
  expect_equal(length(back$properties$b$anyOf), 2L)
  expect_equal(length(back$properties$c$oneOf), 2L)
  expect_true(!is.null(back$properties$d$not))
  expect_equal(back$properties$e$const, "fixed")
})

test_that("tools/list emits the full 2020-12 schema unchanged", {
  rich_schema <- list(
    `$schema` = "https://json-schema.org/draft/2020-12/schema",
    type = "object",
    properties = list(
      tag = list(oneOf = list(list(const = "a"),
                                list(const = "b")))),
    additionalProperties = FALSE,
    required = I("tag"))
  srv <- new_server("t")
  add_capability(srv, new_tool("k", "k",
    input_schema = rich_schema,
    handler = function(args, ctx) response_text("ok")))
  s <- mcpserver:::Session$new("s", srv, function(e) NULL)
  res <- mcpserver:::handle_tools_list(srv, s, list())
  tool <- res$tools[[1L]]
  expect_equal(tool$inputSchema$`$schema`,
               "https://json-schema.org/draft/2020-12/schema")
  expect_false(isTRUE(tool$inputSchema$additionalProperties))
  expect_equal(length(tool$inputSchema$properties$tag$oneOf), 2L)
})
