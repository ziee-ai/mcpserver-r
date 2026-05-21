# Wire shape of tool descriptors: top-level `title`, `_meta`, and
# `execution.taskSupport` per the 2025-06-18 spec and TS SDK v1.29.0.

new_simple_tool <- function(name = "k", ...) {
  args <- list(name = name,
               description = "test",
               input_schema = schema(list()),
               handler = function(args, ctx) response_text("ok"))
  args[names(list(...))] <- list(...)
  do.call(new_tool, args)
}

descriptor_for <- function(t) {
  srv <- new_server("t")
  add_capability(srv, t)
  s <- mcpserver:::Session$new("s", srv, function(e) NULL)
  res <- mcpserver:::handle_tools_list(srv, s, list())
  res$tools[[1L]]
}

test_that("tool_descriptor emits a top-level `title` when set", {
  d <- descriptor_for(new_simple_tool(title = "Display Name"))
  expect_equal(d$title, "Display Name")
})

test_that("tool_descriptor omits `title` when not set", {
  d <- descriptor_for(new_simple_tool())
  expect_null(d$title)
})

test_that("tool_descriptor rejects a non-scalar title", {
  expect_error(new_simple_tool(title = c("a", "b")),
               "scalar character")
})

test_that("tool with `tasks = TRUE` emits execution.taskSupport='optional'", {
  # R's task-mode tools work via either tools/call (sync) or task-
  # augmented streaming — "optional" tells clients they MAY use the
  # streaming path but are not required to.
  d <- descriptor_for(new_simple_tool(tasks = TRUE))
  expect_equal(d$execution$taskSupport, "optional")
})

test_that("tool with `tasks = FALSE` omits execution.taskSupport", {
  d <- descriptor_for(new_simple_tool(tasks = FALSE))
  expect_null(d$execution)
})

test_that("tool_descriptor emits `_meta` when set", {
  d <- descriptor_for(new_simple_tool(meta = list(authoredBy = "qa")))
  expect_equal(d$`_meta`$authoredBy, "qa")
})

test_that("tool_descriptor omits `_meta` when not set", {
  d <- descriptor_for(new_simple_tool())
  expect_null(d$`_meta`)
})

test_that("new_tool rejects a non-named-list meta", {
  expect_error(new_simple_tool(meta = list("x", "y")),
               "named list")
})

test_that("title coexists with the legacy annotations$title (no clobber)", {
  d <- descriptor_for(new_simple_tool(
    title = "Pretty",
    annotations = list(title = "Legacy Pretty", readOnlyHint = TRUE)))
  expect_equal(d$title, "Pretty")
  expect_equal(d$annotations$title, "Legacy Pretty")
  expect_true(isTRUE(d$annotations$readOnlyHint))
})

test_that("tools/list response wraps the new fields per tool", {
  srv <- new_server("t")
  add_capability(srv, new_simple_tool("a", title = "Alpha",
                                      tasks = TRUE,
                                      meta = list(team = "x")))
  add_capability(srv, new_simple_tool("b"))
  s <- mcpserver:::Session$new("s", srv, function(e) NULL)
  res <- mcpserver:::handle_tools_list(srv, s, list())
  by_name <- setNames(res$tools, vapply(res$tools, function(t) t$name,
                                        character(1L)))
  expect_equal(by_name$a$title, "Alpha")
  expect_equal(by_name$a$execution$taskSupport, "optional")
  expect_equal(by_name$a$`_meta`$team, "x")
  expect_null(by_name$b$title)
  expect_null(by_name$b$execution)
  expect_null(by_name$b$`_meta`)
})

# Resource + resource template descriptors -------------------------------

test_that("resource_descriptor emits top-level `title` when set", {
  srv <- new_server("t")
  add_capability(srv, new_resource(
    "r", "desc", "test://r", title = "Pretty Name",
    handler = function(p, c) "x"))
  s <- mcpserver:::Session$new("s", srv, function(e) NULL)
  res <- mcpserver:::handle_resources_list(srv, s, list())
  expect_equal(res$resources[[1L]]$title, "Pretty Name")
})

test_that("resource_template_descriptor emits top-level `title` when set", {
  srv <- new_server("t")
  add_capability(srv, new_resource_template(
    "rt", "desc", "test://r/{id}", title = "Pretty Template",
    handler = function(p, c) "x"))
  s <- mcpserver:::Session$new("s", srv, function(e) NULL)
  res <- mcpserver:::handle_resources_templates_list(srv, s, list())
  expect_equal(res$resourceTemplates[[1L]]$title, "Pretty Template")
})

test_that("resource without title omits the field", {
  srv <- new_server("t")
  add_capability(srv, new_resource(
    "r", "desc", "test://r", handler = function(p, c) "x"))
  s <- mcpserver:::Session$new("s", srv, function(e) NULL)
  res <- mcpserver:::handle_resources_list(srv, s, list())
  expect_null(res$resources[[1L]]$title)
})

# Prompt descriptor -------------------------------------------------------

test_that("prompt_descriptor emits top-level `title` when set", {
  srv <- new_server("t")
  add_capability(srv, new_prompt(
    "p", "desc", arguments = list(), title = "Pretty Prompt",
    handler = function(args, ctx) "ok"))
  s <- mcpserver:::Session$new("s", srv, function(e) NULL)
  res <- mcpserver:::handle_prompts_list(srv, s, list())
  expect_equal(res$prompts[[1L]]$title, "Pretty Prompt")
})

test_that("prompt without title omits the field", {
  srv <- new_server("t")
  add_capability(srv, new_prompt(
    "p", "desc", arguments = list(),
    handler = function(args, ctx) "ok"))
  s <- mcpserver:::Session$new("s", srv, function(e) NULL)
  res <- mcpserver:::handle_prompts_list(srv, s, list())
  expect_null(res$prompts[[1L]]$title)
})

test_that("resource/template/prompt all reject non-scalar title", {
  expect_error(new_resource("r", "d", "test://r",
                            title = c("a", "b"),
                            handler = function(p, c) "x"))
  expect_error(new_resource_template("rt", "d", "test://r/{id}",
                                     title = c("a", "b"),
                                     handler = function(p, c) "x"))
  expect_error(new_prompt("p", "d", title = c("a", "b"),
                          handler = function(args, ctx) "ok"))
})
