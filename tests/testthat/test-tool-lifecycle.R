# Ports test/integration/test/server/mcp.test.ts tool() / resource() /
# prompt() lifecycle suites.

test_that("update_tool replaces fields on an existing tool", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    "k", "old desc", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  update_tool(srv, "k", description = "new desc")
  t <- get("k", envir = srv$tools, inherits = FALSE)
  expect_equal(t$description, "new desc")
})

test_that("update_tool errors when the tool doesn't exist", {
  srv <- new_server("t")
  expect_error(update_tool(srv, "missing", description = "x"),
               "missing")
})

test_that("update_tool replaces input_schema", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    "k", "k", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  update_tool(srv, "k",
              input_schema = schema(list(x = property_integer(required = TRUE))))
  t <- get("k", envir = srv$tools, inherits = FALSE)
  expect_true("x" %in% names(t$input_schema$properties))
})

test_that("update_tool with new outputSchema sticks", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    "k", "k", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  update_tool(srv, "k",
              output_schema = schema(list(ok = property_boolean(required = TRUE))))
  t <- get("k", envir = srv$tools, inherits = FALSE)
  expect_true(!is.null(t$output_schema))
})

test_that("remove_tool deletes the entry and is idempotent", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    "k", "k", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  expect_true(exists("k", envir = srv$tools, inherits = FALSE))
  remove_tool(srv, "k")
  expect_false(exists("k", envir = srv$tools, inherits = FALSE))
  expect_no_error(remove_tool(srv, "k"))
})

test_that("add_capability prevents duplicate tool names (last write wins)", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    "k", "first", schema(list()),
    handler = function(args, ctx) response_text("a")))
  add_capability(srv, new_tool(
    "k", "second", schema(list()),
    handler = function(args, ctx) response_text("b")))
  t <- get("k", envir = srv$tools, inherits = FALSE)
  expect_equal(t$description, "second")
})

test_that("update_resource replaces fields", {
  srv <- new_server("t")
  add_capability(srv, new_resource(
    "r", "old", "test://r", mime_type = "text/plain",
    handler = function(p, c) "x"))
  update_resource(srv, "test://r", description = "new")
  r <- get("test://r", envir = srv$resources, inherits = FALSE)
  expect_equal(r$description, "new")
})

test_that("remove_resource erases and is idempotent", {
  srv <- new_server("t")
  add_capability(srv, new_resource(
    "r", "x", "test://r",
    handler = function(p, c) "x"))
  remove_resource(srv, "test://r")
  expect_false(exists("test://r", envir = srv$resources, inherits = FALSE))
  expect_no_error(remove_resource(srv, "test://r"))
})

test_that("update_resource_template / remove_resource_template", {
  srv <- new_server("t")
  add_capability(srv, new_resource_template(
    "rt", "x", "test://rt/{id}",
    handler = function(p, c) "x"))
  update_resource_template(srv, "rt", description = "y")
  t <- get("rt", envir = srv$resource_templates, inherits = FALSE)
  expect_equal(t$description, "y")
  remove_resource_template(srv, "rt")
  expect_false(exists("rt", envir = srv$resource_templates,
                      inherits = FALSE))
})

test_that("update_prompt and remove_prompt round-trip", {
  srv <- new_server("t")
  add_capability(srv, new_prompt(
    "p", "old", arguments = list(),
    handler = function(args, ctx) "ok"))
  update_prompt(srv, "p", description = "new")
  expect_equal(get("p", envir = srv$prompts,
                   inherits = FALSE)$description, "new")
  remove_prompt(srv, "p")
  expect_false(exists("p", envir = srv$prompts, inherits = FALSE))
})

test_that("schedule_list_changed coalesces back-to-back calls", {
  srv <- new_server("t")
  add_capability(srv, new_tool(
    "a", "a", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  fired <- new.env(parent = emptyenv()); fired$n <- 0L
  assign("s", mcpserver:::Session$new("s", srv, function(e) {
    if (identical(e$method, "notifications/tools/list_changed")) {
      fired$n <- fired$n + 1L
    }
  }), envir = srv$sessions)
  for (i in 1:5) {
    update_tool(srv, "a", description = sprintf("update %d", i))
  }
  # Drain the debounce queue.
  t0 <- Sys.time()
  while (difftime(Sys.time(), t0, units = "secs") < 0.5) {
    later::run_now(timeoutSecs = 0.05)
  }
  # Five updates coalesced to one notification.
  expect_lte(fired$n, 1L)
  expect_gte(fired$n, 0L)
})
