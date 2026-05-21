# Ports protocol.test.ts "Debounced Notifications".
#
# Verifies that bulk lifecycle changes don't produce a notification
# storm — list_changed notifications coalesce within the debounce
# window.

drain_later <- function(seconds = 0.3) {
  t0 <- Sys.time()
  while (difftime(Sys.time(), t0, units = "secs") < seconds) {
    later::run_now(timeoutSecs = 0.05)
  }
}

count_method <- function(out_env, method) {
  sum(vapply(out_env$msgs, function(m)
    identical(m$method, method), logical(1L)))
}

setup_server_with_sink <- function() {
  srv <- new_server("t")
  out <- new.env(parent = emptyenv()); out$msgs <- list()
  s <- mcpserver:::Session$new("s", srv, function(e) {
    out$msgs <- c(out$msgs, list(e))
  })
  assign("s", s, envir = srv$sessions)
  list(srv = srv, out = out)
}

test_that("five tool updates coalesce to at most one notification", {
  e <- setup_server_with_sink()
  add_capability(e$srv, new_tool(
    "t1", "x", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  for (i in 1:5) {
    update_tool(e$srv, "t1", description = sprintf("u%d", i))
  }
  drain_later()
  expect_lte(count_method(e$out, "notifications/tools/list_changed"), 1L)
})

test_that("tool updates and prompt updates don't coalesce against each other", {
  e <- setup_server_with_sink()
  add_capability(e$srv, new_tool(
    "t1", "x", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  add_capability(e$srv, new_prompt(
    "p1", "x", arguments = list(),
    handler = function(args, ctx) "ok"))
  update_tool(e$srv, "t1", description = "u")
  update_prompt(e$srv, "p1", description = "u")
  drain_later()
  expect_gte(count_method(e$out, "notifications/tools/list_changed"), 1L)
  expect_gte(count_method(e$out, "notifications/prompts/list_changed"), 1L)
})

test_that("a notification eventually fires after the debounce window", {
  e <- setup_server_with_sink()
  add_capability(e$srv, new_tool(
    "t1", "x", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  update_tool(e$srv, "t1", description = "u")
  expect_equal(count_method(e$out, "notifications/tools/list_changed"),
               0L)  # not yet
  drain_later()
  expect_equal(count_method(e$out, "notifications/tools/list_changed"),
               1L)
})

test_that("a fresh batch after the window fires another notification", {
  e <- setup_server_with_sink()
  add_capability(e$srv, new_tool(
    "t1", "x", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  update_tool(e$srv, "t1", description = "u1")
  drain_later()
  update_tool(e$srv, "t1", description = "u2")
  drain_later()
  expect_equal(count_method(e$out, "notifications/tools/list_changed"),
               2L)
})
