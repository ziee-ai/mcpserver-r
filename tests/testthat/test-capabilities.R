test_that("server only advertises capabilities for kinds actually registered", {
  srv <- new_server("t")
  caps <- srv$capabilities()
  # No tools / resources / prompts registered yet.
  expect_null(caps$tools)
  expect_null(caps$resources)
  expect_null(caps$prompts)
  expect_null(caps$completions)
  # Logging is always available.
  expect_true(!is.null(caps$logging))

  add_capability(srv, new_tool(
    "x", "x", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  caps2 <- srv$capabilities()
  expect_true(!is.null(caps2$tools))
  expect_true(isTRUE(caps2$tools$listChanged))
  expect_null(caps2$resources)  # still none
})

test_that("completions capability surfaces only when a prompt declares complete()", {
  srv <- new_server("t")
  add_capability(srv, new_prompt(
    "p", "p", arguments = list(),
    handler = function(args, ctx) "ok"))
  expect_null(srv$capabilities()$completions)

  srv2 <- new_server("t")
  add_capability(srv2, new_prompt(
    "p", "p",
    arguments = list(new_prompt_argument("x")),
    complete = list(x = function(value, ctx, args) c("a", "b")),
    handler = function(args, ctx) "ok"))
  expect_true(!is.null(srv2$capabilities()$completions))
})

test_that("user-declared capabilities override generated ones", {
  srv <- new_server("t", capabilities = list(
    experimental = list(tasks = list(list = TRUE))))
  caps <- srv$capabilities()
  expect_true(!is.null(caps$experimental))
  expect_true(isTRUE(caps$experimental$tasks$list))
})
