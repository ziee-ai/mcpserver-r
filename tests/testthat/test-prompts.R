# Unit tests for prompt registration + dispatch.

test_that("prompts/list returns registered prompts", {
  srv <- new_server("t")
  add_capability(srv, new_prompt(
    name = "greet", description = "Greet",
    arguments = list(new_prompt_argument("name", required = TRUE)),
    handler = function(args, ctx) sprintf("hi %s", args$name)))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  res <- mcpserver:::handle_prompts_list(srv, s, list())
  names <- vapply(res$prompts, function(p) p$name, character(1L))
  expect_true("greet" %in% names)
})

test_that("prompts/get on unknown name returns -32602", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::handle_prompts_get(srv, s,
    list(name = "nope"), list(id = 1))
  expect_equal(resp$error$code, -32602)
})

test_that("prompts/get missing required argument returns -32602", {
  srv <- new_server("t")
  add_capability(srv, new_prompt(
    name = "p",
    description = "p",
    arguments = list(new_prompt_argument("city", required = TRUE)),
    handler = function(args, ctx) sprintf("hi %s", args$city)))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::handle_prompts_get(srv, s,
    list(name = "p", arguments = list()), list(id = 1))
  expect_equal(resp$error$code, -32602)
})

test_that("prompts/get with valid args dispatches handler", {
  srv <- new_server("t")
  add_capability(srv, new_prompt(
    name = "p", description = "p",
    arguments = list(new_prompt_argument("city", required = TRUE)),
    handler = function(args, ctx) sprintf("hi %s", args$city)))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  marker <- mcpserver:::handle_prompts_get(srv, s,
    list(name = "p", arguments = list(city = "Boston")),
    list(id = 1))
  expect_true(isTRUE(marker$.prompt_call))
  expect_equal(marker$args$city, "Boston")
})

test_that("finalize_prompt_result wraps a bare string into messages", {
  p <- new_prompt(
    name = "p", description = "p",
    arguments = list(),
    handler = function(args, ctx) "Hello")
  out <- mcpserver:::finalize_prompt_result(p, "Hello")
  expect_equal(out$messages[[1L]]$role, "user")
  expect_equal(out$messages[[1L]]$content$text, "Hello")
})

test_that("cascading completion routes through context.arguments", {
  srv <- new_server("t")
  add_capability(srv, new_prompt(
    name = "p",
    description = "p",
    arguments = list(
      new_prompt_argument("dept", required = TRUE),
      new_prompt_argument("name", required = TRUE)),
    complete = list(
      name = function(value, ctx, args) {
        opts <- if (identical(args$dept, "Eng"))
          c("Alice", "Anna") else c("Bob")
        grep(value, opts, value = TRUE)
      }),
    handler = function(args, ctx) "ok"))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  res <- mcpserver:::handle_completion(srv, s,
    list(
      ref = list(type = "ref/prompt", name = "p"),
      argument = list(name = "name", value = "A"),
      context = list(arguments = list(dept = "Eng"))),
    list(id = 1))
  expect_true("Alice" %in% res$completion$values)
  expect_false("Bob" %in% res$completion$values)
})
