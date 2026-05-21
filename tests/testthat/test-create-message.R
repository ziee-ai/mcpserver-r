# Sampling/createMessage capability checks ported from
# packages/server/test/server.test.ts "createMessage validation".
# These tests exercise the server's gate on sampling capability and
# the message shape; they don't drive a full round-trip.

test_that("request_sampling requires the client to declare sampling capability", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  # No capabilities — sampling MUST fail.
  s$client_capabilities <- list()
  err <- tryCatch(
    mcpserver:::request_sampling_impl(s,
      messages = list(list(role = "user",
                            content = list(type = "text",
                                            text = "hi"))),
      max_tokens = 64L,
      timeout = 1),
    error = function(e) conditionMessage(e))
  expect_match(err, "sampling capability", fixed = TRUE)
})

test_that("request_sampling sends a sampling/createMessage envelope when capability declared", {
  srv <- new_server("t")
  sent <- new.env(parent = emptyenv()); sent$msgs <- list()
  s <- mcpserver:::Session$new("t", srv, function(e) {
    sent$msgs <- c(sent$msgs, list(e))
  })
  s$client_capabilities <- list(sampling = list())
  err <- tryCatch(
    mcpserver:::request_sampling_impl(s,
      messages = list(list(role = "user",
                            content = list(type = "text",
                                            text = "hi"))),
      max_tokens = 64L,
      timeout = 0.05),
    error = function(e) conditionMessage(e))
  expect_match(err, "timed out", fixed = TRUE)
  expect_gte(length(sent$msgs), 1L)
  expect_equal(sent$msgs[[1L]]$method, "sampling/createMessage")
  expect_equal(sent$msgs[[1L]]$params$maxTokens, 64L)
})

test_that("request_elicitation requires client to declare elicitation capability", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  s$client_capabilities <- list()
  err <- tryCatch(
    mcpserver:::request_elicitation_impl(s,
      message = "?",
      requested_schema = list(type = "object"),
      timeout = 1),
    error = function(e) conditionMessage(e))
  expect_match(err, "elicitation capability", fixed = TRUE)
})

test_that("request_roots requires client to declare roots capability", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  s$client_capabilities <- list()
  err <- tryCatch(
    mcpserver:::request_roots_impl(s, timeout = 1),
    error = function(e) conditionMessage(e))
  expect_match(err, "roots capability", fixed = TRUE)
})

test_that("sampling messages are passed through verbatim to the outgoing request", {
  srv <- new_server("t")
  sent <- new.env(parent = emptyenv()); sent$msgs <- list()
  s <- mcpserver:::Session$new("t", srv, function(e) {
    sent$msgs <- c(sent$msgs, list(e))
  })
  s$client_capabilities <- list(sampling = list())
  messages <- list(
    list(role = "user",
         content = list(type = "text", text = "hello")),
    list(role = "assistant",
         content = list(type = "text", text = "hi back")))
  tryCatch(
    mcpserver:::request_sampling_impl(s,
      messages = messages,
      max_tokens = 32L,
      timeout = 0.05),
    error = function(e) NULL)
  expect_equal(length(sent$msgs[[1L]]$params$messages), 2L)
  expect_equal(sent$msgs[[1L]]$params$messages[[1L]]$role, "user")
  expect_equal(sent$msgs[[1L]]$params$messages[[2L]]$role,
               "assistant")
})

test_that("sampling system prompt is included when supplied", {
  srv <- new_server("t")
  sent <- new.env(parent = emptyenv()); sent$msgs <- list()
  s <- mcpserver:::Session$new("t", srv, function(e) {
    sent$msgs <- c(sent$msgs, list(e))
  })
  s$client_capabilities <- list(sampling = list())
  tryCatch(
    mcpserver:::request_sampling_impl(s,
      messages = list(list(role = "user",
                            content = list(type = "text",
                                            text = "hi"))),
      system_prompt = "You are a helpful assistant.",
      max_tokens = 16L,
      timeout = 0.05),
    error = function(e) NULL)
  expect_equal(sent$msgs[[1L]]$params$systemPrompt,
               "You are a helpful assistant.")
})
