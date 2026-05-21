# `sampling.tools` sub-capability gating + tool_use/tool_result content
# block invariants on outbound sampling/createMessage. Mirrors TS SDK's
# index.test.ts > createMessage validation cluster (tests 29-40).

new_pending_session <- function(caps = list(sampling = list())) {
  srv <- new_server("t")
  out <- new.env(parent = emptyenv()); out$msgs <- list()
  s <- mcpserver:::Session$new("s", srv,
    function(env) out$msgs <- c(out$msgs, list(env)))
  s$client_capabilities <- caps
  list(server = srv, session = s, outgoing = out)
}

# Capability gate ---------------------------------------------------------

test_that("sampling/createMessage without `tools` works on bare sampling capability", {
  stub <- new_pending_session(list(sampling = list()))
  # Schedule a synthetic reply so the blocking call returns.
  later::later(function() {
    out <- stub$outgoing$msgs
    if (length(out) == 0L) return()
    rid <- out[[length(out)]]$id
    stub$session$resolve_pending(rid, list(role = "assistant"),
                                 is_error = FALSE)
  }, 0.05)
  res <- mcpserver:::request_sampling_impl(stub$session,
    messages = list(list(role = "user",
                         content = list(type = "text", text = "hi"))),
    max_tokens = 8L, timeout = 2)
  expect_equal(res$role, "assistant")
})

test_that("sampling/createMessage with `tools` requires sampling.tools sub-capability", {
  stub <- new_pending_session(list(sampling = list()))
  expect_error(mcpserver:::request_sampling_impl(stub$session,
    messages = list(list(role = "user",
                         content = list(type = "text", text = "hi"))),
    max_tokens = 8L, timeout = 2,
    tools = list(list(name = "calc",
                      description = "math",
                      input_schema = list()))),
    "sampling.tools capability")
})

test_that("sampling/createMessage with `tool_choice` requires sampling.tools sub-capability", {
  stub <- new_pending_session(list(sampling = list()))
  expect_error(mcpserver:::request_sampling_impl(stub$session,
    messages = list(list(role = "user",
                         content = list(type = "text", text = "hi"))),
    max_tokens = 8L, timeout = 2,
    tool_choice = list(type = "any")),
    "sampling.tools capability")
})

test_that("sampling/createMessage with `tools` succeeds when sampling.tools is declared", {
  stub <- new_pending_session(list(sampling = list(tools = list())))
  later::later(function() {
    out <- stub$outgoing$msgs
    if (length(out) == 0L) return()
    rid <- out[[length(out)]]$id
    stub$session$resolve_pending(rid, list(role = "assistant",
                                            content = list(type = "text",
                                                           text = "ok")),
                                 is_error = FALSE)
  }, 0.05)
  res <- mcpserver:::request_sampling_impl(stub$session,
    messages = list(list(role = "user",
                         content = list(type = "text", text = "hi"))),
    max_tokens = 8L, timeout = 2,
    tools = list(list(name = "calc",
                      description = "math",
                      input_schema = list())))
  expect_equal(res$role, "assistant")
})

# Message-shape invariants -----------------------------------------------

test_that("messages must be a list", {
  expect_error(mcpserver:::validate_sampling_messages("not a list"))
})

test_that("each message must have role 'user' or 'assistant'", {
  expect_error(mcpserver:::validate_sampling_messages(list(
    list(role = "system",
         content = list(type = "text", text = "x")))),
    "must be 'user' or 'assistant'")
})

test_that("tool_use in an assistant message demands tool_result in the next user message", {
  msgs <- list(
    list(role = "assistant",
         content = list(
           list(type = "tool_use", id = "t1", name = "calc",
                input = list(a = 1)))),
    # Missing user message with tool_result.
    list(role = "assistant",
         content = list(type = "text", text = "no result block here")))
  expect_error(mcpserver:::validate_sampling_messages(msgs),
               "next message")
})

test_that("tool_result ids must match the prior tool_use ids", {
  msgs <- list(
    list(role = "assistant",
         content = list(
           list(type = "tool_use", id = "t1", name = "calc",
                input = list()))),
    list(role = "user",
         content = list(
           list(type = "tool_result", tool_use_id = "wrong-id",
                content = "42"))))
  expect_error(mcpserver:::validate_sampling_messages(msgs),
               "tool_use|tool_result")
})

test_that("matched tool_use/tool_result pair is accepted", {
  msgs <- list(
    list(role = "user",
         content = list(type = "text", text = "what's 1+1?")),
    list(role = "assistant",
         content = list(
           list(type = "tool_use", id = "t1", name = "calc",
                input = list(a = 1, b = 1)))),
    list(role = "user",
         content = list(
           list(type = "tool_result", tool_use_id = "t1",
                content = "2"))))
  expect_silent(mcpserver:::validate_sampling_messages(msgs))
})

test_that("missing some tool_results for parallel tool_use is rejected", {
  msgs <- list(
    list(role = "assistant",
         content = list(
           list(type = "tool_use", id = "t1", name = "a", input = list()),
           list(type = "tool_use", id = "t2", name = "b", input = list()))),
    list(role = "user",
         content = list(
           list(type = "tool_result", tool_use_id = "t1",
                content = "ok"))))
  expect_error(mcpserver:::validate_sampling_messages(msgs),
               "tool_result")
})

test_that("extra tool_result without a matching tool_use is rejected", {
  msgs <- list(
    list(role = "assistant",
         content = list(
           list(type = "tool_use", id = "t1", name = "a", input = list()))),
    list(role = "user",
         content = list(
           list(type = "tool_result", tool_use_id = "t1",
                content = "ok"),
           list(type = "tool_result", tool_use_id = "orphan",
                content = "??"))))
  expect_error(mcpserver:::validate_sampling_messages(msgs),
               "without matching tool_use")
})

test_that("empty messages list passes validation", {
  expect_silent(mcpserver:::validate_sampling_messages(list()))
})

test_that("text-only assistant message without tool_use is accepted", {
  msgs <- list(
    list(role = "user",
         content = list(type = "text", text = "hello")),
    list(role = "assistant",
         content = list(type = "text", text = "hi back")))
  expect_silent(mcpserver:::validate_sampling_messages(msgs))
})
