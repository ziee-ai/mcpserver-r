# SEP-1686 outbound client-task helper. The helper drives the
# transport-thread round-trip; we exercise its behaviour with a
# stubbed session that records outgoing envelopes and synthesises
# the matching responses.

# Stub-session factory ---------------------------------------------------

# A reactive stub session: each time the helper writes an envelope to
# the wire (via `session$write_fn`), the stub looks up the script
# entry for that ordinal request and resolves it on the `later` loop.
# This way each response only fires *after* its corresponding request
# went out, regardless of the helper's polling cadence.

new_stub_session <- function(caps = list(tasks = list(requests = list(
                                          sampling = list(createMessage = list()),
                                          elicitation = list(create = list())))),
                             script = list()) {
  srv <- new_server("t")
  bag <- new.env(parent = emptyenv())
  bag$out <- list()
  bag$script <- script
  # write_fn captures the session via promise — we set it after.
  s_holder <- new.env(parent = emptyenv())
  write_fn <- function(env) {
    bag$out <- c(bag$out, list(env))
    idx <- length(bag$out)
    if (idx <= length(bag$script)) {
      payload <- bag$script[[idx]]
      rid <- env$id
      sess <- s_holder$session
      later::later(function() {
        sess$resolve_pending(rid, payload, is_error = FALSE)
      }, 0.01)
    }
  }
  s <- mcpserver:::Session$new("t", srv, write_fn)
  s$client_capabilities <- caps
  s_holder$session <- s
  list(session = s, outgoing = bag)
}

# Tests ------------------------------------------------------------------

test_that("call_client_task injects params.task = { ttl } in milliseconds", {
  stub <- new_stub_session(script = list(
    list(task = list(taskId = "T1", status = "working",
                     ttl = 30000L, pollInterval = 50L)),
    list(task = list(taskId = "T1", status = "completed")),
    list(reply = "ok")))
  res <- mcpserver:::call_client_task(stub$session,
    "sampling/createMessage", list(messages = list()),
    ttl = 5, poll_interval = 0.05, total_timeout = 5)
  expect_equal(res$reply, "ok")
  expect_equal(stub$outgoing$out[[1L]]$method, "sampling/createMessage")
  expect_equal(stub$outgoing$out[[1L]]$params$task$ttl, 5000L)
})

test_that("call_client_task polls tasks/get and finally calls tasks/result", {
  stub <- new_stub_session(script = list(
    list(task = list(taskId = "T2", status = "working")),
    list(task = list(taskId = "T2", status = "completed")),
    list(content = list(answer = "yes"))))
  res <- mcpserver:::call_client_task(stub$session,
    "elicitation/create", list(message = "hi"),
    ttl = 5, poll_interval = 0.05, total_timeout = 5)
  methods <- vapply(stub$outgoing$out, function(e) e$method, character(1L))
  expect_equal(methods,
               c("elicitation/create", "tasks/get", "tasks/result"))
  expect_equal(stub$outgoing$out[[2L]]$params$taskId, "T2")
  expect_equal(stub$outgoing$out[[3L]]$params$taskId, "T2")
  expect_equal(res$content$answer, "yes")
})

test_that("call_client_task tolerates input_required intermediate state", {
  stub <- new_stub_session(script = list(
    list(task = list(taskId = "T3", status = "working")),
    list(task = list(taskId = "T3", status = "input_required")),
    list(task = list(taskId = "T3", status = "completed")),
    list(ok = TRUE)))
  res <- mcpserver:::call_client_task(stub$session,
    "elicitation/create", list(message = "hi"),
    ttl = 5, poll_interval = 0.05, total_timeout = 5)
  expect_true(isTRUE(res$ok))
})

test_that("call_client_task raises when client returns a non-task envelope", {
  stub <- new_stub_session(script = list(
    list(content = list(answer = "no"))))
  expect_error(mcpserver:::call_client_task(stub$session,
    "elicitation/create", list(message = "hi"),
    ttl = 5, poll_interval = 0.05, total_timeout = 2),
    "non-task envelope")
})

test_that("call_client_task raises on terminal 'failed' status", {
  stub <- new_stub_session(script = list(
    list(task = list(taskId = "T4", status = "working")),
    list(task = list(taskId = "T4", status = "failed"))))
  expect_error(mcpserver:::call_client_task(stub$session,
    "sampling/createMessage", list(messages = list()),
    ttl = 5, poll_interval = 0.05, total_timeout = 5),
    "failed")
})

test_that("call_client_task raises on terminal 'cancelled' status", {
  stub <- new_stub_session(script = list(
    list(task = list(taskId = "T5", status = "working")),
    list(task = list(taskId = "T5", status = "cancelled"))))
  expect_error(mcpserver:::call_client_task(stub$session,
    "sampling/createMessage", list(messages = list()),
    ttl = 5, poll_interval = 0.05, total_timeout = 5),
    "cancelled")
})

test_that("call_client_task refuses methods the client did not declare", {
  stub <- new_stub_session(caps = list(
    tasks = list(requests = list(elicitation = list(create = list())))))
  expect_error(mcpserver:::call_client_task(stub$session,
    "sampling/createMessage", list(messages = list()),
    ttl = 5),
    "did not declare tasks.requests capability for 'sampling/createMessage'")
})

test_that("call_client_task refuses when no tasks.requests at all", {
  stub <- new_stub_session(caps = list())
  expect_error(mcpserver:::call_client_task(stub$session,
    "elicitation/create", list(message = "hi"),
    ttl = 5),
    "did not declare tasks.requests")
})

test_that("client pollInterval shrinks the wait, never below 50 ms", {
  # Client suggests 10 ms; helper clamps to 50 ms minimum.
  stub <- new_stub_session(script = list(
    list(task = list(taskId = "T6", status = "working",
                     pollInterval = 10L)),
    list(task = list(taskId = "T6", status = "completed")),
    list()))
  t0 <- Sys.time()
  res <- mcpserver:::call_client_task(stub$session,
    "sampling/createMessage", list(messages = list()),
    ttl = 5, poll_interval = 0.5, total_timeout = 5)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  # Without the clamp we'd wait 0.5s before the first tasks/get.
  # With it we wait at most 0.05s. Allow generous slack.
  expect_lt(elapsed, 0.5)
  expect_true(is.list(res))
})

test_that("ttl in seconds is encoded as milliseconds on the wire", {
  stub <- new_stub_session(script = list(
    list(task = list(taskId = "T7", status = "completed")),
    list()))
  mcpserver:::call_client_task(stub$session,
    "sampling/createMessage", list(messages = list()),
    ttl = 7, poll_interval = 0.05, total_timeout = 5)
  expect_equal(stub$outgoing$out[[1L]]$params$task$ttl, 7000L)
})

test_that("ctx$request_sampling_async surfaces the helper through McpCtx", {
  stub <- new_stub_session(caps = list(
    sampling = list(),
    tasks = list(requests = list(sampling = list(createMessage = list())))),
    script = list(
      list(task = list(taskId = "T8", status = "completed")),
      list(content = list(type = "text", text = "done"))))
  ctx <- mcpserver:::make_ctx(stub$session, list(id = 99L))
  res <- ctx$request_sampling_async(
    messages = list(list(role = "user",
                         content = list(type = "text", text = "go"))),
    max_tokens = 8L, ttl = 5, poll_interval = 0.05,
    total_timeout = 5)
  expect_equal(res$content$text, "done")
})

test_that("ctx$request_sampling_async errors without sampling capability", {
  stub <- new_stub_session(caps = list(
    tasks = list(requests = list(sampling = list(createMessage = list())))))
  ctx <- mcpserver:::make_ctx(stub$session, list(id = 100L))
  expect_error(ctx$request_sampling_async(
    messages = list(list(role = "user",
                         content = list(type = "text", text = "x"))),
    ttl = 5),
    "did not declare sampling capability")
})

test_that("ctx$request_elicitation_async surfaces the helper through McpCtx", {
  stub <- new_stub_session(caps = list(
    elicitation = list(),
    tasks = list(requests = list(elicitation = list(create = list())))),
    script = list(
      list(task = list(taskId = "T9", status = "completed")),
      list(action = "accept", content = list(answer = "ok"))))
  ctx <- mcpserver:::make_ctx(stub$session, list(id = 101L))
  res <- ctx$request_elicitation_async(
    message = "go?",
    requested_schema = list(type = "object",
                            properties = list()),
    ttl = 5, poll_interval = 0.05, total_timeout = 5)
  expect_equal(res$content$answer, "ok")
})

test_that("the two new tools register only when the matching capability is declared", {
  srv <- new_server("demo")
  source(system.file("everything", "server.R", package = "mcpserver"),
         local = TRUE)
  # Without the tasks.requests caps, the on_initialized hook should
  # not add the async tools.
  mcp <- build_everything_server()
  s_no_caps <- mcpserver:::Session$new("a", mcp, function(e) NULL)
  s_no_caps$client_capabilities <- list(sampling = list(),
                                        elicitation = list())
  for (fn in mcp$on_initialized_hooks) fn(mcp, s_no_caps)
  expect_false(exists("trigger-sampling-request-async",
                       envir = mcp$tools, inherits = FALSE))
  expect_false(exists("trigger-elicitation-request-async",
                       envir = mcp$tools, inherits = FALSE))

  # With both caps + tasks.requests sub-keys, they should appear.
  mcp2 <- build_everything_server()
  s_with <- mcpserver:::Session$new("b", mcp2, function(e) NULL)
  s_with$client_capabilities <- list(
    sampling = list(), elicitation = list(),
    tasks = list(requests = list(
      sampling = list(createMessage = list()),
      elicitation = list(create = list()))))
  for (fn in mcp2$on_initialized_hooks) fn(mcp2, s_with)
  expect_true(exists("trigger-sampling-request-async",
                      envir = mcp2$tools, inherits = FALSE))
  expect_true(exists("trigger-elicitation-request-async",
                      envir = mcp2$tools, inherits = FALSE))
})
