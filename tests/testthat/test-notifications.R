# Unit tests for the notification helpers (logging, progress, cancellation,
# subscriptions).

test_that("send_log respects session$log_level threshold", {
  srv <- new_server("t")
  out <- new.env(parent = emptyenv()); out$msgs <- list()
  s <- mcpserver:::Session$new("t", srv, function(e) {
    out$msgs <- c(out$msgs, list(e))
  })
  s$log_level <- "warning"
  mcpserver:::send_log(s, "debug", "below threshold")
  mcpserver:::send_log(s, "warning", "at threshold")
  mcpserver:::send_log(s, "critical", "above threshold")
  methods <- vapply(out$msgs, function(m) m$method, character(1L))
  expect_equal(length(methods), 2L)
  expect_true(all(methods == "notifications/message"))
})

test_that("send_progress no-ops when token is NULL, emits otherwise", {
  srv <- new_server("t")
  out <- new.env(parent = emptyenv()); out$msgs <- list()
  s <- mcpserver:::Session$new("t", srv, function(e) {
    out$msgs <- c(out$msgs, list(e))
  })
  mcpserver:::send_progress(s, NULL, 1)
  expect_equal(length(out$msgs), 0L)
  mcpserver:::send_progress(s, "tok", 1, total = 10,
                            message = "in flight")
  expect_equal(length(out$msgs), 1L)
  expect_equal(out$msgs[[1L]]$method, "notifications/progress")
  expect_equal(out$msgs[[1L]]$params$progressToken, "tok")
})

test_that("send_progress emits `relatedRequestId` when supplied", {
  srv <- new_server("t")
  out <- new.env(parent = emptyenv()); out$msgs <- list()
  s <- mcpserver:::Session$new("t", srv,
    function(e) out$msgs <- c(out$msgs, list(e)))
  mcpserver:::send_progress(s, "tok", 1, total = 10,
                            message = "step",
                            related_request_id = 42L)
  expect_equal(out$msgs[[1L]]$params$relatedRequestId, 42L)
})

test_that("send_progress omits `relatedRequestId` when not supplied", {
  srv <- new_server("t")
  out <- new.env(parent = emptyenv()); out$msgs <- list()
  s <- mcpserver:::Session$new("t", srv,
    function(e) out$msgs <- c(out$msgs, list(e)))
  mcpserver:::send_progress(s, "tok", 1)
  expect_null(out$msgs[[1L]]$params$relatedRequestId)
})

test_that("ctx$send_progress() auto-fills relatedRequestId from msg$id", {
  srv <- new_server("t")
  out <- new.env(parent = emptyenv()); out$msgs <- list()
  s <- mcpserver:::Session$new("t", srv,
    function(e) out$msgs <- c(out$msgs, list(e)))
  msg <- list(id = 7L, params = list(`_meta` = list(progressToken = "tok7")))
  ctx <- mcpserver:::make_ctx(s, msg)
  ctx$send_progress(1, total = 3, message = "ok")
  expect_equal(out$msgs[[1L]]$params$progressToken, "tok7")
  expect_equal(out$msgs[[1L]]$params$relatedRequestId, 7L)
})

test_that("notifications/cancelled sets the cancel flag", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  flag <- new.env(parent = emptyenv()); flag$cancelled <- FALSE
  assign("42", flag, envir = s$cancel)
  mcpserver:::handle_cancelled(srv, s, list(requestId = 42L))
  expect_true(isTRUE(flag$cancelled))
})

test_that("notify_resource_updated only emits to subscribed sessions", {
  srv <- new_server("t")
  sent_a <- new.env(parent = emptyenv()); sent_a$n <- 0L
  sent_b <- new.env(parent = emptyenv()); sent_b$n <- 0L
  sa <- mcpserver:::Session$new("a", srv,
    function(e) sent_a$n <- sent_a$n + 1L)
  sb <- mcpserver:::Session$new("b", srv,
    function(e) sent_b$n <- sent_b$n + 1L)
  assign("a", sa, envir = srv$sessions)
  assign("b", sb, envir = srv$sessions)
  assign("demo://x", TRUE, envir = sa$subs)
  notify_resource_updated(srv, "demo://x")
  expect_equal(sent_a$n, 1L)
  expect_equal(sent_b$n, 0L)
})

test_that("notify_tool_list_changed broadcasts to all sessions", {
  srv <- new_server("t")
  counts <- new.env(parent = emptyenv()); counts$total <- 0L
  for (k in c("x", "y")) {
    assign(k, mcpserver:::Session$new(k, srv,
      function(e) counts$total <- counts$total + 1L),
      envir = srv$sessions)
  }
  notify_tool_list_changed(srv)
  expect_equal(counts$total, 2L)
})
