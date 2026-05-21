# Ports core/test/shared/protocol.test.ts (the parts we have analogues
# for) and protocolTransportHandling.test.ts.

test_that("response envelope echoes request id verbatim for string/int/numeric", {
  srv <- new_server("t")
  add_capability(srv, new_tool("k", "k", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  for (id in list("string-id", 7L, 3.14)) {
    r <- mcpserver:::route_message(srv, s, list(
      jsonrpc = "2.0", id = id, method = "tools/list"))
    expect_identical(r$id, id)
  }
})

test_that("notifications return NULL (no response envelope)", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  r <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", method = "notifications/initialized"))
  expect_null(r)
})

test_that("multiple concurrent sessions route responses independently", {
  srv <- new_server("t")
  add_capability(srv, new_tool("k", "k", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  outs <- list()
  for (i in 1:3) {
    outs[[i]] <- local({
      bag <- new.env(parent = emptyenv()); bag$msgs <- list()
      s <- mcpserver:::Session$new(sprintf("s%d", i), srv,
        function(e) bag$msgs <- c(bag$msgs, list(e)))
      assign(sprintf("s%d", i), s, envir = srv$sessions)
      bag
    })
  }
  # Notify only sessions 1 and 3 about an updated resource subscribed
  # to demo://x.
  for (i in c(1L, 3L)) {
    sess <- get(sprintf("s%d", i), envir = srv$sessions,
                inherits = FALSE)
    assign("demo://x", TRUE, envir = sess$subs)
  }
  notify_resource_updated(srv, "demo://x")
  expect_equal(length(outs[[1L]]$msgs), 1L)
  expect_equal(length(outs[[2L]]$msgs), 0L)
  expect_equal(length(outs[[3L]]$msgs), 1L)
})

test_that("response with no method and no result/error is invalid", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  r <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1L))
  expect_equal(r$error$code, -32600L)
})

test_that("incoming jsonrpc != '2.0' is invalid", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  r <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "1.0", id = 1L, method = "ping"))
  expect_equal(r$error$code, -32600L)
})

test_that("two sequential client requests don't share state across sessions", {
  srv <- new_server("t")
  s1 <- mcpserver:::Session$new("s1", srv, function(e) NULL)
  s2 <- mcpserver:::Session$new("s2", srv, function(e) NULL)
  s1$log_level <- "debug"
  s2$log_level <- "error"
  expect_equal(s1$log_level, "debug")
  expect_equal(s2$log_level, "error")
})
