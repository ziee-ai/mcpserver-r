# Ports from packages/server/test/server/server.test.ts
# and packages/core/test/inMemory.test.ts lifecycle suites.

test_that("on_close hook is registered and fires on session teardown", {
  srv <- new_server("t")
  fired <- new.env(parent = emptyenv()); fired$n <- 0L
  fired$last <- NULL
  on_close(srv, function(mcp, sid) {
    fired$n <- fired$n + 1L
    fired$last <- sid
  })
  srv$fire_close("session-abc")
  expect_equal(fired$n, 1L)
  expect_equal(fired$last, "session-abc")
})

test_that("on_close supports multiple hooks; each fires once per close", {
  srv <- new_server("t")
  counts <- new.env(parent = emptyenv()); counts$a <- 0L; counts$b <- 0L
  on_close(srv, function(mcp, sid) counts$a <- counts$a + 1L)
  on_close(srv, function(mcp, sid) counts$b <- counts$b + 1L)
  srv$fire_close("s1")
  expect_equal(counts$a, 1L)
  expect_equal(counts$b, 1L)
  srv$fire_close("s2")
  expect_equal(counts$a, 2L)
  expect_equal(counts$b, 2L)
})

test_that("on_error hook is registered and fires with the condition object", {
  srv <- new_server("t")
  caught <- new.env(parent = emptyenv()); caught$msg <- NULL
  on_error(srv, function(err, mcp, sid) {
    caught$msg <- conditionMessage(err)
    caught$sid <- sid
  })
  srv$fire_error(simpleError("boom"), session_id = "s1")
  expect_equal(caught$msg, "boom")
  expect_equal(caught$sid, "s1")
})

test_that("on_error tolerates handler exceptions (does not crash the server)", {
  srv <- new_server("t")
  on_error(srv, function(err, mcp, sid) stop("handler also failed"))
  # safely() emits a message() but must not propagate the error.
  expect_no_error(suppressMessages(
    srv$fire_error(simpleError("inner"), "s1")))
})

test_that("register_capabilities merges into declared_capabilities", {
  srv <- new_server("t", capabilities = list(experimental = list(tasks = TRUE)))
  register_capabilities(srv, list(custom = list(featureX = TRUE)))
  caps <- srv$capabilities()
  expect_true(isTRUE(caps$experimental$tasks))
  expect_true(isTRUE(caps$custom$featureX))
})

test_that("register_capabilities lets the user override package defaults", {
  srv <- new_server("t")
  add_capability(srv, new_tool("k", "k", schema(list()),
    handler = function(args, ctx) response_text("ok")))
  expect_true(isTRUE(srv$capabilities()$tools$listChanged))
  # Override: declare tools.listChanged = FALSE.
  register_capabilities(srv, list(tools = list(listChanged = FALSE)))
  expect_false(isTRUE(srv$capabilities()$tools$listChanged))
})

test_that("send_ping returns NULL for unknown session id", {
  srv <- new_server("t")
  expect_null(send_ping(srv, "unknown-session"))
})
