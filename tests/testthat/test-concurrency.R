skip_if_not_installed("processx")
skip_if_not_installed("httr2")
skip_on_cran()
skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))

# Helpers ---------------------------------------------------------------

spawn_concurrency_server <- function(port, daemons = 4L) {
  runner_script <- tempfile(fileext = ".R")
  log_path <- tempfile(fileext = ".log")
  writeLines(c(
    "log_path <- Sys.getenv('MCPSERVER_LOG', '')",
    "if (nzchar(log_path)) sink(file(log_path, open = 'a'), type='message')",
    "tryCatch({",
    "  suppressPackageStartupMessages(library(mcpserver))",
    "  srv <- new_server('concurrency', version = '0.1.0')",
    "  add_capability(srv, new_tool(",
    "    name = 'slow', description = 'Slow tool',",
    "    input_schema = schema(list(",
    "      ms = property_integer('sleep ms', default = 500L))),",
    "    handler = function(args, ctx) {",
    "      Sys.sleep((args$ms %||% 500L) / 1000)",
    "      response_text(paste0('slept ', args$ms %||% 500L))",
    "    }))",
    "  add_capability(srv, new_tool(",
    "    name = 'fast', description = 'Fast tool',",
    "    input_schema = schema(list()),",
    "    handler = function(args, ctx) response_text('ok')))",
    "  add_capability(srv, new_resource(",
    "    name = 'r-slow', description = 'Slow resource',",
    "    uri = 'demo://slow', mime_type = 'text/plain',",
    "    handler = function(params, ctx) { Sys.sleep(0.3); 'slow-body' }))",
    "  add_capability(srv, new_prompt(",
    "    name = 'p-slow', description = 'Slow prompt',",
    "    arguments = list(),",
    "    handler = function(args, ctx) { Sys.sleep(0.3); 'slow-text' }))",
    sprintf("  serve_http(srv, port = %dL, allowed_origins = c('http://127.0.0.1'), daemons = %dL)",
            port, daemons),
    "}, error = function(e) { message('server error: ', conditionMessage(e)); quit(status = 1) })"
  ), runner_script)

  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                               collapse = .Platform$path.sep)
  child_env["MCPSERVER_LOG"] <- log_path
  p <- processx::process$new("Rscript", c(runner_script),
                             stdout = "|", stderr = "|",
                             env = child_env)
  Sys.sleep(3)
  url <- sprintf("http://127.0.0.1:%d/mcp", port)
  init <- tryCatch(httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Origin = "http://127.0.0.1",
                       `Content-Type` = "application/json") |>
    httr2::req_body_raw(charToRaw(
      '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}')) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(8) |>
    httr2::req_perform(),
    error = function(e) NULL)
  sid <- if (!is.null(init)) httr2::resp_header(init, "Mcp-Session-Id") else NULL
  list(process = p, url = url, sid = sid, log_path = log_path,
       runner_script = runner_script)
}

post_method <- function(srv, body, timeout = 30) {
  httr2::request(srv$url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Origin = "http://127.0.0.1",
      `Content-Type` = "application/json",
      `Mcp-Session-Id` = srv$sid,
      `MCP-Protocol-Version` = "2025-06-18") |>
    httr2::req_body_raw(charToRaw(body)) |>
    httr2::req_timeout(timeout) |>
    httr2::req_perform_promise()
}

resolve_all <- function(promises, deadline_s = 15) {
  state <- new.env(parent = emptyenv())
  state$done <- 0L
  state$errors <- list()
  for (pr in promises) {
    promises::then(pr,
      onFulfilled = function(r) { state$done <- state$done + 1L },
      onRejected  = function(e) {
        state$done <- state$done + 1L
        state$errors <- c(state$errors,
                          list(conditionMessage(e)))
      })
  }
  t0 <- Sys.time()
  while (state$done < length(promises) &&
         difftime(Sys.time(), t0, units = "secs") < deadline_s) {
    later::run_now(timeoutSecs = 0.05)
  }
  state
}

# Test A — ten slow tool calls run in parallel ---------------------------

test_that("ten concurrent slow tool calls finish in roughly four daemon rounds", {
  srv <- spawn_concurrency_server(port = 42301L, daemons = 4L)
  withr::defer({
    srv$process$kill()
    unlink(srv$runner_script)
  })
  if (is.null(srv$sid) || !nzchar(srv$sid)) {
    skip(paste("server did not start:",
               paste(readLines(srv$log_path, warn = FALSE), collapse = " | ")))
  }

  fire <- function(id) {
    post_method(srv, sprintf(
      '{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":"slow","arguments":{"ms":500}}}',
      id))
  }
  t0 <- Sys.time()
  ps <- lapply(seq_len(10L), fire)
  state <- resolve_all(ps, deadline_s = 15)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_equal(state$done, 10L)
  expect_equal(length(state$errors), 0L,
               info = paste(unlist(state$errors), collapse = " | "))
  # Sequential would be 10 * 0.5 = 5s. With four daemons we expect
  # ceil(10/4) = 3 rounds = ~1.5s plus a small overhead. Anything under
  # 2.5s is decisive evidence of parallelism.
  expect_lt(elapsed, 2.5)
})

# Test B — mixed-type concurrent requests parallelise --------------------

test_that("tools, resources and prompts can all run concurrently", {
  srv <- spawn_concurrency_server(port = 42302L, daemons = 6L)
  withr::defer({
    srv$process$kill()
    unlink(srv$runner_script)
  })
  if (is.null(srv$sid) || !nzchar(srv$sid)) {
    skip("server did not start")
  }

  body_tool <- '{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":"slow","arguments":{"ms":300}}}'
  body_res  <- '{"jsonrpc":"2.0","id":%d,"method":"resources/read","params":{"uri":"demo://slow"}}'
  body_prom <- '{"jsonrpc":"2.0","id":%d,"method":"prompts/get","params":{"name":"p-slow"}}'

  t0 <- Sys.time()
  ps <- list()
  for (i in 1:3) {
    ps <- c(ps, list(post_method(srv, sprintf(body_tool, i * 3L - 2L))))
    ps <- c(ps, list(post_method(srv, sprintf(body_res,  i * 3L - 1L))))
    ps <- c(ps, list(post_method(srv, sprintf(body_prom, i * 3L))))
  }
  state <- resolve_all(ps, deadline_s = 15)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_equal(state$done, 9L)
  expect_equal(length(state$errors), 0L,
               info = paste(unlist(state$errors), collapse = " | "))
  # Nine 300ms requests over six daemons = 2 rounds = ~0.6s. Sequential
  # would be 2.7s. Assert clearly below half-serialised.
  expect_lt(elapsed, 1.5)
})

# Test C — a slow request does not stall a fast one ---------------------
#
# Issuing both requests from the same R session via httr2's promise API
# serialises on the libcurl thread, masking real server-side parallelism.
# Spawning two `curl` subprocesses guarantees the requests hit the wire
# simultaneously, so this test measures only the server's behaviour.

test_that("a fast request returns long before an in-flight slow request", {
  curl_bin <- Sys.which("curl")
  skip_if(!nzchar(curl_bin), "curl not on PATH")

  srv <- spawn_concurrency_server(port = 42303L, daemons = 4L)
  withr::defer({
    srv$process$kill()
    unlink(srv$runner_script)
  })
  if (is.null(srv$sid) || !nzchar(srv$sid)) {
    skip("server did not start")
  }

  curl_post <- function(body) {
    processx::process$new(
      curl_bin,
      c("-sS", "-o", "/dev/null",
        "-w", "%{time_total}",
        "-X", "POST", srv$url,
        "-H", "Origin: http://127.0.0.1",
        "-H", sprintf("Mcp-Session-Id: %s", srv$sid),
        "-H", "MCP-Protocol-Version: 2025-06-18",
        "-H", "Content-Type: application/json",
        "-d", body),
      stdout = "|", stderr = "|")
  }
  finish <- function(p, timeout = 10) {
    p$wait(timeout = as.integer(timeout * 1000))
    if (p$is_alive()) p$kill()
    out <- tryCatch(p$read_all_output(), error = function(e) "")
    as.numeric(out)
  }

  slow_p <- curl_post(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"slow","arguments":{"ms":3000}}}')
  Sys.sleep(0.5)  # ensure the slow request is in flight inside a daemon
  fast_p <- curl_post(
    '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"fast","arguments":{}}}')

  fast_time <- finish(fast_p, timeout = 5)
  slow_time <- finish(slow_p, timeout = 10)

  expect_true(is.finite(fast_time),
              info = "fast subprocess produced no timing output")
  expect_true(is.finite(slow_time))
  # Fast tool's wall time on the server side is microseconds; the bulk
  # of curl's `time_total` here is the round-trip + connection. Allow up
  # to 1.0s of slack for slow CI hosts but assert clearly below the
  # 3 seconds the slow tool sleeps for.
  expect_lt(fast_time, 1.0)
  # Slow request still completes successfully (~3s on a quiet host).
  expect_gt(slow_time, 2.5)
})
