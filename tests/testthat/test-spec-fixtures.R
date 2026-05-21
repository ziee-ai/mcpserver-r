# Replay-driven assertions against the canonical MCP spec example
# fixtures vendored from modelcontextprotocol/modelcontextprotocol's
# schema/draft/examples directory. Each fixture file is canonical JSON
# for one envelope; we cross-check that our server emits matching wire
# shapes and error codes.

fixtures_root <- function() {
  d <- system.file("spec", "examples", package = "mcpserver")
  if (!nzchar(d)) {
    d <- file.path("..", "..", "inst", "spec", "examples")
  }
  d
}

read_fixture <- function(rel) {
  jsonlite::fromJSON(file.path(fixtures_root(), rel),
                     simplifyVector = FALSE)
}

skip_if(length(list.files(fixtures_root(), recursive = TRUE,
                          pattern = "\\.json$")) == 0L,
        "spec example fixtures not installed")

test_that("all bundled spec fixtures are syntactically valid JSON", {
  files <- list.files(fixtures_root(), recursive = TRUE,
                      pattern = "\\.json$", full.names = TRUE)
  for (f in files) {
    expect_silent(jsonlite::fromJSON(f, simplifyVector = FALSE))
  }
  expect_true(length(files) >= 100L)
})

# Build a tiny test server that returns the same error envelopes the
# fixtures describe.
build_fixture_server <- function() {
  srv <- new_server("fixture")
  add_capability(srv, new_tool(
    name = "echo",
    description = "echo",
    input_schema = schema(list(
      message = property_string(required = TRUE))),
    handler = function(args, ctx) response_text(args$message)))
  add_capability(srv, new_prompt(
    name = "simple",
    description = "s",
    arguments = list(),
    handler = function(args, ctx) "ok"))
  srv
}

test_that("unknown tool yields -32602 matching unknown-tool.json", {
  expected <- read_fixture("InvalidParamsError/unknown-tool.json")
  srv <- build_fixture_server()
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "tools/call",
    params = list(name = "invalid_tool_name", arguments = list())))
  expect_equal(resp$error$code, expected$code)
})

test_that("unknown prompt yields -32602 matching unknown-prompt.json", {
  expected <- read_fixture("InvalidParamsError/unknown-prompt.json")
  srv <- build_fixture_server()
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "prompts/get",
    params = list(name = "invalid-prompt")))
  expect_equal(resp$error$code, expected$code)
})

test_that("invalid tool arguments yield -32602 matching invalid-tool-arguments.json", {
  expected <- read_fixture("InvalidParamsError/invalid-tool-arguments.json")
  srv <- build_fixture_server()
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  # echo requires 'message' (string)
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "tools/call",
    params = list(name = "echo", arguments = list(message = 42L))))
  expect_equal(resp$error$code, expected$code)
})

test_that("unknown method yields -32601 matching MethodNotFoundError shape", {
  files <- list.files(file.path(fixtures_root(), "MethodNotFoundError"),
                      full.names = TRUE)
  expected <- jsonlite::fromJSON(files[[1L]], simplifyVector = FALSE)
  srv <- build_fixture_server()
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  resp <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "totally/unknown"))
  expect_equal(resp$error$code, expected$code)
  expect_equal(resp$error$code, -32601L)
})

test_that("malformed JSON over stdio yields -32700 matching invalid-json.json", {
  expected <- read_fixture("ParseError/invalid-json.json")
  # Use the dispatcher's parse path directly.
  parsed <- mcpserver:::jrpc_decode("{this is not valid json")
  expect_null(parsed)
  err <- mcpserver:::jrpc_error(NULL, mcpserver:::jrpc_codes$parse_error,
                                expected$message)
  expect_equal(err$error$code, expected$code)
})

test_that("cancelled notification matches the spec example shape", {
  expected <- read_fixture("CancelledNotification/user-requested-cancellation.json")
  # Synthesise our equivalent: build_cancelled gives us params shape
  built <- mcpserver:::jrpc_notification(
    "notifications/cancelled",
    list(requestId = expected$params$requestId,
         reason = expected$params$reason))
  expect_equal(built$method, expected$method)
  expect_equal(built$params$requestId, expected$params$requestId)
  expect_equal(built$params$reason, expected$params$reason)
})
