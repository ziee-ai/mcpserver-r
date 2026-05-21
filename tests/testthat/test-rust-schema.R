# Validate the envelopes our server emits against the Rust SDK's
# generated JSON Schema. Catches future wire-shape drift beyond what
# our hand-crafted assertions cover.

skip_if_not_installed("jsonvalidate")

schemas_dir <- system.file("spec", "rust-schemas",
                           package = "mcpserver")
if (!nzchar(schemas_dir)) {
  schemas_dir <- file.path("..", "..", "inst", "spec",
                            "rust-schemas")
}
skip_if(!file.exists(file.path(schemas_dir,
                               "server_json_rpc_message_schema.json")),
        "rust-schemas/server_json_rpc_message_schema.json not bundled")

server_validator <- jsonvalidate::json_validator(
  file.path(schemas_dir, "server_json_rpc_message_schema.json"),
  engine = "ajv")

validates <- function(envelope) {
  json <- jsonlite::toJSON(envelope, auto_unbox = TRUE, force = TRUE,
                            null = "null")
  isTRUE(server_validator(json))
}

test_that("a jrpc_response envelope validates against the server schema", {
  env <- mcpserver:::jrpc_response(1L,
    list(content = list(list(type = "text", text = "ok")),
         isError = FALSE))
  expect_true(validates(env))
})

test_that("a jrpc_error envelope validates against the server schema", {
  env <- mcpserver:::jrpc_error(1L,
    mcp_error_codes()$method_not_found,
    "no such method")
  expect_true(validates(env))
})

test_that("a notification envelope validates against the server schema", {
  env <- mcpserver:::jrpc_notification(
    "notifications/message",
    list(level = "info", data = list(message = "hi")))
  expect_true(validates(env))
})

test_that("a server-to-client request validates against the server schema", {
  env <- mcpserver:::jrpc_request("sampling/createMessage",
    list(messages = list(list(role = "user",
                              content = list(type = "text",
                                              text = "hi"))),
         maxTokens = 100L),
    id = -1L)
  expect_true(validates(env))
})

test_that("intentionally-malformed envelope fails validation", {
  bad <- list(jsonrpc = "1.0", id = 1L, result = list())
  expect_false(validates(bad))
})
