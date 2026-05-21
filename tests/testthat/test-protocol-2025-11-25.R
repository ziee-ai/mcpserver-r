# 2025-11-25 protocol revision tests.

test_that("2025-11-25 is the default / latest supported revision", {
  expect_equal(mcp_protocol_version(), "2025-11-25")
  expect_equal(mcp_supported_protocol_versions()[[1L]], "2025-11-25")
})

test_that("2025-11-25 initialize negotiation echoes the client version", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  r <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "initialize",
    params = list(protocolVersion = "2025-11-25",
                  capabilities = list())))
  expect_equal(r$result$protocolVersion, "2025-11-25")
  expect_equal(s$protocol_version, "2025-11-25")
})

test_that("2024-11-05 still negotiates round-trip (backwards compat)", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  r <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "initialize",
    params = list(protocolVersion = "2024-11-05",
                  capabilities = list())))
  expect_equal(r$result$protocolVersion, "2024-11-05")
})
