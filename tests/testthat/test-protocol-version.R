test_that("mcp_protocol_version returns the latest revision", {
  expect_equal(mcp_protocol_version(), "2025-06-18")
})

test_that("supported protocol versions are listed newest-first", {
  v <- mcp_supported_protocol_versions()
  expect_true("2025-06-18" %in% v)
  expect_true("2025-03-26" %in% v)
  expect_true("2024-11-05" %in% v)
  expect_equal(v[[1L]], "2025-06-18")
})

test_that("negotiate_protocol_version echoes a supported version", {
  for (v in mcp_supported_protocol_versions()) {
    expect_equal(negotiate_protocol_version(v), v)
  }
})

test_that("negotiate_protocol_version falls back to latest on unknown / NULL", {
  expect_equal(negotiate_protocol_version("2099-01-01"), "2025-06-18")
  expect_equal(negotiate_protocol_version("not-a-version"), "2025-06-18")
  expect_equal(negotiate_protocol_version(NULL), "2025-06-18")
})

test_that("initialize echoes the negotiated version", {
  srv <- new_server("t")
  s <- mcpserver:::Session$new("t", srv, function(e) NULL)
  for (v in mcp_supported_protocol_versions()) {
    res <- mcpserver:::route_message(srv, s, list(
      jsonrpc = "2.0", id = 1, method = "initialize",
      params = list(protocolVersion = v, capabilities = list())))
    expect_equal(res$result$protocolVersion, v)
  }
  # Unknown version -> latest.
  res <- mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1, method = "initialize",
    params = list(protocolVersion = "2099-01-01")))
  expect_equal(res$result$protocolVersion, "2025-06-18")
})
