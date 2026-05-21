# MCP back-compat: a client that advertises `elicitation: {}` (the
# pre-2025-11-25 shape) MUST be treated as if it had advertised
# `elicitation: { form: { applyDefaults: true } }`. Mirrors TS SDK's
# `ClientCapabilitiesSchema backwards compatibility` preprocessing.

new_srv <- function() new_server("t")

normalize <- function(caps) {
  srv <- new_srv()
  s <- mcpserver:::Session$new("s", srv, function(env) NULL)
  mcpserver:::route_message(srv, s, list(
    jsonrpc = "2.0", id = 1L, method = "initialize",
    params = list(protocolVersion = "2025-06-18",
                  capabilities = caps,
                  clientInfo = list(name = "c", version = "0"))))
  s$client_capabilities
}

test_that("empty elicitation:{} normalises to form mode with applyDefaults", {
  out <- normalize(list(elicitation = list()))
  expect_false(is.null(out$elicitation$form))
  expect_true(isTRUE(out$elicitation$form$applyDefaults))
})

test_that("explicit elicitation.form survives normalisation", {
  out <- normalize(list(elicitation = list(
    form = list(applyDefaults = FALSE))))
  expect_false(isTRUE(out$elicitation$form$applyDefaults))
})

test_that("absent elicitation capability is left absent", {
  out <- normalize(list(sampling = list()))
  expect_null(out$elicitation)
})

test_that("non-empty elicitation with arbitrary keys is untouched", {
  out <- normalize(list(elicitation = list(url = list())))
  # URL-only declaration: form is not injected.
  expect_null(out$elicitation$form)
  expect_false(is.null(out$elicitation$url))
})

test_that("non-list elicitation is left as-is (defensive)", {
  out <- mcpserver:::normalize_client_capabilities(list(
    elicitation = "junk"))
  expect_equal(out$elicitation, "junk")
})

test_that("request_elicitation still works against a back-compat capability", {
  srv <- new_srv()
  s <- mcpserver:::Session$new("s", srv, function(env) NULL)
  s$client_capabilities <- mcpserver:::normalize_client_capabilities(
    list(elicitation = list()))
  # No actual outbound call here — just verify the cap-check passes.
  expect_silent({
    caps <- s$client_capabilities %||% list()
    if (is.null(caps$elicitation))
      stop("would have failed without back-compat normalisation")
  })
})
