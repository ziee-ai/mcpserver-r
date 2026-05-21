# DNS-rebinding / Origin / Host header defenses. Mirrors the
# Python SDK's test_streamable_http_security.py + Java's
# DefaultServerTransportSecurityValidatorTests.

new_state <- function(require_origin = TRUE,
                      allowed_origins = c("http://localhost",
                                          "http://127.0.0.1"),
                      allowed_hosts = NULL) {
  e <- new.env(parent = emptyenv())
  e$require_origin <- isTRUE(require_origin)
  e$allowed_origins <- allowed_origins
  e$allowed_hosts <- allowed_hosts
  e
}

req <- function(headers = character()) list(headers = headers)

# Origin header ----------------------------------------------------------

test_that("missing Origin with require_origin = TRUE is rejected", {
  st <- new_state()
  expect_false(mcpserver:::validate_origin(st, req()))
})

test_that("missing Origin with require_origin = FALSE is accepted", {
  st <- new_state(require_origin = FALSE)
  expect_true(mcpserver:::validate_origin(st, req()))
})

test_that("Origin exactly equal to an allowed entry is accepted", {
  st <- new_state()
  expect_true(mcpserver:::validate_origin(st,
    req(c("Origin" = "http://127.0.0.1"))))
})

test_that("Origin with port suffix matches a port-less allowed entry", {
  st <- new_state(allowed_origins = "http://localhost")
  expect_true(mcpserver:::validate_origin(st,
    req(c("Origin" = "http://localhost:8080"))))
  expect_true(mcpserver:::validate_origin(st,
    req(c("Origin" = "http://localhost:65535"))))
})

test_that("Origin from a sibling-domain rebind attempt is rejected", {
  st <- new_state(allowed_origins = "http://localhost")
  expect_false(mcpserver:::validate_origin(st,
    req(c("Origin" = "http://localhost.evil.com"))))
  expect_false(mcpserver:::validate_origin(st,
    req(c("Origin" = "http://attacker.com"))))
})

test_that("Origin with a mismatched scheme is rejected", {
  st <- new_state(allowed_origins = "http://localhost")
  expect_false(mcpserver:::validate_origin(st,
    req(c("Origin" = "https://localhost"))))
})

test_that("Origin header is case-insensitive on lookup", {
  st <- new_state()
  # nanonext exposes headers as a named char vec; header_get is case-insensitive.
  expect_true(mcpserver:::validate_origin(st,
    req(c("origin" = "http://127.0.0.1"))))
})

# Host header ------------------------------------------------------------

test_that("validate_host accepts when allowed_hosts is NULL (default)", {
  st <- new_state(allowed_hosts = NULL)
  expect_true(mcpserver:::validate_host(st, req(c("Host" = "anything"))))
})

test_that("validate_host accepts exact-match Host header", {
  st <- new_state(allowed_hosts = "myserver:8080")
  expect_true(mcpserver:::validate_host(st,
    req(c("Host" = "myserver:8080"))))
})

test_that("validate_host accepts bare-host form against port-qualified allowlist", {
  st <- new_state(allowed_hosts = "myserver")
  expect_true(mcpserver:::validate_host(st,
    req(c("Host" = "myserver:8080"))))
  expect_true(mcpserver:::validate_host(st,
    req(c("Host" = "myserver"))))
})

test_that("validate_host rejects mismatched Host", {
  st <- new_state(allowed_hosts = "myserver")
  expect_false(mcpserver:::validate_host(st,
    req(c("Host" = "evil.com"))))
  expect_false(mcpserver:::validate_host(st,
    req(c("Host" = "evil.com:80"))))
})

test_that("validate_host rejects an absent Host header when allowlist is set", {
  st <- new_state(allowed_hosts = "myserver")
  expect_false(mcpserver:::validate_host(st, req()))
})

# DNS-rebind scenario: attacker sets the victim's browser to send the
# correct Origin header (allowed: localhost), but the resolved IP
# silently rotates to the attacker. Origin header inspection alone is
# necessary but not sufficient; Host header pinning closes the gap.

test_that("Host + Origin pinning together reject a rebind attempt", {
  st <- new_state(allowed_origins = "http://localhost",
                  allowed_hosts = "127.0.0.1")
  # Attacker manages to set a valid Origin but Host header is sniped.
  r <- req(c("Origin" = "http://localhost",
             "Host" = "victim-internal.lan:8000"))
  expect_true(mcpserver:::validate_origin(st, r))
  expect_false(mcpserver:::validate_host(st, r))
})

# IPv6 -------------------------------------------------------------------

test_that("allowed_origins accepts an explicit IPv6 entry with port suffix", {
  st <- new_state(allowed_origins = "http://[::1]")
  expect_true(mcpserver:::validate_origin(st,
    req(c("Origin" = "http://[::1]:48000"))))
})

test_that("custom allowed_origins (non-default) is honoured", {
  st <- new_state(allowed_origins = c("https://app.example.com"))
  expect_true(mcpserver:::validate_origin(st,
    req(c("Origin" = "https://app.example.com"))))
  expect_false(mcpserver:::validate_origin(st,
    req(c("Origin" = "http://localhost"))))
})

# allowed_hosts as a custom list (override) -----------------------------

test_that("custom allowed_hosts can be set to multiple entries", {
  st <- new_state(allowed_hosts = c("internal.example.com",
                                    "127.0.0.1"))
  expect_true(mcpserver:::validate_host(st,
    req(c("Host" = "internal.example.com"))))
  expect_true(mcpserver:::validate_host(st,
    req(c("Host" = "127.0.0.1:9000"))))
  expect_false(mcpserver:::validate_host(st,
    req(c("Host" = "other.example.com"))))
})
