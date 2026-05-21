skip_if_not_installed("processx")
skip_if_not_installed("httr2")
skip_on_cran()
skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))

# End-to-end OAuth flow against a real `serve_http()` instance.
# Registers a client via DCR, hits /authorize with PKCE-S256, swaps
# the code for tokens, and uses the bearer token to call /mcp.

spawn_as_server <- function(port) {
  runner <- tempfile(fileext = ".R")
  log_path <- tempfile(fileext = ".log")
  writeLines(c(
    "tryCatch({",
    "  suppressPackageStartupMessages(library(mcpserver))",
    sprintf("  port <- %dL", port),
    "  issuer <- sprintf('http://127.0.0.1:%d', port)",
    "  audience <- sprintf('http://127.0.0.1:%d/mcp', port)",
    "  as_cfg <- oauth_server_config(",
    "    issuer = issuer, audience = audience,",
    "    auto_consent = TRUE, subject = 'e2e-user')",
    "  srv <- new_server('e2e-as', version = '0.1.0')",
    "  add_capability(srv, new_tool(",
    "    name = 'echo', description = 'echo',",
    "    input_schema = schema(list(",
    "      x = property_string('text', required = TRUE))),",
    "    handler = function(args, ctx) response_text(args$x)))",
    "  serve_http(srv, port = port,",
    "             allowed_origins = c('http://127.0.0.1'),",
    "             oauth_as = as_cfg)",
    "}, error = function(e) { message('server error: ',",
    "  conditionMessage(e)); quit(status = 1) })"
  ), runner)
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                               collapse = .Platform$path.sep)
  p <- processx::process$new("Rscript", c(runner),
                             stdout = "|", stderr = "|",
                             env = child_env)
  Sys.sleep(3)
  base <- sprintf("http://127.0.0.1:%d", port)
  list(process = p, base = base, runner = runner,
       log_path = log_path)
}

# Helper: send a GET, never follow redirects so we can read the
# Location header out of /authorize directly.
get_noredirect <- function(url) {
  httr2::request(url) |>
    httr2::req_options(followlocation = 0L) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(5) |>
    httr2::req_perform()
}

post_form <- function(url, params) {
  httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(`Content-Type` =
                         "application/x-www-form-urlencoded") |>
    httr2::req_body_form(!!!params) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(5) |>
    httr2::req_perform()
}

post_json <- function(url, body) {
  httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_raw(charToRaw(
      jsonlite::toJSON(body, auto_unbox = TRUE))) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(5) |>
    httr2::req_perform()
}

challenge_for <- function(verifier) {
  raw <- openssl::sha256(charToRaw(verifier))
  sub("=+$", "", jose::base64url_encode(raw))
}

test_that("register → authorize → token → call /mcp succeeds end-to-end", {
  srv <- spawn_as_server(port = 44210L)
  withr::defer({
    srv$process$kill()
    unlink(srv$runner)
  })

  # /.well-known/oauth-authorization-server
  meta_resp <- httr2::req_perform(
    httr2::request(paste0(srv$base,
      "/.well-known/oauth-authorization-server")) |>
      httr2::req_timeout(5) |>
      httr2::req_error(is_error = function(r) FALSE))
  if (httr2::resp_status(meta_resp) != 200L) {
    skip("AS metadata endpoint did not respond")
  }
  meta <- jsonlite::fromJSON(httr2::resp_body_string(meta_resp),
                             simplifyVector = FALSE)
  expect_equal(meta$issuer, srv$base)
  expect_equal(meta$authorization_endpoint,
               paste0(srv$base, "/authorize"))

  # 1) Register a client.
  reg_resp <- post_json(meta$registration_endpoint, list(
    client_name = "e2e",
    redirect_uris = list("http://app.local/cb")))
  expect_equal(httr2::resp_status(reg_resp), 201L)
  reg <- jsonlite::fromJSON(httr2::resp_body_string(reg_resp),
                            simplifyVector = FALSE)
  expect_true(nzchar(reg$client_id))

  # 2) Authorize (PKCE-S256) and read the redirect.
  verifier <- paste(rep("e", 64L), collapse = "")
  auth_url <- paste0(meta$authorization_endpoint, "?",
    "response_type=code",
    "&client_id=", utils::URLencode(reg$client_id, reserved = TRUE),
    "&redirect_uri=", utils::URLencode("http://app.local/cb",
                                       reserved = TRUE),
    "&code_challenge=", utils::URLencode(challenge_for(verifier),
                                         reserved = TRUE),
    "&code_challenge_method=S256",
    "&state=xyz")
  auth_resp <- get_noredirect(auth_url)
  expect_equal(httr2::resp_status(auth_resp), 302L)
  loc <- httr2::resp_header(auth_resp, "Location")
  expect_match(loc, "^http://app.local/cb\\?code=")
  code <- sub(".*[?&]code=([^&]+).*", "\\1", loc)

  # 3) Exchange the code for tokens.
  tok_resp <- post_form(meta$token_endpoint, list(
    grant_type = "authorization_code",
    code = utils::URLdecode(code),
    client_id = reg$client_id,
    redirect_uri = "http://app.local/cb",
    code_verifier = verifier))
  expect_equal(httr2::resp_status(tok_resp), 200L)
  tok <- jsonlite::fromJSON(httr2::resp_body_string(tok_resp),
                            simplifyVector = FALSE)
  expect_true(nzchar(tok$access_token))

  # 4) Use the access token to initialize an MCP session against
  #    the same process.
  init_resp <- httr2::request(paste0(srv$base, "/mcp")) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `Origin` = "http://127.0.0.1",
      `Content-Type` = "application/json",
      `Authorization` = paste("Bearer", tok$access_token)) |>
    httr2::req_body_raw(charToRaw(jsonlite::toJSON(list(
      jsonrpc = "2.0", id = 1L, method = "initialize",
      params = list(protocolVersion = "2025-06-18",
                    capabilities = list())),
      auto_unbox = TRUE))) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(8) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(init_resp), 200L)

  # 5) And a follow-up tools/list with the same bearer token.
  session_id <- httr2::resp_header(init_resp, "Mcp-Session-Id")
  list_resp <- httr2::request(paste0(srv$base, "/mcp")) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `Origin` = "http://127.0.0.1",
      `Content-Type` = "application/json",
      `Mcp-Session-Id` = session_id,
      `MCP-Protocol-Version` = "2025-06-18",
      `Authorization` = paste("Bearer", tok$access_token)) |>
    httr2::req_body_raw(charToRaw(jsonlite::toJSON(list(
      jsonrpc = "2.0", id = 2L, method = "tools/list"),
      auto_unbox = TRUE))) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(8) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(list_resp), 200L)
  body <- jsonlite::fromJSON(httr2::resp_body_string(list_resp),
                             simplifyVector = FALSE)
  names_ <- vapply(body$result$tools, function(t) t$name,
                   character(1L))
  expect_true("echo" %in% names_)
})

test_that("/mcp rejects requests without a bearer token (401 + WWW-Authenticate)", {
  srv <- spawn_as_server(port = 44211L)
  withr::defer({
    srv$process$kill()
    unlink(srv$runner)
  })
  resp <- httr2::request(paste0(srv$base, "/mcp")) |>
    httr2::req_method("POST") |>
    httr2::req_headers(`Origin` = "http://127.0.0.1",
                       `Content-Type` = "application/json") |>
    httr2::req_body_raw(charToRaw(
      '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}')) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(5) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 401L)
  expect_match(httr2::resp_header(resp, "WWW-Authenticate"),
               "^Bearer")
})

test_that("oauth_as and auth must agree on issuer/audience", {
  srv <- new_server("t")
  as_cfg <- oauth_server_config(
    issuer = "https://a.example",
    audience = "https://r.example")
  bad_rs <- oauth_config(issuer = "https://b.example",
                         audience = "https://r.example",
                         jwks_uri = "https://a.example/jwks")
  expect_error(serve_http(srv, port = 44290L, auth = bad_rs,
                          oauth_as = as_cfg, block = FALSE,
                          allowed_origins = "http://127.0.0.1"),
               "disagree on issuer")
})
