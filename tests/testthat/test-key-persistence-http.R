# Issue 1, integration: a token minted before a restart must still
# authenticate against the same `/mcp` after the server is stopped and
# restarted with the same key_path + sqlite store + port. Pre-fix the key
# rotated on every start and all outstanding tokens silently broke.

skip_if_not_installed("processx")
skip_if_not_installed("httr2")
skip_on_cran()
skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))

# Spawn a server bound to a fixed key_path + sqlite db. On first start it
# mints a user token and writes it to `tokfile`; on restart it reuses both.
spawn_persist_server <- function(port, dbdir) {
  runner <- tempfile(fileext = ".R")
  writeLines(c(
    "suppressPackageStartupMessages(library(mcpserver))",
    "`%||%` <- function(a,b) if (is.null(a)) b else a",
    sprintf("PORT <- %dL; DBDIR <- %s", port, deparse(dbdir)),
    "dir.create(DBDIR, recursive = TRUE, showWarnings = FALSE)",
    "store <- new_mcp_store('sqlite', path = file.path(DBDIR, 'state.db'))",
    "as_cfg <- oauth_server_config(",
    "  issuer = sprintf('http://127.0.0.1:%d', PORT), audience = 'mcp',",
    "  store = store, key_path = file.path(DBDIR, 'as-key.pem'),",
    "  scopes_supported = c('mcp:read'))",
    "tokfile <- file.path(DBDIR, 'token.txt')",
    "if (!file.exists(tokfile)) {",
    "  u <- store$users$add(list(username = 'alice'))",
    "  tok <- oauth_mint_user_token(as_cfg, user_id = u$id,",
    "    scopes = c('mcp:read'), ttl = 3600L, name = 't')",
    "  writeLines(tok$token, tokfile)",
    "}",
    "srv <- new_server('persist', version = '0.1.0')",
    "add_capability(srv, new_tool('ping','ping', schema(list()),",
    "  handler = function(args, ctx) response_text('pong')))",
    "serve_http(srv, port = PORT, oauth_as = as_cfg, require_origin = FALSE)"
  ), runner)
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(), collapse = .Platform$path.sep)
  p <- processx::process$new("Rscript", c(runner),
                             stdout = "|", stderr = "|", env = child_env)
  url <- sprintf("http://127.0.0.1:%d/mcp", port)
  jwks_url <- sprintf("http://127.0.0.1:%d/jwks", port)
  tokfile <- file.path(dbdir, "token.txt")
  # Readiness via the unauthenticated /jwks endpoint, then read the minted
  # token and do an *authenticated* initialize (oauth_as gates /mcp).
  up <- FALSE
  for (i in seq_len(40L)) {
    Sys.sleep(0.5)
    jr <- tryCatch(httr2::request(jwks_url) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_timeout(3) |> httr2::req_perform(), error = function(e) NULL)
    if (!is.null(jr) && httr2::resp_status(jr) == 200L && file.exists(tokfile)) {
      up <- TRUE; break
    }
  }
  sid <- NULL
  token <- if (up) readLines(tokfile)[[1L]] else NULL
  if (up) {
    init <- tryCatch(httr2::request(url) |>
      httr2::req_method("POST") |>
      httr2::req_headers(`Content-Type` = "application/json",
                         Accept = "application/json, text/event-stream",
                         Authorization = paste("Bearer", token)) |>
      httr2::req_body_raw(charToRaw(
        '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}')) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_timeout(5) |>
      httr2::req_perform(), error = function(e) NULL)
    if (!is.null(init)) sid <- httr2::resp_header(init, "Mcp-Session-Id")
  }
  list(p = p, url = url, jwks_url = jwks_url, runner = runner,
       sid = sid, token = token)
}

call_ping <- function(srv, token) {
  httr2::request(srv$url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `Content-Type` = "application/json",
      Accept = "application/json, text/event-stream",
      `Mcp-Session-Id` = srv$sid,
      Authorization = paste("Bearer", token)) |>
    httr2::req_body_raw(charToRaw(
      '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"ping","arguments":{}}}')) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(5) |>
    httr2::req_perform()
}

test_that("a token minted before a restart still authenticates after it", {
  port <- 45210L
  dbdir <- tempfile("persist-")

  s1 <- spawn_persist_server(port, dbdir)
  if (is.null(s1$sid)) { s1$p$kill(); skip("server did not start") }
  token <- readLines(file.path(dbdir, "token.txt"))[[1L]]

  r1 <- call_ping(s1, token)
  jwks1 <- httr2::resp_body_string(
    httr2::req_perform(httr2::req_error(
      httr2::request(s1$jwks_url), is_error = function(r) FALSE)))
  s1$p$kill(); unlink(s1$runner)
  Sys.sleep(2)  # let the port free up

  expect_equal(httr2::resp_status(r1), 200L)

  # Restart: same port, same dbdir (=> same key_path + same token rows).
  s2 <- spawn_persist_server(port, dbdir)
  if (is.null(s2$sid)) { s2$p$kill(); skip("server did not restart") }
  withr::defer({ s2$p$kill(); unlink(s2$runner) })

  r2 <- call_ping(s2, token)
  expect_equal(httr2::resp_status(r2), 200L)   # would be 401 pre-fix
  body <- jsonlite::fromJSON(httr2::resp_body_string(r2), simplifyVector = FALSE)
  expect_equal(body$result$content[[1L]]$text, "pong")

  # /jwks is identical across the restart (same persisted key).
  jwks2 <- httr2::resp_body_string(
    httr2::req_perform(httr2::req_error(
      httr2::request(s2$jwks_url), is_error = function(r) FALSE)))
  expect_equal(jwks1, jwks2)
})
