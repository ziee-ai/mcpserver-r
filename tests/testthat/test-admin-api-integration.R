# End-to-end integration tests for the /admin/* REST surface.
#
# Boots `serve_http(admin = TRUE)` in a subprocess against a temp SQLite
# database, then exercises every endpoint with `httr2`. Covers the
# auth matrix (bootstrap / admin JWT / non-admin JWT / missing /
# malformed), payload validation, and error codes documented in the
# plan.

skip_if_not_installed("processx")
skip_if_not_installed("httr2")
skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")
skip_on_cran()
skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))

# ----- subprocess fixture ------------------------------------------------

spawn_admin <- function(port) {
  bootstrap <- paste(openssl::rand_bytes(16L), collapse = "")
  store_path <- tempfile(fileext = ".db")
  runner <- tempfile(fileext = ".R")
  writeLines(c(
    "suppressPackageStartupMessages(library(mcpserver))",
    sprintf("store <- new_mcp_store('sqlite', path = '%s')", store_path),
    sprintf("oauth_as <- oauth_server_config(issuer = 'http://127.0.0.1:%d', audience = 'mcp', store = store)", port),
    "mcp <- new_server('it', version = '0.1.0')",
    "add_capability(mcp, new_tool('whoami', 'whoami', schema(list()),",
    "  handler = function(args, ctx) response_text(",
    "    sprintf('user_id=%s name=%s admin=%s scopes=%s',",
    "      ctx$user_id %||% 'none',",
    "      ctx$user_name %||% 'none',",
    "      ctx$is_admin,",
    "      paste(ctx$auth_scopes, collapse=',')))))",
    sprintf("Sys.setenv(MCPSERVER_ADMIN_TOKEN='%s')", bootstrap),
    sprintf("serve_http(mcp, port = %dL, oauth_as = oauth_as,", port),
    "             admin = TRUE,",
    "             allowed_origins = c('http://127.0.0.1'))"
  ), runner)
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                               collapse = .Platform$path.sep)
  p <- processx::process$new("Rscript", c(runner),
                             stdout = "|", stderr = "|",
                             env = child_env)
  url <- sprintf("http://127.0.0.1:%d", port)
  # Poll the healthz endpoint until ready (or fail with the child's logs).
  ok <- FALSE
  last <- NULL
  for (i in seq_len(60L)) {
    resp <- tryCatch(
      httr2::request(paste0(url, "/admin/healthz")) |>
        httr2::req_headers(Authorization = paste("Bearer", bootstrap),
                           Origin = "http://127.0.0.1") |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_timeout(2) |>
        httr2::req_perform(),
      error = function(e) { last <<- conditionMessage(e); NULL })
    if (!is.null(resp) && httr2::resp_status(resp) == 200L) {
      ok <- TRUE; break
    }
    Sys.sleep(0.25)
  }
  if (!ok) {
    err <- paste("stderr:", p$read_error_lines() |> paste(collapse=" | "),
                 "| stdout:", p$read_output_lines() |>
                   paste(collapse=" | "),
                 "| last:", last %||% "")
    p$kill()
    skip(paste("admin server failed to start:",
               substr(err, 1L, 600L)))
  }
  list(process = p, url = url, bootstrap = bootstrap,
       runner = runner, store_path = store_path)
}

teardown_admin <- function(srv) {
  if (srv$process$is_alive()) srv$process$kill()
  unlink(c(srv$runner, srv$store_path, paste0(srv$store_path, "-wal"),
           paste0(srv$store_path, "-shm")))
}

H <- function(srv, extra = list()) {
  c(Origin = "http://127.0.0.1",
    `Content-Type` = "application/json",
    Authorization = paste("Bearer", srv$bootstrap),
    extra)
}

http_call <- function(url, method, headers, body = NULL) {
  req <- httr2::request(url) |>
    httr2::req_method(method) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(5)
  hdr_args <- as.list(headers); names(hdr_args) <- names(headers)
  req <- do.call(httr2::req_headers, c(list(req), hdr_args))
  if (!is.null(body)) {
    req <- httr2::req_body_raw(req, charToRaw(jsonlite::toJSON(
      body, auto_unbox = TRUE)))
  }
  httr2::req_perform(req)
}

jsbody <- function(resp) {
  txt <- httr2::resp_body_string(resp)
  if (!nzchar(txt)) return(NULL)
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

# ----- shared test fixture -----------------------------------------------

PORT <- 47101L
srv  <- NULL

start_once <- function() {
  if (!is.null(srv) && srv$process$is_alive()) return(invisible(srv))
  srv <<- spawn_admin(PORT)
  invisible(srv)
}

withr::defer(if (!is.null(srv)) teardown_admin(srv),
             testthat::teardown_env())

# ----- /admin/healthz ----------------------------------------------------

test_that("GET /admin/healthz: bootstrap token returns 200", {
  start_once()
  r <- http_call(paste0(srv$url, "/admin/healthz"), "GET",
                 H(srv))
  expect_equal(httr2::resp_status(r), 200L)
  expect_equal(jsbody(r)$status, "ok")
})

test_that("GET /admin/healthz: missing/malformed/wrong tokens", {
  start_once()
  base <- paste0(srv$url, "/admin/healthz")
  # missing
  r1 <- http_call(base, "GET",
                  c(Origin = "http://127.0.0.1"))
  expect_equal(httr2::resp_status(r1), 401L)
  # malformed (no "Bearer ")
  r2 <- http_call(base, "GET",
                  c(Origin = "http://127.0.0.1",
                    Authorization = "Token xyz"))
  expect_equal(httr2::resp_status(r2), 401L)
  # wrong
  r3 <- http_call(base, "GET",
                  c(Origin = "http://127.0.0.1",
                    Authorization = "Bearer nope"))
  expect_equal(httr2::resp_status(r3), 401L)
})

# ----- /admin/users ------------------------------------------------------

test_that("POST /admin/users + GET + GET /:id + PATCH + DELETE", {
  start_once()
  url <- paste0(srv$url, "/admin/users")
  # create
  r <- http_call(url, "POST", H(srv),
                 body = list(username = "alice",
                             email = "a@x.com",
                             groups = c("dev")))
  expect_equal(httr2::resp_status(r), 201L)
  u <- jsbody(r)
  expect_equal(u$username, "alice")
  expect_false(u$is_admin)

  # list (must contain alice)
  r2 <- http_call(url, "GET", H(srv))
  expect_equal(httr2::resp_status(r2), 200L)
  unames <- vapply(jsbody(r2)$users, function(x) x$username,
                   character(1L))
  expect_true("alice" %in% unames)

  # get by id
  r3 <- http_call(paste0(url, "/", u$id), "GET", H(srv))
  expect_equal(httr2::resp_status(r3), 200L)
  expect_equal(jsbody(r3)$email, "a@x.com")

  # patch
  r4 <- http_call(paste0(url, "/", u$id), "PATCH", H(srv),
                  body = list(email = "a@y.com"))
  expect_equal(httr2::resp_status(r4), 200L)
  expect_equal(jsbody(r4)$email, "a@y.com")

  # delete
  r5 <- http_call(paste0(url, "/", u$id), "DELETE", H(srv))
  expect_equal(httr2::resp_status(r5), 204L)

  # delete again -> 404
  r6 <- http_call(paste0(url, "/", u$id), "DELETE", H(srv))
  expect_equal(httr2::resp_status(r6), 404L)
})

test_that("POST /admin/users: validation + uniqueness", {
  start_once()
  url <- paste0(srv$url, "/admin/users")
  # missing body
  r1 <- http_call(url, "POST", H(srv), body = list())
  expect_equal(httr2::resp_status(r1), 400L)
  # create then dup
  r2 <- http_call(url, "POST", H(srv),
                  body = list(username = "dupe"))
  expect_equal(httr2::resp_status(r2), 201L)
  r3 <- http_call(url, "POST", H(srv),
                  body = list(username = "dupe"))
  expect_equal(httr2::resp_status(r3), 409L)
})

test_that("GET /admin/users/{id}: 404 for unknown id", {
  start_once()
  r <- http_call(paste0(srv$url, "/admin/users/u_missing"),
                 "GET", H(srv))
  expect_equal(httr2::resp_status(r), 404L)
})

# ----- /admin/tokens/mint + revoke ---------------------------------------

test_that("mint -> verify (whoami) -> revoke -> 401 flow", {
  start_once()
  # create a non-admin user
  r1 <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                  body = list(username = "bob"))
  expect_equal(httr2::resp_status(r1), 201L)
  uid <- jsbody(r1)$id

  # mint token
  r2 <- http_call(paste0(srv$url, "/admin/tokens/mint"),
                  "POST", H(srv),
                  body = list(user_id = uid, name = "ci",
                              scopes = list("mcp:read"),
                              ttl = 3600L))
  expect_equal(httr2::resp_status(r2), 200L)
  mint <- jsbody(r2)
  expect_true(nzchar(mint$jti))
  expect_true(nzchar(mint$token))

  # use it on /mcp tools/list
  init <- http_call(paste0(srv$url, "/mcp"), "POST",
                    c(Origin = "http://127.0.0.1",
                      `Content-Type` = "application/json",
                      Accept = "application/json, text/event-stream",
                      Authorization = paste("Bearer", mint$token)),
                    body = list(jsonrpc = "2.0", id = 1,
                                method = "initialize",
                                params = list(
                                  protocolVersion = "2025-06-18",
                                  capabilities = list())))
  expect_equal(httr2::resp_status(init), 200L)
  sid <- httr2::resp_header(init, "Mcp-Session-Id")
  expect_true(nzchar(sid %||% ""))

  # call whoami; tool sees ctx$user_id
  call <- http_call(paste0(srv$url, "/mcp"), "POST",
                    c(Origin = "http://127.0.0.1",
                      `Content-Type` = "application/json",
                      Accept = "application/json, text/event-stream",
                      Authorization = paste("Bearer", mint$token),
                      `Mcp-Session-Id` = sid),
                    body = list(jsonrpc = "2.0", id = 2,
                                method = "tools/call",
                                params = list(name = "whoami",
                                              arguments = list())))
  expect_equal(httr2::resp_status(call), 200L)
  body <- jsbody(call)
  result_text <- body$result$content[[1L]]$text
  expect_true(grepl(uid, result_text, fixed = TRUE))
  expect_true(grepl("name=bob", result_text, fixed = TRUE))
  expect_true(grepl("admin=FALSE", result_text, fixed = TRUE))

  # revoke
  r3 <- http_call(sprintf("%s/admin/tokens/%s/revoke",
                          srv$url, mint$jti),
                  "POST", H(srv))
  expect_equal(httr2::resp_status(r3), 204L)

  # next call with the same JWT -> 401
  call2 <- http_call(paste0(srv$url, "/mcp"), "POST",
                     c(Origin = "http://127.0.0.1",
                       `Content-Type` = "application/json",
                       Accept = "application/json, text/event-stream",
                       Authorization = paste("Bearer", mint$token)),
                     body = list(jsonrpc = "2.0", id = 3,
                                 method = "tools/list",
                                 params = list()))
  expect_equal(httr2::resp_status(call2), 401L)
  expect_true(grepl("Bearer",
                    httr2::resp_header(call2, "WWW-Authenticate") %||% "",
                    fixed = TRUE))
})

test_that("mint payload validation: missing user_id / unknown user / scope subset", {
  start_once()
  mint_url <- paste0(srv$url, "/admin/tokens/mint")
  # missing user_id
  r1 <- http_call(mint_url, "POST", H(srv),
                  body = list(name = "x", scopes = list("mcp:read"),
                              ttl = 60L))
  expect_equal(httr2::resp_status(r1), 400L)
  # unknown user
  r2 <- http_call(mint_url, "POST", H(srv),
                  body = list(user_id = "u_nope",
                              scopes = list("mcp:read"), ttl = 60L))
  expect_equal(httr2::resp_status(r2), 404L)
  # bad scope (not in supported)
  r0 <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                  body = list(username = "scopetest"))
  expect_equal(httr2::resp_status(r0), 201L)
  uid <- jsbody(r0)$id
  r3 <- http_call(mint_url, "POST", H(srv),
                  body = list(user_id = uid,
                              scopes = list("evil:scope"),
                              ttl = 60L))
  expect_equal(httr2::resp_status(r3), 400L)
})

test_that("revoke unknown jti -> 404", {
  start_once()
  r <- http_call(paste0(srv$url, "/admin/tokens/jti_missing/revoke"),
                 "POST", H(srv))
  expect_equal(httr2::resp_status(r), 404L)
})

# ----- /admin/users/{id}/tokens ------------------------------------------

test_that("GET /admin/users/{id}/tokens lists active by default", {
  start_once()
  # create user
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                  body = list(username = "carol"))
  uid <- jsbody(cu)$id
  # mint two tokens, revoke one
  m1 <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"),
                         "POST", H(srv),
                         body = list(user_id = uid, name = "a",
                                     scopes = list("mcp:read"),
                                     ttl = 60L)))
  m2 <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"),
                         "POST", H(srv),
                         body = list(user_id = uid, name = "b",
                                     scopes = list("mcp:read"),
                                     ttl = 60L)))
  http_call(sprintf("%s/admin/tokens/%s/revoke", srv$url, m1$jti),
            "POST", H(srv))

  # default: only active
  r1 <- http_call(sprintf("%s/admin/users/%s/tokens", srv$url, uid),
                  "GET", H(srv))
  expect_equal(httr2::resp_status(r1), 200L)
  active <- jsbody(r1)$tokens
  expect_equal(length(active), 1L)
  expect_equal(active[[1L]]$jti, m2$jti)

  # include_revoked
  r2 <- http_call(sprintf("%s/admin/users/%s/tokens?include_revoked=true",
                          srv$url, uid),
                  "GET", H(srv))
  expect_equal(httr2::resp_status(r2), 200L)
  all <- jsbody(r2)$tokens
  expect_equal(length(all), 2L)
})

# ----- /admin/tokens/{jti}/reactivate + DELETE ---------------------------

test_that("mint -> revoke -> reactivate restores the JWT", {
  start_once()
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                  body = list(username = sprintf("react-%d",
                                                 as.integer(Sys.time()))))
  uid <- jsbody(cu)$id
  m <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"),
                        "POST", H(srv),
                        body = list(user_id = uid, name = "ci",
                                    scopes = list("mcp:read"),
                                    ttl = 3600L)))

  # `initialize` establishes its own session, so it's a self-contained
  # probe: 200 when the bearer is accepted, 401 once revoked.
  probe <- function() http_call(paste0(srv$url, "/mcp"), "POST",
    c(Origin = "http://127.0.0.1",
      `Content-Type` = "application/json",
      Accept = "application/json, text/event-stream",
      Authorization = paste("Bearer", m$token)),
    body = list(jsonrpc = "2.0", id = 1, method = "initialize",
                params = list(protocolVersion = "2025-06-18",
                              capabilities = list())))

  # works -> revoke -> 401 -> reactivate -> works again
  expect_equal(httr2::resp_status(probe()), 200L)
  rv <- http_call(sprintf("%s/admin/tokens/%s/revoke", srv$url, m$jti),
                  "POST", H(srv))
  expect_equal(httr2::resp_status(rv), 204L)
  expect_equal(httr2::resp_status(probe()), 401L)

  re <- http_call(sprintf("%s/admin/tokens/%s/reactivate", srv$url, m$jti),
                  "POST", H(srv))
  expect_equal(httr2::resp_status(re), 200L)
  expect_false(jsbody(re)$revoked)
  expect_equal(jsbody(re)$jti, m$jti)
  expect_equal(httr2::resp_status(probe()), 200L)
})

test_that("reactivate is idempotent on an active token; 404 for unknown", {
  start_once()
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                  body = list(username = sprintf("react2-%d",
                                                 as.integer(Sys.time()))))
  uid <- jsbody(cu)$id
  m <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"),
                        "POST", H(srv),
                        body = list(user_id = uid, name = "ci",
                                    scopes = list("mcp:read"),
                                    ttl = 60L)))
  # already active -> still 200, still active
  r1 <- http_call(sprintf("%s/admin/tokens/%s/reactivate", srv$url, m$jti),
                  "POST", H(srv))
  expect_equal(httr2::resp_status(r1), 200L)
  expect_false(jsbody(r1)$revoked)
  # unknown jti -> 404
  r2 <- http_call(paste0(srv$url, "/admin/tokens/jti_missing/reactivate"),
                  "POST", H(srv))
  expect_equal(httr2::resp_status(r2), 404L)
})

test_that("DELETE token (active or revoked) -> 204; unknown -> 404", {
  start_once()
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                  body = list(username = sprintf("del-%d",
                                                 as.integer(Sys.time()))))
  uid <- jsbody(cu)$id
  # delete an ACTIVE token
  ma <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"),
                         "POST", H(srv),
                         body = list(user_id = uid, name = "active",
                                     scopes = list("mcp:read"),
                                     ttl = 60L)))
  da <- http_call(sprintf("%s/admin/tokens/%s", srv$url, ma$jti),
                  "DELETE", H(srv))
  expect_equal(httr2::resp_status(da), 204L)
  # delete a REVOKED token
  mr <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"),
                         "POST", H(srv),
                         body = list(user_id = uid, name = "revoked",
                                     scopes = list("mcp:read"),
                                     ttl = 60L)))
  http_call(sprintf("%s/admin/tokens/%s/revoke", srv$url, mr$jti),
            "POST", H(srv))
  dr <- http_call(sprintf("%s/admin/tokens/%s", srv$url, mr$jti),
                  "DELETE", H(srv))
  expect_equal(httr2::resp_status(dr), 204L)
  # both gone from the include_revoked listing
  lr <- jsbody(http_call(
    sprintf("%s/admin/users/%s/tokens?include_revoked=true", srv$url, uid),
    "GET", H(srv)))
  expect_equal(length(lr$tokens), 0L)
  # delete unknown -> 404
  d404 <- http_call(paste0(srv$url, "/admin/tokens/jti_missing"),
                    "DELETE", H(srv))
  expect_equal(httr2::resp_status(d404), 404L)
})

test_that("delete frees the name so it can be minted again (regression)", {
  start_once()
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                  body = list(username = sprintf("reuse-%d",
                                                 as.integer(Sys.time()))))
  uid <- jsbody(cu)$id
  m1 <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"),
                         "POST", H(srv),
                         body = list(user_id = uid, name = "key-a",
                                     scopes = list("mcp:read"),
                                     ttl = 60L)))
  http_call(sprintf("%s/admin/tokens/%s/revoke", srv$url, m1$jti),
            "POST", H(srv))
  # While the revoked row exists, re-minting the same name still 409s.
  rconflict <- http_call(paste0(srv$url, "/admin/tokens/mint"),
                         "POST", H(srv),
                         body = list(user_id = uid, name = "key-a",
                                     scopes = list("mcp:read"),
                                     ttl = 60L))
  expect_equal(httr2::resp_status(rconflict), 409L)
  # Delete it, then the same name mints cleanly.
  http_call(sprintf("%s/admin/tokens/%s", srv$url, m1$jti),
            "DELETE", H(srv))
  rok <- http_call(paste0(srv$url, "/admin/tokens/mint"),
                   "POST", H(srv),
                   body = list(user_id = uid, name = "key-a",
                               scopes = list("mcp:read"),
                               ttl = 60L))
  expect_equal(httr2::resp_status(rok), 200L)
})

test_that("reactivate + delete require admin (401 missing, 403 non-admin)", {
  start_once()
  # a token to target, and a non-admin user's JWT
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                  body = list(username = sprintf("gate-%d",
                                                 as.integer(Sys.time()))))
  uid <- jsbody(cu)$id
  m <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"),
                        "POST", H(srv),
                        body = list(user_id = uid, name = "ci",
                                    scopes = list("mcp:read"),
                                    ttl = 600L)))
  no_auth   <- c(Origin = "http://127.0.0.1")
  user_auth <- c(Origin = "http://127.0.0.1",
                 Authorization = paste("Bearer", m$token))
  react_url <- sprintf("%s/admin/tokens/%s/reactivate", srv$url, m$jti)
  del_url   <- sprintf("%s/admin/tokens/%s", srv$url, m$jti)

  expect_equal(httr2::resp_status(
    http_call(react_url, "POST", no_auth)), 401L)
  expect_equal(httr2::resp_status(
    http_call(del_url, "DELETE", no_auth)), 401L)
  expect_equal(httr2::resp_status(
    http_call(react_url, "POST", user_auth)), 403L)
  expect_equal(httr2::resp_status(
    http_call(del_url, "DELETE", user_auth)), 403L)
})

# ----- admin auth: admin-user JWT path ----------------------------------

test_that("an is_admin user's JWT can call /admin/*", {
  start_once()
  # create an admin user via bootstrap
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                  body = list(username = "boss",
                              is_admin = TRUE))
  expect_equal(httr2::resp_status(cu), 201L)
  uid <- jsbody(cu)$id

  # mint a JWT for that user (under bootstrap auth)
  m <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"),
                        "POST", H(srv),
                        body = list(user_id = uid, name = "ssh",
                                    scopes = list("mcp:read"),
                                    ttl = 600L)))
  expect_true(nzchar(m$token))

  # use the JWT (not bootstrap) on /admin/users
  r <- http_call(paste0(srv$url, "/admin/users"), "GET",
                 c(Origin = "http://127.0.0.1",
                   Authorization = paste("Bearer", m$token)))
  expect_equal(httr2::resp_status(r), 200L)
})

test_that("a non-admin user's JWT is rejected with 403", {
  start_once()
  # plain user
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                  body = list(username = "intern"))
  uid <- jsbody(cu)$id
  m <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"),
                        "POST", H(srv),
                        body = list(user_id = uid, name = "tok",
                                    scopes = list("mcp:read"),
                                    ttl = 600L)))
  r <- http_call(paste0(srv$url, "/admin/users"), "GET",
                 c(Origin = "http://127.0.0.1",
                   Authorization = paste("Bearer", m$token)))
  expect_equal(httr2::resp_status(r), 403L)
})

test_that("GET /admin/users honors limit + cursor pagination", {
  start_once()
  # Seed a known prefix so we can find them deterministically.
  pfx <- sprintf("pg-%d-", as.integer(Sys.time()))
  ids <- character(0L)
  for (i in seq_len(5L)) {
    r <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                   body = list(username = paste0(pfx, i)))
    expect_equal(httr2::resp_status(r), 201L)
    ids <- c(ids, jsbody(r)$id)
  }

  # Page 1: limit=2 returns 2 users and a next_cursor
  r1 <- http_call(paste0(srv$url, "/admin/users?limit=2"), "GET",
                  H(srv))
  expect_equal(httr2::resp_status(r1), 200L)
  b1 <- jsbody(r1)
  expect_equal(length(b1$users), 2L)
  expect_true(nzchar(b1$next_cursor))

  # Page 2 follows
  r2 <- http_call(sprintf("%s/admin/users?limit=2&cursor=%s",
                          srv$url, b1$next_cursor),
                  "GET", H(srv))
  expect_equal(httr2::resp_status(r2), 200L)
  b2 <- jsbody(r2)
  expect_equal(length(b2$users), 2L)

  # Ensure we never see the same user twice across pages
  seen1 <- vapply(b1$users, function(u) u$id, character(1L))
  seen2 <- vapply(b2$users, function(u) u$id, character(1L))
  expect_equal(length(intersect(seen1, seen2)), 0L)
})

test_that("GET /admin/users rejects negative limit", {
  start_once()
  r <- http_call(paste0(srv$url, "/admin/users?limit=-3"), "GET",
                 H(srv))
  expect_equal(httr2::resp_status(r), 400L)
})

test_that("revoke is idempotent (re-revoke -> 204)", {
  start_once()
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                  body = list(username = sprintf("idem-%d",
                                                 as.integer(Sys.time()))))
  uid <- jsbody(cu)$id
  m <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"),
                        "POST", H(srv),
                        body = list(user_id = uid, name = "idem",
                                    scopes = list("mcp:read"),
                                    ttl = 60L)))
  r1 <- http_call(sprintf("%s/admin/tokens/%s/revoke",
                          srv$url, m$jti),
                  "POST", H(srv))
  expect_equal(httr2::resp_status(r1), 204L)
  r2 <- http_call(sprintf("%s/admin/tokens/%s/revoke",
                          srv$url, m$jti),
                  "POST", H(srv))
  expect_equal(httr2::resp_status(r2), 204L)
})

test_that("POST /admin/tokens/mint rejects ttl > max", {
  start_once()
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                  body = list(username = sprintf("ttl-%d",
                                                 as.integer(Sys.time()))))
  uid <- jsbody(cu)$id
  # admin max_ttl defaults to 1 year (365 days); 10 years > max.
  r <- http_call(paste0(srv$url, "/admin/tokens/mint"), "POST", H(srv),
                 body = list(user_id = uid, name = "huge",
                             scopes = list("mcp:read"),
                             ttl = 10L * 365L * 24L * 3600L))
  expect_equal(httr2::resp_status(r), 400L)
  expect_match(jsbody(r)$message, "ttl exceeds")
})

test_that("PATCH /admin/users rejects unknown fields with 400", {
  start_once()
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                  body = list(username = sprintf("uf-%d",
                                                 as.integer(Sys.time()))))
  uid <- jsbody(cu)$id
  r <- http_call(sprintf("%s/admin/users/%s", srv$url, uid),
                 "PATCH", H(srv),
                 body = list(nonsense = "value"))
  expect_equal(httr2::resp_status(r), 400L)
  expect_match(jsbody(r)$message, "unknown field")
})

test_that("non-bootstrap admins cannot flip is_admin via PATCH/POST", {
  start_once()
  # set up an admin user (via bootstrap)
  cu1 <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                   body = list(username = "boss2", is_admin = TRUE))
  uid_admin <- jsbody(cu1)$id
  m_admin <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"),
                              "POST", H(srv),
                              body = list(user_id = uid_admin,
                                          name = "tk",
                                          scopes = list("mcp:read"),
                                          ttl = 600L)))
  admin_auth <- c(Origin = "http://127.0.0.1",
                  `Content-Type` = "application/json",
                  Authorization = paste("Bearer", m_admin$token))

  # admin (JWT) creates a user, tries to set is_admin -> 403
  r1 <- http_call(paste0(srv$url, "/admin/users"), "POST", admin_auth,
                  body = list(username = "joe", is_admin = TRUE))
  expect_equal(httr2::resp_status(r1), 403L)

  # admin (JWT) PATCH another user setting is_admin -> 403
  pu <- http_call(paste0(srv$url, "/admin/users"), "POST", H(srv),
                  body = list(username = "joe2"))
  uid <- jsbody(pu)$id
  r2 <- http_call(sprintf("%s/admin/users/%s", srv$url, uid),
                  "PATCH", admin_auth,
                  body = list(is_admin = TRUE))
  expect_equal(httr2::resp_status(r2), 403L)
})
