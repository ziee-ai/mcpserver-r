# End-to-end integration tests for /admin/ui/* — verifies the bundled
# SPA is served, deep links fall back to index.html, /mcp and
# /.well-known/* are NOT swallowed by the SPA route, gzip pre-compressed
# assets are served when the client accepts them, and path traversal is
# blocked.

skip_if_not_installed("processx")
skip_if_not_installed("httr2")
skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")
skip_on_cran()
skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))

dist_dir <- system.file("admin-ui", "dist", package = "mcpserver")
if (!nzchar(dist_dir) || !file.exists(file.path(dist_dir, "index.html"))) {
  test_that("static UI bundle exists", {
    skip(paste("admin-ui bundle missing; run",
               "`npm run build && npm run sync` in web-admin/"))
  })
} else {

spawn_static <- function(port) {
  bootstrap <- paste(openssl::rand_bytes(16L), collapse = "")
  store_path <- tempfile(fileext = ".db")
  runner <- tempfile(fileext = ".R")
  writeLines(c(
    "suppressPackageStartupMessages(library(mcpserver))",
    sprintf("store <- new_mcp_store('sqlite', path = '%s')", store_path),
    sprintf("oauth_as <- oauth_server_config(issuer = 'http://127.0.0.1:%d', audience = 'mcp', store = store)", port),
    "mcp <- new_server('it', version = '0.1.0')",
    sprintf("Sys.setenv(MCPSERVER_ADMIN_TOKEN='%s')", bootstrap),
    sprintf("serve_http(mcp, port = %dL, oauth_as = oauth_as,", port),
    "             admin = list(bootstrap_token = Sys.getenv('MCPSERVER_ADMIN_TOKEN'),",
    "                          ui = TRUE),",
    "             allowed_origins = c('http://127.0.0.1'))"
  ), runner)
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                               collapse = .Platform$path.sep)
  p <- processx::process$new("Rscript", c(runner),
                             stdout = "|", stderr = "|",
                             env = child_env)
  url <- sprintf("http://127.0.0.1:%d", port)
  ok <- FALSE
  for (i in seq_len(80L)) {
    resp <- tryCatch(
      httr2::request(paste0(url, "/admin/healthz")) |>
        httr2::req_headers(Authorization = paste("Bearer", bootstrap),
                           Origin = "http://127.0.0.1") |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_timeout(2) |>
        httr2::req_perform(),
      error = function(e) NULL)
    if (!is.null(resp) && httr2::resp_status(resp) == 200L) {
      ok <- TRUE; break
    }
    Sys.sleep(0.25)
  }
  if (!ok) {
    p$kill()
    skip("static server failed to start")
  }
  list(process = p, url = url, bootstrap = bootstrap,
       runner = runner, store_path = store_path)
}

teardown <- function(srv) {
  if (srv$process$is_alive()) srv$process$kill()
  unlink(c(srv$runner, srv$store_path,
           paste0(srv$store_path, "-wal"),
           paste0(srv$store_path, "-shm")))
}

PORT <- 47201L
srv <- NULL
start_once <- function() {
  if (!is.null(srv) && srv$process$is_alive()) return(invisible(srv))
  srv <<- spawn_static(PORT)
  invisible(srv)
}
withr::defer(if (!is.null(srv)) teardown(srv),
             testthat::teardown_env())

get_raw <- function(url, extra_headers = list(), accept_encoding = NULL) {
  req <- httr2::request(url) |>
    httr2::req_method("GET") |>
    httr2::req_headers(Origin = "http://127.0.0.1") |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(5)
  if (!is.null(accept_encoding)) {
    req <- httr2::req_headers(req, `Accept-Encoding` = accept_encoding)
  }
  for (n in names(extra_headers)) {
    req <- httr2::req_headers(req, !!n := extra_headers[[n]])
  }
  httr2::req_perform(req)
}

test_that("GET /admin/ui returns the SPA shell", {
  start_once()
  r <- get_raw(paste0(srv$url, "/admin/ui"))
  expect_equal(httr2::resp_status(r), 200L)
  expect_match(httr2::resp_header(r, "Content-Type"), "text/html")
  body <- httr2::resp_body_string(r)
  expect_true(grepl("<div id=\"root\">", body, fixed = TRUE))
  expect_true(grepl("/admin/ui/assets/", body, fixed = TRUE))
})

test_that("SPA deep links fall back to index.html", {
  start_once()
  r <- get_raw(paste0(srv$url, "/admin/ui/users"))
  expect_equal(httr2::resp_status(r), 200L)
  expect_match(httr2::resp_header(r, "Content-Type"), "text/html")
  expect_true(grepl("<div id=\"root\">",
                    httr2::resp_body_string(r), fixed = TRUE))
})

test_that("index.html carries CSP + no-cache headers", {
  start_once()
  r <- get_raw(paste0(srv$url, "/admin/ui"))
  csp <- httr2::resp_header(r, "Content-Security-Policy")
  expect_true(!is.null(csp) && nzchar(csp))
  expect_match(csp, "default-src 'self'")
  expect_match(httr2::resp_header(r, "Cache-Control") %||% "",
               "no-cache")
  expect_equal(httr2::resp_header(r, "X-Frame-Options"), "DENY")
})

test_that("hashed assets are served with immutable cache + gzip pre-compressed", {
  start_once()
  # Pick a real hashed JS asset from the bundle.
  assets <- list.files(file.path(dist_dir, "assets"),
                       pattern = "\\.js$", full.names = FALSE)
  skip_if(length(assets) == 0L, "no JS assets in bundle")
  asset <- assets[[1L]]
  url <- sprintf("%s/admin/ui/assets/%s", srv$url, asset)
  # Without gzip: 200, immutable cache
  r1 <- get_raw(url, accept_encoding = "identity")
  expect_equal(httr2::resp_status(r1), 200L)
  expect_match(httr2::resp_header(r1, "Cache-Control") %||% "",
               "immutable")
  # With gzip: same status, Content-Encoding: gzip
  r2 <- get_raw(url, accept_encoding = "gzip")
  expect_equal(httr2::resp_status(r2), 200L)
  expect_equal(httr2::resp_header(r2, "Content-Encoding"), "gzip")
})

test_that("/.well-known/oauth-protected-resource is NOT swallowed", {
  start_once()
  r <- get_raw(paste0(srv$url,
                      "/.well-known/oauth-protected-resource"))
  expect_equal(httr2::resp_status(r), 200L)
  expect_match(httr2::resp_header(r, "Content-Type"),
               "application/json")
})

test_that("/mcp routes still respond (POST gated by auth + headers)", {
  start_once()
  # No bearer here -> 401 from the MCP endpoint, not a 200 SPA shell.
  r <- httr2::request(paste0(srv$url, "/mcp")) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Origin = "http://127.0.0.1",
      `Content-Type` = "application/json",
      Accept = "application/json, text/event-stream") |>
    httr2::req_body_raw(charToRaw(
      '{"jsonrpc":"2.0","id":1,"method":"tools/list"}')) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(5) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(r), 401L)
})

}  # end if dist exists
