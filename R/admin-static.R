# Static handler for the bundled React SPA at /admin/ui/* ----------------

# Serves files from `inst/admin-ui/dist/` produced by `web-admin/`'s
# `npm run build && npm run sync`. Adds:
#   * gzip pre-compressed serving (Vite emits .gz siblings) when the
#     client advertises Accept-Encoding: gzip.
#   * SPA fallback: paths that don't match a real file (and aren't
#     under /assets/) return index.html. This is what makes React
#     Router's deep links work after a hard reload.
#   * Path traversal protection.
#   * Cache-Control: immutable for hashed `/assets/*`; no-cache for
#     index.html (so a fresh deploy is picked up immediately).
#   * A strict Content-Security-Policy on index.html.

admin_ui_dist_dir <- function(state) {
  cfg_dir <- state$admin$ui_dist_dir
  if (!is.null(cfg_dir) && nzchar(cfg_dir)) return(cfg_dir)
  system.file("admin-ui", "dist", package = "mcpserver")
}

content_type_for <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    "html" = "text/html; charset=utf-8",
    "js"   = "application/javascript; charset=utf-8",
    "mjs"  = "application/javascript; charset=utf-8",
    "css"  = "text/css; charset=utf-8",
    "json" = "application/json",
    "svg"  = "image/svg+xml",
    "png"  = "image/png",
    "jpg"  = "image/jpeg",
    "jpeg" = "image/jpeg",
    "gif"  = "image/gif",
    "ico"  = "image/x-icon",
    "woff" = "font/woff",
    "woff2" = "font/woff2",
    "ttf"  = "font/ttf",
    "map"  = "application/json",
    "txt"  = "text/plain; charset=utf-8",
    "application/octet-stream"
  )
}

# Strip the "/admin/ui" mount prefix to get the asset path inside dist/.
strip_ui_prefix <- function(uri) {
  p <- sub("\\?.*$", "", uri)
  p <- sub("^/admin/ui/?", "", p)
  p <- sub("^/+", "", p)
  p
}

# Accept gzip if the client offers it.
client_accepts_gzip <- function(req) {
  ae <- header_get(req$headers, "Accept-Encoding", default = "")
  grepl("gzip", ae, ignore.case = TRUE)
}

# Reject anything that looks like path traversal. We don't trust
# fs::path_norm-style normalization here; refuse outright if a "..",
# leading slash after stripping, or absolute drive-style prefix appears.
unsafe_path <- function(rel) {
  if (identical(rel, "")) return(FALSE)
  if (grepl("(^|/)\\.\\.(/|$)", rel)) return(TRUE)
  if (grepl("^/", rel)) return(TRUE)
  # Path components that resolve outside the root are caught by the
  # final realpath comparison; this is defense in depth.
  FALSE
}

serve_admin_static <- function(state, req) {
  dist_dir <- admin_ui_dist_dir(state)
  if (!nzchar(dist_dir) || !dir.exists(dist_dir)) {
    return(admin_error(503L, "admin_ui_missing",
                       "admin UI dist directory not present; run ",
                       "`npm run build && npm run sync` in web-admin/"))
  }
  rel <- strip_ui_prefix(req$uri %||% "")
  if (unsafe_path(rel)) {
    return(admin_error(400L, "bad_request", "invalid path"))
  }
  if (identical(rel, "")) {
    rel <- "index.html"
  }
  target <- file.path(dist_dir, rel)
  # Final containment check: real path must remain under dist_dir.
  real_target <- normalizePath(target, mustWork = FALSE)
  real_root <- normalizePath(dist_dir, mustWork = TRUE)
  if (!startsWith(real_target, real_root)) {
    return(admin_error(400L, "bad_request", "invalid path"))
  }

  # SPA fallback: any /admin/ui/<deep-link> that isn't an actual file
  # is rewritten to index.html so React Router can handle it client-side.
  if (!file.exists(target) || dir.exists(target)) {
    target <- file.path(dist_dir, "index.html")
    rel    <- "index.html"
  }

  serve_static_file(target, rel, req)
}

serve_static_file <- function(target, rel, req) {
  is_index <- identical(rel, "index.html")
  ct <- content_type_for(target)
  # Pre-compressed sibling, if any
  gz_target <- paste0(target, ".gz")
  serve_gz <- !is_index && file.exists(gz_target) && client_accepts_gzip(req)
  body_path <- if (serve_gz) gz_target else target
  body <- readBin(body_path, what = "raw", n = file.info(body_path)$size)

  headers <- c(`Content-Type` = ct)
  if (serve_gz) {
    headers <- c(headers, c(`Content-Encoding` = "gzip",
                            Vary = "Accept-Encoding"))
  }
  if (is_index) {
    headers <- c(
      headers,
      c(`Cache-Control` = "no-cache",
        `Content-Security-Policy` = paste(
          "default-src 'self';",
          "script-src 'self';",
          "style-src 'self' 'unsafe-inline';",
          "img-src 'self' data:;",
          "font-src 'self' data:;",
          "connect-src 'self';",
          "frame-ancestors 'none';",
          "base-uri 'self';"),
        `X-Content-Type-Options` = "nosniff",
        `X-Frame-Options` = "DENY"))
  } else if (grepl("^assets/", rel)) {
    # Vite emits hash-named asset files; they're safe to cache forever.
    headers <- c(headers,
                 c(`Cache-Control` = "public, max-age=31536000, immutable"))
  }
  list(status = 200L, headers = headers, body = body)
}
