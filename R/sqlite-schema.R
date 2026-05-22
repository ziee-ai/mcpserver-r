# SQLite schema for the users + tokens store -----------------------------

# Two tables: `mcp_users` (the system of record for user identities) and
# `mcp_tokens` (metadata for issued JWTs, used for instant revocation and
# admin-UI listing). The JWT secret itself is never stored; tokens are
# RS256-signed by the AS and verified with the public JWKS.
#
# Idempotent: safe to call against an existing DB; rows are not touched.
# WAL mode lets the admin UI (e.g. Directus) and `mcpserver-r` share the
# file with sensible reader/writer concurrency.

#' Initialize the mcpserver SQLite schema
#'
#' Creates the `mcp_users` and `mcp_tokens` tables if they don't already
#' exist, and switches the database to WAL journaling. Safe to call
#' repeatedly. Used internally by [new_mcp_store()] and exposed for
#' explicit deployment scripts.
#'
#' @param path Filesystem path to the SQLite database. Parent directories
#'   are created as needed.
#' @return Invisibly returns `path`.
#' @export
mcp_init_schema <- function(path) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RSQLite", quietly = TRUE)) {
    stop("mcp_init_schema() requires the 'DBI' and 'RSQLite' packages. ",
         "Install them with install.packages(c('DBI','RSQLite')).",
         call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS mcp_users (",
    "  id         TEXT PRIMARY KEY,",
    "  username   TEXT NOT NULL UNIQUE,",
    "  email      TEXT,",
    "  is_admin   INTEGER NOT NULL DEFAULT 0,",
    "  groups     TEXT,",
    "  metadata   TEXT,",
    "  created_at TEXT NOT NULL,",
    "  updated_at TEXT NOT NULL",
    ")"))
  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS mcp_tokens (",
    "  jti          TEXT PRIMARY KEY,",
    "  user_id      TEXT NOT NULL",
    "    REFERENCES mcp_users(id) ON DELETE CASCADE,",
    "  name         TEXT NOT NULL,",
    "  scopes       TEXT NOT NULL,",
    "  created_at   TEXT NOT NULL,",
    "  expires_at   TEXT NOT NULL,",
    "  last_used_at TEXT,",
    "  revoked      INTEGER NOT NULL DEFAULT 0,",
    "  UNIQUE(user_id, name)",
    ")"))
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_mcp_tokens_user ON mcp_tokens(user_id)")
  invisible(path)
}
