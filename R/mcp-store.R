# Users + tokens store ---------------------------------------------------

# `new_mcp_store()` returns one object with two namespaces (`$users` and
# `$tokens`) sharing a single backing connection. Two drivers exist:
# "memory" (env-based, ephemeral, default) and "sqlite" (DBI/RSQLite).
#
# The store is the integration contract with the admin UI: a generic SQL
# admin tool can be pointed at the same SQLite file and edit users/tokens
# directly. JWT signing still happens in `mcpserver-r` because the AS's
# private key lives there.

#' Open a users + tokens store
#'
#' Returns an object with `$users` (CRUD on user records) and `$tokens`
#' (CRUD on issued-JWT metadata) namespaces. Both share one backing
#' connection. Pass the returned object to [oauth_server_config()] as
#' the `store` argument.
#'
#' @param driver Either `"memory"` (default; ephemeral env-based) or
#'   `"sqlite"` (persistent via DBI/RSQLite).
#' @param path For `driver = "sqlite"`, filesystem path to the database
#'   file. Created if absent.
#' @return A list with class `"mcp_store"` and elements `$users`,
#'   `$tokens`, `$close()`, `$revocation_store()`.
#' @export
new_mcp_store <- function(driver = c("memory", "sqlite"), path = NULL) {
  driver <- match.arg(driver)
  if (identical(driver, "memory")) {
    return(new_memory_store())
  }
  if (is.null(path) || !nzchar(path)) {
    stop("driver = 'sqlite' requires a non-empty path", call. = FALSE)
  }
  new_sqlite_store(path)
}

# ----- Memory driver ----------------------------------------------------

new_memory_store <- function() {
  users_env  <- new.env(parent = emptyenv())
  tokens_env <- new.env(parent = emptyenv())
  state <- new.env(parent = emptyenv())
  state$users_by_username <- new.env(parent = emptyenv())
  state$tokens_by_user <- new.env(parent = emptyenv())

  users <- list(
    add = function(user) {
      validate_user(user)
      rec <- fill_user_defaults(user)
      if (exists(rec$id, envir = users_env, inherits = FALSE)) {
        stop(sprintf("user id '%s' already exists", rec$id),
             call. = FALSE)
      }
      if (exists(rec$username, envir = state$users_by_username,
                 inherits = FALSE)) {
        stop(sprintf("username '%s' already exists", rec$username),
             call. = FALSE)
      }
      assign(rec$id, rec, envir = users_env)
      assign(rec$username, rec$id, envir = state$users_by_username)
      rec
    },
    get = function(id) {
      if (!exists(id, envir = users_env, inherits = FALSE)) return(NULL)
      get(id, envir = users_env, inherits = FALSE)
    },
    find_by_username = function(username) {
      if (!exists(username, envir = state$users_by_username,
                  inherits = FALSE)) return(NULL)
      id <- get(username, envir = state$users_by_username,
                inherits = FALSE)
      get(id, envir = users_env, inherits = FALSE)
    },
    list = function(limit = NULL, cursor = NULL) {
      ids <- ls(users_env, all.names = TRUE)
      all <- lapply(ids, function(i) get(i, envir = users_env,
                                         inherits = FALSE))
      # Stable ordering by (created_at, id) so cursor pagination is
      # deterministic regardless of insertion order.
      ord <- order(vapply(all, function(u) u$created_at, character(1L)),
                   vapply(all, function(u) u$id, character(1L)))
      all <- all[ord]
      if (!is.null(cursor) && nzchar(cursor)) {
        idx <- vapply(all, function(u) identical(u$id, cursor),
                      logical(1L))
        if (any(idx)) {
          start <- which(idx)[[1L]] + 1L
          all <- if (start > length(all)) list() else all[start:length(all)]
        }
      }
      if (!is.null(limit) && is.finite(limit) && limit >= 0) {
        all <- head(all, as.integer(limit))
      }
      all
    },
    update = function(id, changes) {
      if (!exists(id, envir = users_env, inherits = FALSE)) {
        stop(sprintf("no such user: %s", id), call. = FALSE)
      }
      rec <- get(id, envir = users_env, inherits = FALSE)
      if (!is.null(changes$username) &&
          !identical(changes$username, rec$username)) {
        if (exists(changes$username, envir = state$users_by_username,
                   inherits = FALSE)) {
          stop(sprintf("username '%s' already exists", changes$username),
               call. = FALSE)
        }
        rm(list = rec$username, envir = state$users_by_username)
        assign(changes$username, id, envir = state$users_by_username)
      }
      for (key in names(changes)) {
        rec[[key]] <- changes[[key]]
      }
      rec$updated_at <- iso_now()
      assign(id, rec, envir = users_env)
      rec
    },
    delete = function(id) {
      if (!exists(id, envir = users_env, inherits = FALSE)) return(FALSE)
      rec <- get(id, envir = users_env, inherits = FALSE)
      rm(list = id, envir = users_env)
      if (exists(rec$username, envir = state$users_by_username,
                 inherits = FALSE)) {
        rm(list = rec$username, envir = state$users_by_username)
      }
      # cascade tokens
      if (exists(id, envir = state$tokens_by_user, inherits = FALSE)) {
        jtis <- get(id, envir = state$tokens_by_user, inherits = FALSE)
        for (j in jtis) {
          if (exists(j, envir = tokens_env, inherits = FALSE)) {
            rm(list = j, envir = tokens_env)
          }
        }
        rm(list = id, envir = state$tokens_by_user)
      }
      TRUE
    }
  )

  tokens <- list(
    add = function(token) {
      validate_token(token)
      if (exists(token$jti, envir = tokens_env, inherits = FALSE)) {
        stop(sprintf("token jti '%s' already exists", token$jti),
             call. = FALSE)
      }
      # uniqueness check: (user_id, name)
      existing <- if (exists(token$user_id, envir = state$tokens_by_user,
                             inherits = FALSE)) {
        get(token$user_id, envir = state$tokens_by_user, inherits = FALSE)
      } else character(0L)
      for (j in existing) {
        e <- get(j, envir = tokens_env, inherits = FALSE)
        if (identical(e$name, token$name) && !isTRUE(e$revoked)) {
          stop(sprintf(
            "token name '%s' already in use for this user",
            token$name), call. = FALSE)
        }
      }
      rec <- fill_token_defaults(token)
      assign(rec$jti, rec, envir = tokens_env)
      assign(token$user_id, c(existing, rec$jti),
             envir = state$tokens_by_user)
      rec
    },
    get = function(jti) {
      if (!exists(jti, envir = tokens_env, inherits = FALSE)) return(NULL)
      get(jti, envir = tokens_env, inherits = FALSE)
    },
    list_for_user = function(user_id, include_revoked = FALSE) {
      if (!exists(user_id, envir = state$tokens_by_user,
                  inherits = FALSE)) return(list())
      jtis <- get(user_id, envir = state$tokens_by_user, inherits = FALSE)
      out <- lapply(jtis, function(j) {
        if (exists(j, envir = tokens_env, inherits = FALSE)) {
          get(j, envir = tokens_env, inherits = FALSE)
        } else NULL
      })
      out <- out[!vapply(out, is.null, logical(1L))]
      if (!isTRUE(include_revoked)) {
        out <- out[!vapply(out, function(t) isTRUE(t$revoked),
                           logical(1L))]
      }
      out
    },
    revoke = function(jti) {
      if (!exists(jti, envir = tokens_env, inherits = FALSE)) return(FALSE)
      rec <- get(jti, envir = tokens_env, inherits = FALSE)
      rec$revoked <- TRUE
      assign(jti, rec, envir = tokens_env)
      TRUE
    },
    reactivate = function(jti) {
      if (!exists(jti, envir = tokens_env, inherits = FALSE)) return(FALSE)
      rec <- get(jti, envir = tokens_env, inherits = FALSE)
      rec$revoked <- FALSE
      assign(jti, rec, envir = tokens_env)
      TRUE
    },
    delete = function(jti) {
      if (!exists(jti, envir = tokens_env, inherits = FALSE)) return(FALSE)
      rec <- get(jti, envir = tokens_env, inherits = FALSE)
      rm(list = jti, envir = tokens_env)
      # drop from the (user_id -> jti vector) index
      if (exists(rec$user_id, envir = state$tokens_by_user,
                 inherits = FALSE)) {
        jtis <- get(rec$user_id, envir = state$tokens_by_user,
                    inherits = FALSE)
        assign(rec$user_id, setdiff(jtis, jti),
               envir = state$tokens_by_user)
      }
      TRUE
    },
    touch_last_used = function(jti) {
      if (!exists(jti, envir = tokens_env, inherits = FALSE)) return(FALSE)
      rec <- get(jti, envir = tokens_env, inherits = FALSE)
      rec$last_used_at <- iso_now()
      assign(jti, rec, envir = tokens_env)
      TRUE
    }
  )

  store <- list(
    driver = "memory",
    users  = users,
    tokens = tokens,
    close  = function() invisible(NULL),
    revocation_store = function() tokens
  )
  class(store) <- "mcp_store"
  store
}

# ----- SQLite driver ----------------------------------------------------

new_sqlite_store <- function(path) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RSQLite", quietly = TRUE)) {
    stop("driver = 'sqlite' requires DBI and RSQLite. ",
         "Install with install.packages(c('DBI','RSQLite')).",
         call. = FALSE)
  }
  mcp_init_schema(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")

  users <- list(
    add = function(user) {
      validate_user(user)
      rec <- fill_user_defaults(user)
      tryCatch({
        DBI::dbExecute(con,
          paste("INSERT INTO mcp_users",
                "(id, username, email, is_admin, groups, metadata,",
                " created_at, updated_at)",
                "VALUES ($id,$un,$em,$ia,$gr,$mt,$ca,$ua)"),
          params = list(id = rec$id, un = rec$username,
                        em = rec$email %||% NA_character_,
                        ia = as.integer(isTRUE(rec$is_admin)),
                        gr = encode_json_field(rec$groups),
                        mt = encode_json_field(rec$metadata),
                        ca = rec$created_at, ua = rec$updated_at))
      }, error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("UNIQUE", msg) && grepl("username", msg)) {
          stop(sprintf("username '%s' already exists", rec$username),
               call. = FALSE)
        }
        if (grepl("UNIQUE", msg) && grepl("id", msg)) {
          stop(sprintf("user id '%s' already exists", rec$id),
               call. = FALSE)
        }
        stop(e)
      })
      rec
    },
    get = function(id) {
      row <- DBI::dbGetQuery(con,
        "SELECT * FROM mcp_users WHERE id = $id",
        params = list(id = id))
      if (nrow(row) == 0L) return(NULL)
      row_to_user(row[1L, ])
    },
    find_by_username = function(username) {
      row <- DBI::dbGetQuery(con,
        "SELECT * FROM mcp_users WHERE username = $un",
        params = list(un = username))
      if (nrow(row) == 0L) return(NULL)
      row_to_user(row[1L, ])
    },
    list = function(limit = NULL, cursor = NULL) {
      params <- list()
      cursor_clause <- ""
      if (!is.null(cursor) && nzchar(cursor)) {
        cursor_clause <- paste(
          "WHERE (created_at, id) > (",
          "  SELECT created_at, id FROM mcp_users WHERE id = $cursor",
          ")")
        params$cursor <- cursor
      }
      limit_clause <- ""
      if (!is.null(limit) && is.finite(limit) && limit >= 0) {
        limit_clause <- "LIMIT $limit"
        params$limit <- as.integer(limit)
      }
      q <- paste("SELECT * FROM mcp_users",
                 cursor_clause,
                 "ORDER BY created_at ASC, id ASC",
                 limit_clause)
      rows <- if (length(params) == 0L) {
        DBI::dbGetQuery(con, q)
      } else {
        DBI::dbGetQuery(con, q, params = params)
      }
      if (nrow(rows) == 0L) return(list())
      lapply(seq_len(nrow(rows)), function(i) row_to_user(rows[i, ]))
    },
    update = function(id, changes) {
      cur <- DBI::dbGetQuery(con,
        "SELECT * FROM mcp_users WHERE id = $id",
        params = list(id = id))
      if (nrow(cur) == 0L) {
        stop(sprintf("no such user: %s", id), call. = FALSE)
      }
      cur <- row_to_user(cur[1L, ])
      merged <- cur
      for (k in names(changes)) merged[[k]] <- changes[[k]]
      merged$updated_at <- iso_now()
      tryCatch({
        DBI::dbExecute(con, paste(
          "UPDATE mcp_users SET",
          "  username = $un, email = $em, is_admin = $ia,",
          "  groups = $gr, metadata = $mt, updated_at = $ua",
          "WHERE id = $id"),
          params = list(
            un = merged$username,
            em = merged$email %||% NA_character_,
            ia = as.integer(isTRUE(merged$is_admin)),
            gr = encode_json_field(merged$groups),
            mt = encode_json_field(merged$metadata),
            ua = merged$updated_at,
            id = id))
      }, error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("UNIQUE", msg) && grepl("username", msg)) {
          stop(sprintf("username '%s' already exists",
                       merged$username), call. = FALSE)
        }
        stop(e)
      })
      merged
    },
    delete = function(id) {
      n <- DBI::dbExecute(con,
        "DELETE FROM mcp_users WHERE id = $id",
        params = list(id = id))
      n > 0L
    }
  )

  tokens <- list(
    add = function(token) {
      validate_token(token)
      rec <- fill_token_defaults(token)
      tryCatch({
        DBI::dbExecute(con, paste(
          "INSERT INTO mcp_tokens",
          "(jti, user_id, name, scopes, created_at, expires_at,",
          " last_used_at, revoked)",
          "VALUES ($jti,$uid,$nm,$sc,$ca,$ea,$lu,$rv)"),
          params = list(
            jti = rec$jti, uid = rec$user_id, nm = rec$name,
            sc = paste(rec$scopes, collapse = " "),
            ca = rec$created_at, ea = rec$expires_at,
            lu = rec$last_used_at %||% NA_character_,
            rv = as.integer(isTRUE(rec$revoked))))
      }, error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("UNIQUE", msg) && grepl("user_id", msg)) {
          stop(sprintf(
            "token name '%s' already in use for this user", rec$name),
            call. = FALSE)
        }
        if (grepl("FOREIGN KEY", msg)) {
          stop(sprintf("no such user: %s", rec$user_id), call. = FALSE)
        }
        stop(e)
      })
      rec
    },
    get = function(jti) {
      row <- DBI::dbGetQuery(con,
        "SELECT * FROM mcp_tokens WHERE jti = $jti",
        params = list(jti = jti))
      if (nrow(row) == 0L) return(NULL)
      row_to_token(row[1L, ])
    },
    list_for_user = function(user_id, include_revoked = FALSE) {
      q <- if (isTRUE(include_revoked)) {
        "SELECT * FROM mcp_tokens WHERE user_id = $uid ORDER BY created_at ASC"
      } else {
        "SELECT * FROM mcp_tokens WHERE user_id = $uid AND revoked = 0 ORDER BY created_at ASC"
      }
      rows <- DBI::dbGetQuery(con, q, params = list(uid = user_id))
      if (nrow(rows) == 0L) return(list())
      lapply(seq_len(nrow(rows)), function(i) row_to_token(rows[i, ]))
    },
    revoke = function(jti) {
      n <- DBI::dbExecute(con,
        "UPDATE mcp_tokens SET revoked = 1 WHERE jti = $jti",
        params = list(jti = jti))
      n > 0L
    },
    reactivate = function(jti) {
      n <- DBI::dbExecute(con,
        "UPDATE mcp_tokens SET revoked = 0 WHERE jti = $jti",
        params = list(jti = jti))
      n > 0L
    },
    delete = function(jti) {
      n <- DBI::dbExecute(con,
        "DELETE FROM mcp_tokens WHERE jti = $jti",
        params = list(jti = jti))
      n > 0L
    },
    touch_last_used = function(jti) {
      n <- DBI::dbExecute(con,
        "UPDATE mcp_tokens SET last_used_at = $t WHERE jti = $jti",
        params = list(t = iso_now(), jti = jti))
      n > 0L
    }
  )

  store <- list(
    driver = "sqlite",
    path   = path,
    users  = users,
    tokens = tokens,
    close  = function() {
      if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
      invisible(NULL)
    },
    revocation_store = function() tokens
  )
  class(store) <- "mcp_store"
  store
}

# ----- Shared helpers ---------------------------------------------------

iso_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

validate_user <- function(user) {
  if (!is.list(user)) stop("user must be a list", call. = FALSE)
  if (is.null(user$username) || !nzchar(user$username)) {
    stop("user$username is required", call. = FALSE)
  }
}

fill_user_defaults <- function(user) {
  now <- iso_now()
  list(
    id         = user$id %||% paste0("u_", new_uuid()),
    username   = as.character(user$username),
    email      = if (is.null(user$email)) NULL else as.character(user$email),
    is_admin   = isTRUE(user$is_admin),
    groups     = user$groups,
    metadata   = user$metadata,
    created_at = user$created_at %||% now,
    updated_at = user$updated_at %||% now
  )
}

validate_token <- function(token) {
  if (!is.list(token)) stop("token must be a list", call. = FALSE)
  for (k in c("jti", "user_id", "name", "expires_at")) {
    if (is.null(token[[k]]) || !nzchar(token[[k]])) {
      stop(sprintf("token$%s is required", k), call. = FALSE)
    }
  }
  if (is.null(token$scopes)) {
    stop("token$scopes is required (character vector, may be empty)",
         call. = FALSE)
  }
}

fill_token_defaults <- function(token) {
  now <- iso_now()
  list(
    jti          = as.character(token$jti),
    user_id      = as.character(token$user_id),
    name         = as.character(token$name),
    scopes       = if (length(token$scopes) == 0L) character(0L)
                   else as.character(token$scopes),
    created_at   = token$created_at %||% now,
    expires_at   = as.character(token$expires_at),
    last_used_at = token$last_used_at,
    revoked      = isTRUE(token$revoked)
  )
}

encode_json_field <- function(x) {
  if (is.null(x)) return(NA_character_)
  jsonlite::toJSON(x, auto_unbox = FALSE, null = "null")
}

decode_json_field <- function(x) {
  if (is.na(x) || is.null(x) || !nzchar(x)) return(NULL)
  safely(jsonlite::fromJSON(x, simplifyVector = FALSE), log = FALSE)
}

row_to_user <- function(row) {
  list(
    id         = row$id,
    username   = row$username,
    email      = if (is.na(row$email)) NULL else row$email,
    is_admin   = isTRUE(as.integer(row$is_admin) == 1L),
    groups     = decode_json_field(row$groups),
    metadata   = decode_json_field(row$metadata),
    created_at = row$created_at,
    updated_at = row$updated_at
  )
}

row_to_token <- function(row) {
  scopes <- if (is.na(row$scopes) || !nzchar(row$scopes)) character(0L)
            else strsplit(row$scopes, "\\s+")[[1L]]
  list(
    jti          = row$jti,
    user_id      = row$user_id,
    name         = row$name,
    scopes       = scopes,
    created_at   = row$created_at,
    expires_at   = row$expires_at,
    last_used_at = if (is.na(row$last_used_at)) NULL else row$last_used_at,
    revoked      = isTRUE(as.integer(row$revoked) == 1L)
  )
}
