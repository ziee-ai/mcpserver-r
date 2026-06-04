# Shared helpers for stdio-transport integration tests (processx + line-
# delimited JSON-RPC). Used by test-stdio-async.R and test-stdio-extpool.R.

# Spawn an mcpserver stdio server from a runner script given as character
# lines. Returns list(process, script); the caller is responsible for
# withr::defer({ res$process$kill(); unlink(res$script) }).
stdio_spawn <- function(script_lines) {
  script <- tempfile(fileext = ".R")
  writeLines(script_lines, script)
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(), collapse = .Platform$path.sep)
  p <- processx::process$new(
    "Rscript", c(script),
    stdin = "|", stdout = "|", stderr = "|",
    env = child_env)
  list(process = p, script = script)
}

stdio_send <- function(p, msg) {
  p$write_input(paste0(jsonlite::toJSON(msg, auto_unbox = TRUE), "\n"))
}

# Persistent line buffer so multiple replies flushed together aren't lost.
stdio_reader <- function(p) {
  buf <- new.env(parent = emptyenv())
  buf$lines <- character(0L)
  buf$partial <- ""
  buf$p <- p
  buf
}

stdio_readline <- function(buf, timeout_ms = 30000) {
  if (length(buf$lines) > 0L) {
    out <- buf$lines[[1L]]; buf$lines <- buf$lines[-1L]; return(out)
  }
  t0 <- Sys.time()
  while (difftime(Sys.time(), t0, units = "secs") < timeout_ms / 1000) {
    buf$p$poll_io(200)
    chunk <- buf$p$read_output()
    if (nchar(chunk) > 0L) {
      buf$partial <- paste0(buf$partial, chunk)
      if (grepl("\n", buf$partial, fixed = TRUE)) {
        parts <- strsplit(buf$partial, "\n", fixed = TRUE)[[1L]]
        if (endsWith(buf$partial, "\n")) {
          buf$lines <- c(buf$lines, parts); buf$partial <- ""
        } else {
          buf$lines <- c(buf$lines, parts[-length(parts)])
          buf$partial <- parts[[length(parts)]]
        }
        buf$lines <- buf$lines[nzchar(buf$lines)]
        if (length(buf$lines) > 0L) {
          out <- buf$lines[[1L]]; buf$lines <- buf$lines[-1L]; return(out)
        }
      }
    }
    if (!buf$p$is_alive()) break
  }
  NA_character_
}

# Read responses until every id in `ids` is seen (they can arrive out of
# order / interleaved). Returns a list keyed by id, or NULL on timeout.
stdio_collect_by_id <- function(buf, ids, timeout_ms = 30000) {
  got <- list()
  t0 <- Sys.time()
  while (length(got) < length(ids) &&
         difftime(Sys.time(), t0, units = "secs") < timeout_ms / 1000) {
    line <- stdio_readline(buf, timeout_ms = timeout_ms)
    if (is.na(line)) break
    parsed <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
                       error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$id)) next
    key <- as.character(parsed$id)
    if (key %in% as.character(ids)) got[[key]] <- parsed
  }
  if (length(got) < length(ids)) return(NULL)
  got
}

# Drive the initialize + notifications/initialized handshake; return the
# reader buffer ready for subsequent requests.
stdio_init <- function(srv, timeout_ms = 30000) {
  buf <- stdio_reader(srv$process)
  stdio_send(srv$process, list(jsonrpc = "2.0", id = 1, method = "initialize",
                               params = list(protocolVersion = "2025-06-18",
                                             capabilities = list())))
  init_line <- stdio_readline(buf, timeout_ms = timeout_ms)
  testthat::expect_false(
    is.na(init_line),
    info = paste("stderr:",
                 paste(srv$process$read_error_lines(), collapse = " | ")))
  stdio_send(srv$process, list(jsonrpc = "2.0",
                               method = "notifications/initialized"))
  buf
}
