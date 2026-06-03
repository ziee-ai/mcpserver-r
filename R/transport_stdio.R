# stdio transport ---------------------------------------------------------

#' Run an MCP server over stdio
#'
#' Reads newline-delimited JSON-RPC messages from `stdin`, dispatches them
#' to the server's handlers, and writes responses to `stdout`. Tool
#' handlers run inside `mirai` daemons so long-running calls do not block
#' the main read/dispatch loop.
#'
#' @param mcp An `McpServer` returned by [new_server()].
#' @param log_path Optional path to redirect `stderr` to. When `NULL`,
#'   warnings and messages flow to the inherited stderr. The stdout stream
#'   is reserved for the MCP protocol; never write to it directly.
#' @param daemons Number of `mirai` daemons to spawn.
#' @return `NULL`, invisibly. The function blocks until `stdin` closes.
#' @export
serve_io <- function(mcp, log_path = NULL, daemons = 4L) {
  stopifnot(inherits(mcp, "McpServer"))
  if (!is.null(log_path)) {
    sink(file(log_path, open = "a"), type = "message")
    on.exit(sink(NULL, type = "message"), add = TRUE)
  }
  ensure_daemons(daemons)
  on.exit(stop_daemons(), add = TRUE)

  out_q <- new.env(parent = emptyenv())
  out_q$pending <- list()

  flush_out <- function() {
    if (length(out_q$pending) == 0L) return(invisible())
    for (env in out_q$pending) {
      cat(jrpc_encode(env), "\n", sep = "", file = stdout())
    }
    out_q$pending <- list()
    flush(stdout())
  }
  write_fn <- function(envelope) {
    out_q$pending <- c(out_q$pending, list(envelope))
    later::later(flush_out, 0)
  }

  session <- Session$new("stdio", mcp, write_fn)
  assign("stdio", session, envir = mcp$sessions)

  sock <- nanonext::read_stdin()
  cv <- nanonext::cv()
  aio <- nanonext::recv_aio(sock, mode = "string", cv = cv)

  handle_one <- function(m) {
    out <- route_message(mcp, session, m)
    if (is.null(out)) return(invisible(NULL))
    if (isTRUE(out$.async)) {
      promises::then(finalize_async(out, mcp, session),
                     onFulfilled = write_fn,
                     onRejected = function(e) {
                       write_fn(jrpc_error(out$.id %||% NULL,
                                           jrpc_codes$internal_error,
                                           conditionMessage(e)))
                     })
      return(invisible(NULL))
    }
    write_fn(out)
  }

  # The blocking wait must happen inside `later::run_now()` (a positive
  # timeout), not in `until()`. A mirai daemon result is posted onto
  # `later`'s queue from a nanonext background thread; on Windows that
  # cross-thread post is only surfaced/drained when `run_now` is performing
  # its blocking wait (the same reason the HTTP loop's `run_now(Inf)` works
  # there). A zero-timeout `run_now(0)` paired with a 50ms block in
  # `until(cv, 50L)` — which watches only the stdin cv — never delivers the
  # completion on Windows, so async tools/call results hang. So: poll stdin
  # non-blockingly with `until(cv, 0L)`, and let `run_now(0.05)` be the sole
  # blocking wait (mirrors `tasks.R`/`roots.R`'s run_now(0.05) and HTTP).
  repeat {
    if (isTRUE(nanonext::until(cv, 0L))) {
      nanonext::cv_reset(cv)
      line <- aio$data
      aio <- nanonext::recv_aio(sock, mode = "string", cv = cv)
      if (!is.null(line) && !identical(line, "")) {
        msg <- jrpc_decode(line)
        if (is.null(msg)) {
          write_fn(jrpc_error(NULL, jrpc_codes$parse_error, "parse error"))
        } else if (is.list(msg) && is.null(names(msg)) &&
                   length(msg) > 0L && is.list(msg[[1L]])) {
          for (m in msg) handle_one(m)
        } else {
          handle_one(msg)
        }
      }
    }
    # Block here so mirai-resolved promise callbacks are delivered and
    # drained — this is what writes async tools/call results to stdout.
    later::run_now(timeoutSecs = 0.05)
  }
  invisible(NULL)
}
