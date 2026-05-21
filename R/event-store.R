# Pluggable event store interface.
#
# The Streamable HTTP transport records every server-emitted SSE event
# against a per-session ring buffer so reconnecting clients can replay
# events after a given Last-Event-ID. The in-memory implementation
# here is the default; users can plug a Redis / SQLite / file backend
# by supplying their own object with the same `$append` / `$replay`
# interface to `serve_http(event_store = my_store)`.

#' Construct an in-memory MCP event store
#'
#' Returns an object exposing `$append(stream_id, payload)` and
#' `$replay(stream_id, last_event_id, max)`. Replace it with a custom
#' implementation when you need cross-process resumability.
#'
#' @param max_per_stream Maximum events retained per stream (default
#'   1000). When exceeded the oldest events are evicted.
#' @return An environment with `$append`, `$replay`, `$drop_stream`,
#'   `$size` methods.
#' @export
#' @examples
#' es <- new_event_store(max_per_stream = 50)
#' id <- es$append("s1", "hello")
#' es$replay("s1", id)
new_event_store <- function(max_per_stream = 1000L) {
  store <- new.env(parent = emptyenv())
  store$streams <- new.env(parent = emptyenv())
  store$max_per_stream <- as.integer(max_per_stream)

  store$append <- function(stream_id, payload) {
    if (!exists(stream_id, envir = store$streams, inherits = FALSE)) {
      assign(stream_id, list(), envir = store$streams)
    }
    s <- get(stream_id, envir = store$streams, inherits = FALSE)
    id <- new_uuid()
    s[[length(s) + 1L]] <- list(id = id, payload = payload)
    if (length(s) > store$max_per_stream) {
      s <- tail(s, store$max_per_stream)
    }
    assign(stream_id, s, envir = store$streams)
    id
  }
  store$replay <- function(stream_id, last_event_id = NULL,
                           max = NA_integer_) {
    if (!exists(stream_id, envir = store$streams, inherits = FALSE)) {
      return(list())
    }
    s <- get(stream_id, envir = store$streams, inherits = FALSE)
    if (is.null(last_event_id) || identical(last_event_id, "0")) {
      out <- s
    } else {
      ids <- vapply(s, function(e) e$id, character(1L))
      idx <- which(ids == last_event_id)
      if (length(idx) == 0L) {
        return(c(list(list(id = "replay-truncated", payload = "")),
                 s))
      }
      if (idx == length(ids)) return(list())
      out <- s[(idx + 1L):length(ids)]
    }
    if (!is.na(max) && length(out) > as.integer(max)) {
      out <- head(out, as.integer(max))
    }
    out
  }
  store$drop_stream <- function(stream_id) {
    if (exists(stream_id, envir = store$streams, inherits = FALSE)) {
      rm(list = stream_id, envir = store$streams)
    }
  }
  store$size <- function(stream_id = NULL) {
    if (is.null(stream_id)) {
      length(ls(store$streams, all.names = TRUE))
    } else if (exists(stream_id, envir = store$streams,
                      inherits = FALSE)) {
      length(get(stream_id, envir = store$streams, inherits = FALSE))
    } else 0L
  }
  class(store) <- c("McpEventStore", "environment")
  store
}
