# Session-scoped resources -----------------------------------------------
#
# Tools may register a resource at call time that exists only for the
# lifetime of the session (e.g. `gzip-file-as-resource` materialises a
# per-call archive and returns a link to it). The registry is a plain
# env keyed by URI; entries are torn down by `Session$close()`. The
# `handle_resources_read` dispatcher checks this env before falling
# back to the server-wide static + template registries.

session_resource_register <- function(session, uri,
                                      handler,
                                      mime_type = NULL,
                                      name = NULL,
                                      description = NULL,
                                      title = NULL) {
  stopifnot(!is.null(session$session_resources))
  stopifnot(is.function(handler))
  entry <- list(
    name = name %||% uri,
    title = title,
    description = description %||% "Session-scoped resource",
    uri = uri,
    mime_type = mime_type,
    handler = handler
  )
  attr(entry, "mcp_kind") <- "resource"
  assign(uri, entry, envir = session$session_resources)
  invisible(entry)
}

session_resource_get <- function(session, uri) {
  if (is.null(session$session_resources)) return(NULL)
  if (!exists(uri, envir = session$session_resources,
              inherits = FALSE)) {
    return(NULL)
  }
  get(uri, envir = session$session_resources, inherits = FALSE)
}

session_resource_list <- function(session) {
  if (is.null(session$session_resources)) return(list())
  uris <- ls(session$session_resources, all.names = TRUE)
  lapply(uris, function(u)
    get(u, envir = session$session_resources, inherits = FALSE))
}
