# Resource and resource-template registration + read --------------------

#' Define a static MCP resource
#'
#' Use this for resources with a fixed URI. For URIs that include a
#' parameter (e.g. `demo://x/{id}`), use [new_resource_template()] instead.
#'
#' Handlers receive `params` (a named list including `uri`) and `ctx` and
#' should return a list with either a `text` or `blob` field plus an
#' optional `mimeType`. A character return value is wrapped automatically
#' into `list(text = value)`.
#'
#' @param name Short identifier (must be unique).
#' @param description Human-readable description.
#' @param uri Resource URI.
#' @param mime_type Optional MIME type.
#' @param annotations Optional MCP annotations list.
#' @param handler A function `function(params, ctx)`.
#' @param title Optional human-readable display name, distinct from
#'   the programmatic `name`.
#' @return A resource descriptor (tagged list).
#' @export
#' @examples
#' new_resource("greeting", "A greeting", "demo://greeting",
#'              mime_type = "text/plain",
#'              handler = function(params, ctx) "hello")
new_resource <- function(name, description, uri,
                         mime_type = NULL,
                         annotations = NULL,
                         handler,
                         title = NULL) {
  stopifnot(is.function(handler))
  if (!is.null(title) &&
      !(is.character(title) && length(title) == 1L)) {
    stop("resource 'title' must be a scalar character")
  }
  out <- list(
    name = as.character(name),
    title = title,
    description = as.character(description),
    uri = as.character(uri),
    mime_type = mime_type,
    annotations = annotations,
    handler = handler
  )
  attr(out, "mcp_kind") <- "resource"
  out
}

#' Define a templated MCP resource
#'
#' Templates use level-1 RFC 6570 syntax (`{var}` placeholders). The
#' supplied `handler` is called with `params$variables` set to a named list
#' of the matched values.
#'
#' Pass `complete = list(varName = function(value, ctx) character(...))` to
#' surface argument completions for the corresponding template variable.
#'
#' @param name Short identifier.
#' @param description Human-readable description.
#' @param uri_template URI template, e.g. `"demo://item/{id}"`.
#' @param mime_type Optional MIME type.
#' @param annotations Optional MCP annotations list.
#' @param handler A function `function(params, ctx)`.
#' @param complete Optional named list of completion functions keyed by
#'   variable name.
#' @param title Optional human-readable display name, distinct from
#'   the programmatic `name`.
#' @return A resource template descriptor (tagged list).
#' @export
#' @examples
#' new_resource_template("item", "Items", "demo://item/{id}",
#'                       handler = function(params, ctx) "data")
new_resource_template <- function(name, description, uri_template,
                                  mime_type = NULL,
                                  annotations = NULL,
                                  handler,
                                  complete = NULL,
                                  title = NULL) {
  stopifnot(is.function(handler))
  if (!is.null(title) &&
      !(is.character(title) && length(title) == 1L)) {
    stop("resource_template 'title' must be a scalar character")
  }
  out <- list(
    name = as.character(name),
    title = title,
    description = as.character(description),
    uri_template = as.character(uri_template),
    mime_type = mime_type,
    annotations = annotations,
    handler = handler,
    complete = complete
  )
  attr(out, "mcp_kind") <- "resource_template"
  out
}

resource_descriptor <- function(r) {
  out <- list(name = r$name, description = r$description, uri = r$uri)
  if (!is.null(r$title))       out$title       <- r$title
  if (!is.null(r$mime_type))   out$mimeType    <- r$mime_type
  if (!is.null(r$annotations)) out$annotations <- r$annotations
  out
}

resource_template_descriptor <- function(t) {
  out <- list(name = t$name, description = t$description,
              uriTemplate = t$uri_template)
  if (!is.null(t$title))       out$title       <- t$title
  if (!is.null(t$mime_type))   out$mimeType    <- t$mime_type
  if (!is.null(t$annotations)) out$annotations <- t$annotations
  out
}

handle_resources_list <- function(server, session, params, msg = NULL) {
  rs <- ls(server$resources, all.names = TRUE)
  list(resources = j_list(lapply(rs, function(n) {
    resource_descriptor(get(n, envir = server$resources, inherits = FALSE))
  })))
}

handle_resources_templates_list <- function(server, session, params, msg = NULL) {
  ts <- ls(server$resource_templates, all.names = TRUE)
  list(resourceTemplates = j_list(lapply(ts, function(n) {
    resource_template_descriptor(
      get(n, envir = server$resource_templates, inherits = FALSE))
  })))
}

# Build the canonical resources/read result envelope for a handler return.
finalize_resource_read <- function(uri, mime_type, value) {
  contents <- if (is.character(value) && length(value) == 1L) {
    list(uri = uri, text = value)
  } else if (is.raw(value)) {
    list(uri = uri, blob = jsonlite::base64_enc(value))
  } else if (is.list(value)) {
    base <- list(uri = uri)
    if (!is.null(value$text)) base$text <- as.character(value$text)
    if (!is.null(value$blob)) base$blob <- encode_blob(value$blob)
    if (!is.null(value$mimeType)) base$mimeType <- value$mimeType
    base
  } else {
    list(uri = uri, text = to_json(value))
  }
  if (is.null(contents$mimeType) && !is.null(mime_type)) {
    contents$mimeType <- mime_type
  }
  list(contents = list(contents))
}

handle_resources_read <- function(server, session, params, msg) {
  uri <- params$uri
  if (is.null(uri)) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params, "missing uri"))
  }
  # Session-scoped resources take precedence (e.g. per-call gzip
  # archives surfaced by tools).
  sr <- session_resource_get(session, uri)
  if (!is.null(sr)) {
    return(list(.resource_call = TRUE,
                resource = sr,
                params = list(uri = uri),
                mime_type = sr$mime_type,
                ctx = make_ctx(session, msg)))
  }
  # Exact match against the server-wide registry.
  if (exists(uri, envir = server$resources, inherits = FALSE)) {
    r <- get(uri, envir = server$resources, inherits = FALSE)
    return(list(.resource_call = TRUE,
                resource = r,
                params = list(uri = uri),
                mime_type = r$mime_type,
                ctx = make_ctx(session, msg)))
  }
  # Template fallback.
  for (n in ls(server$resource_templates, all.names = TRUE)) {
    t <- get(n, envir = server$resource_templates, inherits = FALSE)
    vars <- uri_template_match(t$uri_template, uri)
    if (!is.null(vars)) {
      return(list(.resource_call = TRUE,
                  resource = t,
                  params = list(uri = uri, variables = vars),
                  mime_type = t$mime_type,
                  ctx = make_ctx(session, msg)))
    }
  }
  jrpc_error(msg$id, jrpc_codes$invalid_params,
             sprintf("resource not found: %s", uri))
}

# Subscriptions ----------------------------------------------------------

handle_resources_subscribe <- function(server, session, params, msg) {
  uri <- params$uri
  if (is.null(uri)) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params, "missing uri"))
  }
  assign(uri, TRUE, envir = session$subs)
  list()
}

handle_resources_unsubscribe <- function(server, session, params, msg) {
  uri <- params$uri
  if (is.null(uri)) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params, "missing uri"))
  }
  if (exists(uri, envir = session$subs, inherits = FALSE)) {
    rm(list = uri, envir = session$subs)
  }
  list()
}
