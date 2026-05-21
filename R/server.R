# McpServer + new_server() + add_capability() -----------------------------

McpServer <- R6::R6Class(
  "McpServer",
  public = list(
    name = NULL,
    title = NULL,
    version = NULL,
    instructions = NULL,
    declared_capabilities = NULL,

    tools = NULL,
    resources = NULL,
    resource_templates = NULL,
    prompts = NULL,
    completions = NULL,

    sessions = NULL,
    on_initialized_hooks = NULL,
    on_close_hooks = NULL,
    on_error_hooks = NULL,
    task_store = NULL,
    # When TRUE, attempts to register handlers for methods whose
    # capability hasn't been declared raise. Mirrors TS
    # Server.enforceStrictCapabilities.
    strict_capabilities = FALSE,
    # Optional pluggable schema validator (function returned by
    # new_validator()). When NULL, falls back to validate_args().
    schema_validator = NULL,
    # Implementation extras (2025-11-25 BaseMetadata fields).
    icons = NULL,
    website_url = NULL,
    description = NULL,
    # Free-form extension state. Sample servers (e.g. the bundled
    # everything-demo) stash their timer/toggle state here so the
    # core package keeps no reference to it.
    extension_state = NULL,
    # User-registered custom request / notification handlers.
    custom_request_handlers = NULL,
    custom_notification_handlers = NULL,
    fallback_request_handler = NULL,
    fallback_notification_handler = NULL,

    initialize = function(name, title, version, instructions,
                          capabilities) {
      self$name <- name
      self$title <- title
      self$version <- version
      self$instructions <- instructions
      self$declared_capabilities <- capabilities
      self$tools <- new.env(parent = emptyenv())
      self$resources <- new.env(parent = emptyenv())
      self$resource_templates <- new.env(parent = emptyenv())
      self$prompts <- new.env(parent = emptyenv())
      self$completions <- new.env(parent = emptyenv())
      self$sessions <- new.env(parent = emptyenv())
      self$on_initialized_hooks <- list()
      self$on_close_hooks <- list()
      self$on_error_hooks <- list()
    },

    on_close = function(fn) {
      stopifnot(is.function(fn))
      self$on_close_hooks <- c(self$on_close_hooks, list(fn))
      invisible(self)
    },
    on_error = function(fn) {
      stopifnot(is.function(fn))
      self$on_error_hooks <- c(self$on_error_hooks, list(fn))
      invisible(self)
    },
    fire_close = function(session_id = NULL) {
      for (fn in self$on_close_hooks) {
        safely(fn(self, session_id), log = TRUE)
      }
    },
    fire_error = function(err, session_id = NULL) {
      for (fn in self$on_error_hooks) {
        safely(fn(err, self, session_id), log = TRUE)
      }
    },

    send_ping = function(session, timeout = 5) {
      # Best-effort outbound ping using the same blocking call helper.
      tryCatch(call_client_blocking(session, "ping", NULL, timeout),
               error = function(e) NULL)
    },

    register_tool = function(tool) {
      assign(tool$name, tool, envir = self$tools)
      invisible(self)
    },
    register_resource = function(res) {
      assign(res$uri, res, envir = self$resources)
      invisible(self)
    },
    register_resource_template = function(tpl) {
      assign(tpl$name, tpl, envir = self$resource_templates)
      invisible(self)
    },
    register_prompt = function(p) {
      assign(p$name, p, envir = self$prompts)
      invisible(self)
    },

    on_initialized = function(fn) {
      stopifnot(is.function(fn))
      self$on_initialized_hooks <-
        c(self$on_initialized_hooks, list(fn))
      invisible(self)
    },

    capabilities = function() {
      caps <- list()
      # Only advertise the kinds we actually have something registered
      # for. Over-advertising lets clients call methods that then
      # return MethodNotFound — see TS server/mcp.js for the same rule.
      if (length(ls(self$tools, all.names = TRUE)) > 0L) {
        caps$tools <- list(listChanged = TRUE)
      }
      if (length(ls(self$resources, all.names = TRUE)) > 0L ||
          length(ls(self$resource_templates, all.names = TRUE)) > 0L) {
        caps$resources <- list(listChanged = TRUE, subscribe = TRUE)
      }
      if (length(ls(self$prompts, all.names = TRUE)) > 0L) {
        caps$prompts <- list(listChanged = TRUE)
        # Completions are only useful when a prompt declares them.
        for (n in ls(self$prompts, all.names = TRUE)) {
          p <- get(n, envir = self$prompts, inherits = FALSE)
          if (!is.null(p$complete) && length(p$complete) > 0L) {
            caps$completions <- j_empty_obj()
            break
          }
        }
      }
      # Logging is always available — send_log gates by session level.
      caps$logging <- j_empty_obj()
      # User-supplied capability declarations win.
      for (k in names(self$declared_capabilities %||% list())) {
        caps[[k]] <- self$declared_capabilities[[k]]
      }
      caps
    },

    server_info = function() {
      info <- list(name = self$name, version = self$version)
      if (!is.null(self$title)) info$title <- self$title
      if (!is.null(self$description)) info$description <- self$description
      if (!is.null(self$website_url)) info$websiteUrl <- self$website_url
      if (!is.null(self$icons)) info$icons <- self$icons
      info
    }
  )
)

#' Create an MCP server
#'
#' Bundles a name, version, and capability declarations together with the
#' registries of tools, resources, resource templates, and prompts. The
#' returned object is mutable: register additional capabilities with
#' [add_capability()] and start it with [serve_io()] or [serve_http()].
#'
#' @param name Short server identifier.
#' @param title Optional human-friendly title (defaults to `name`).
#' @param version Server version string.
#' @param instructions Optional text returned to the client during the
#'   `initialize` handshake.
#' @param capabilities Optional named list of extra capability declarations
#'   (e.g. `list(experimental = list(tasks = TRUE))`).
#' @param tools,resources,resource_templates,prompts Optional lists of
#'   capabilities to register up front; equivalent to calling
#'   [add_capability()] on each.
#' @return An `McpServer` object.
#' @export
#' @examples
#' srv <- new_server("demo", version = "0.1.0")
#' srv
new_server <- function(name,
                       title = NULL,
                       version = "0.1.0",
                       instructions = NULL,
                       capabilities = NULL,
                       tools = list(),
                       resources = list(),
                       resource_templates = list(),
                       prompts = list(),
                       description = NULL,
                       website_url = NULL,
                       icons = NULL,
                       strict_capabilities = FALSE,
                       schema_validator = NULL) {
  srv <- McpServer$new(
    name = as.character(name),
    title = title %||% as.character(name),
    version = as.character(version),
    instructions = instructions,
    capabilities = capabilities
  )
  srv$description <- description
  srv$website_url <- website_url
  srv$icons <- icons
  srv$strict_capabilities <- isTRUE(strict_capabilities)
  srv$schema_validator <- schema_validator
  for (t in tools) add_capability(srv, t)
  for (r in resources) add_capability(srv, r)
  for (t in resource_templates) add_capability(srv, t)
  for (p in prompts) add_capability(srv, p)
  srv
}

#' Register a tool, resource, resource template, or prompt on a server
#'
#' Returns the server invisibly so calls can be chained.
#'
#' @param mcp An `McpServer` returned by [new_server()].
#' @param capability A capability descriptor built with [new_tool()],
#'   [new_resource()], [new_resource_template()], or [new_prompt()].
#' @return `mcp`, invisibly.
#' @export
#' @examples
#' srv <- new_server("demo")
#' tool <- new_tool("echo", "Echo text",
#'                  input_schema = schema(list(text = property_string(required = TRUE))),
#'                  handler = function(args, ctx) response_text(args$text))
#' add_capability(srv, tool)
add_capability <- function(mcp, capability) {
  stopifnot(inherits(mcp, "McpServer"))
  kind <- attr(capability, "mcp_kind") %||% ""
  switch(kind,
    "tool"              = mcp$register_tool(capability),
    "resource"          = mcp$register_resource(capability),
    "resource_template" = mcp$register_resource_template(capability),
    "prompt"            = mcp$register_prompt(capability),
    stop("add_capability(): unrecognised capability of kind '", kind, "'"))
  invisible(mcp)
}

#' Register a hook to run after the `initialize` handshake completes
#'
#' Useful for conditionally registering tools that depend on client
#' capabilities (e.g. only expose sampling-using tools when the client
#' declared `sampling`).
#'
#' @param mcp An `McpServer`.
#' @param fn A function `function(mcp, session)`.
#' @return `mcp`, invisibly.
#' @export
#' @examples
#' srv <- new_server("demo")
#' on_initialized(srv, function(mcp, session) {
#'   # e.g. add_capability(mcp, ...) if session$client_capabilities$sampling
#' })
on_initialized <- function(mcp, fn) {
  mcp$on_initialized(fn)
  invisible(mcp)
}

#' Register a server-level close callback
#'
#' Fires when a transport closes a session (HTTP `DELETE` or stdio EOF)
#' or when the server itself shuts down. The callback receives the
#' server and the closing `session_id` (or `NULL` for global shutdown).
#'
#' @param mcp An `McpServer`.
#' @param fn A function `function(mcp, session_id)`.
#' @return `mcp`, invisibly.
#' @export
on_close <- function(mcp, fn) {
  mcp$on_close(fn)
  invisible(mcp)
}

#' Register a server-level error callback
#'
#' Fires for uncaught exceptions in request and notification handlers.
#' The callback receives the condition object, the server, and the
#' originating `session_id` (or `NULL` if unknown).
#'
#' @param mcp An `McpServer`.
#' @param fn A function `function(err, mcp, session_id)`.
#' @return `mcp`, invisibly.
#' @export
on_error <- function(mcp, fn) {
  mcp$on_error(fn)
  invisible(mcp)
}

#' Merge additional capability declarations into a server
#'
#' Mirrors the TS SDK's `Server.registerCapabilities`. Must be called
#' before [serve_io()] / [serve_http()].
#'
#' @param mcp An `McpServer`.
#' @param capabilities Named list to merge into the server's declared
#'   capabilities.
#' @return `mcp`, invisibly.
#' @export
register_capabilities <- function(mcp, capabilities) {
  stopifnot(inherits(mcp, "McpServer"))
  stopifnot(is.list(capabilities))
  cur <- mcp$declared_capabilities %||% list()
  for (k in names(capabilities)) {
    cur[[k]] <- capabilities[[k]]
  }
  mcp$declared_capabilities <- cur
  invisible(mcp)
}

#' Send an outbound `ping` to a specific session
#'
#' @param mcp An `McpServer`.
#' @param session_id Target session id.
#' @param timeout Seconds to wait for the client's reply.
#' @return The client's reply or `NULL` on timeout.
#' @export
send_ping <- function(mcp, session_id, timeout = 5) {
  stopifnot(inherits(mcp, "McpServer"))
  if (!exists(session_id, envir = mcp$sessions, inherits = FALSE)) {
    return(NULL)
  }
  sess <- get(session_id, envir = mcp$sessions, inherits = FALSE)
  mcp$send_ping(sess, timeout)
}

#' @export
print.McpServer <- function(x, ...) {
  cat("<McpServer> ", x$name, " v", x$version, "\n", sep = "")
  cat("  tools     : ", length(ls(x$tools, all.names = TRUE)), "\n", sep = "")
  cat("  resources : ", length(ls(x$resources, all.names = TRUE)), "\n", sep = "")
  cat("  templates : ", length(ls(x$resource_templates, all.names = TRUE)),
      "\n", sep = "")
  cat("  prompts   : ", length(ls(x$prompts, all.names = TRUE)), "\n", sep = "")
  invisible(x)
}
