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
    task_store = NULL,

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
      caps$tools <- list(listChanged = TRUE)
      caps$resources <- list(listChanged = TRUE, subscribe = TRUE)
      caps$prompts <- list(listChanged = TRUE)
      caps$logging <- j_empty_obj()
      caps$completions <- j_empty_obj()
      # User overrides (e.g. tasks experimental capability)
      for (k in names(self$declared_capabilities %||% list())) {
        caps[[k]] <- self$declared_capabilities[[k]]
      }
      caps
    },

    server_info = function() {
      info <- list(name = self$name, version = self$version)
      if (!is.null(self$title)) info$title <- self$title
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
                       prompts = list()) {
  srv <- McpServer$new(
    name = as.character(name),
    title = title %||% as.character(name),
    version = as.character(version),
    instructions = instructions,
    capabilities = capabilities
  )
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
