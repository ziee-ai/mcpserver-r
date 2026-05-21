# Tool / resource / prompt registry lifecycle.
#
# update_*() and remove_*() mirror the TS SDK's `RegisteredTool.update()`
# and `.remove()` semantics. They mutate the McpServer's registry env
# and schedule a debounced `*list_changed` notification.

#' Update an existing tool's descriptor
#'
#' Replaces the named tool with a new descriptor. Fires a debounced
#' `notifications/tools/list_changed` to every active session. Errors
#' if no tool with that name is registered.
#'
#' @param mcp An `McpServer`.
#' @param name Tool name.
#' @param ... Fields to override on the existing descriptor (e.g.
#'   `description = ...`, `input_schema = ...`, `handler = ...`).
#' @return `mcp`, invisibly.
#' @export
update_tool <- function(mcp, name, ...) {
  stopifnot(inherits(mcp, "McpServer"))
  if (!exists(name, envir = mcp$tools, inherits = FALSE)) {
    stop(sprintf("update_tool(): no tool named '%s'", name))
  }
  cur <- get(name, envir = mcp$tools, inherits = FALSE)
  changes <- list(...)
  for (k in names(changes)) cur[[k]] <- changes[[k]]
  attr(cur, "mcp_kind") <- "tool"
  assign(name, cur, envir = mcp$tools)
  schedule_list_changed(mcp, "tools")
  invisible(mcp)
}

#' Remove a tool by name
#'
#' Fires a debounced `notifications/tools/list_changed` if the tool
#' existed.
#'
#' @param mcp An `McpServer`.
#' @param name Tool name.
#' @return `mcp`, invisibly.
#' @export
remove_tool <- function(mcp, name) {
  stopifnot(inherits(mcp, "McpServer"))
  if (exists(name, envir = mcp$tools, inherits = FALSE)) {
    rm(list = name, envir = mcp$tools)
    schedule_list_changed(mcp, "tools")
  }
  invisible(mcp)
}

#' Update an existing resource's descriptor
#'
#' @param mcp An `McpServer`.
#' @param uri Resource URI.
#' @param ... Fields to override.
#' @return `mcp`, invisibly.
#' @export
update_resource <- function(mcp, uri, ...) {
  stopifnot(inherits(mcp, "McpServer"))
  if (!exists(uri, envir = mcp$resources, inherits = FALSE)) {
    stop(sprintf("update_resource(): no resource at '%s'", uri))
  }
  cur <- get(uri, envir = mcp$resources, inherits = FALSE)
  changes <- list(...)
  for (k in names(changes)) cur[[k]] <- changes[[k]]
  attr(cur, "mcp_kind") <- "resource"
  assign(uri, cur, envir = mcp$resources)
  schedule_list_changed(mcp, "resources")
  invisible(mcp)
}

#' Remove a resource by URI
#'
#' @param mcp An `McpServer`.
#' @param uri Resource URI.
#' @return `mcp`, invisibly.
#' @export
remove_resource <- function(mcp, uri) {
  stopifnot(inherits(mcp, "McpServer"))
  if (exists(uri, envir = mcp$resources, inherits = FALSE)) {
    rm(list = uri, envir = mcp$resources)
    schedule_list_changed(mcp, "resources")
  }
  invisible(mcp)
}

#' Update or remove a resource template
#'
#' @param mcp An `McpServer`.
#' @param name Template name.
#' @param ... Fields to override.
#' @return `mcp`, invisibly.
#' @export
update_resource_template <- function(mcp, name, ...) {
  stopifnot(inherits(mcp, "McpServer"))
  if (!exists(name, envir = mcp$resource_templates, inherits = FALSE)) {
    stop(sprintf(
      "update_resource_template(): no template named '%s'", name))
  }
  cur <- get(name, envir = mcp$resource_templates, inherits = FALSE)
  changes <- list(...)
  for (k in names(changes)) cur[[k]] <- changes[[k]]
  attr(cur, "mcp_kind") <- "resource_template"
  assign(name, cur, envir = mcp$resource_templates)
  schedule_list_changed(mcp, "resources")
  invisible(mcp)
}

#' @rdname update_resource_template
#' @export
remove_resource_template <- function(mcp, name) {
  stopifnot(inherits(mcp, "McpServer"))
  if (exists(name, envir = mcp$resource_templates, inherits = FALSE)) {
    rm(list = name, envir = mcp$resource_templates)
    schedule_list_changed(mcp, "resources")
  }
  invisible(mcp)
}

#' Update or remove a prompt
#'
#' @param mcp An `McpServer`.
#' @param name Prompt name.
#' @param ... Fields to override.
#' @return `mcp`, invisibly.
#' @export
update_prompt <- function(mcp, name, ...) {
  stopifnot(inherits(mcp, "McpServer"))
  if (!exists(name, envir = mcp$prompts, inherits = FALSE)) {
    stop(sprintf("update_prompt(): no prompt named '%s'", name))
  }
  cur <- get(name, envir = mcp$prompts, inherits = FALSE)
  changes <- list(...)
  for (k in names(changes)) cur[[k]] <- changes[[k]]
  attr(cur, "mcp_kind") <- "prompt"
  assign(name, cur, envir = mcp$prompts)
  schedule_list_changed(mcp, "prompts")
  invisible(mcp)
}

#' @rdname update_prompt
#' @export
remove_prompt <- function(mcp, name) {
  stopifnot(inherits(mcp, "McpServer"))
  if (exists(name, envir = mcp$prompts, inherits = FALSE)) {
    rm(list = name, envir = mcp$prompts)
    schedule_list_changed(mcp, "prompts")
  }
  invisible(mcp)
}

# Debounce window (ms) shared by all list_changed methods.
.LIST_CHANGED_DEBOUNCE_MS <- 50

# Schedule a debounced list_changed notification for `kind` in
# {"tools","resources","prompts"}. Repeated calls within the debounce
# window coalesce into one notification per kind.
schedule_list_changed <- function(mcp, kind) {
  state <- mcp$extension_state
  if (is.null(state)) {
    mcp$extension_state <- new.env(parent = emptyenv())
    state <- mcp$extension_state
  }
  if (is.null(state$.debouncer)) {
    state$.debouncer <- new.env(parent = emptyenv())
  }
  bus <- state$.debouncer
  key <- paste0("pending_", kind)
  if (isTRUE(bus[[key]])) return(invisible(NULL))
  bus[[key]] <- TRUE
  later::later(function() {
    bus[[key]] <- FALSE
    switch(kind,
      "tools" = notify_tool_list_changed(mcp),
      "resources" = notify_resource_list_changed(mcp),
      "prompts" = notify_prompt_list_changed(mcp))
  }, delay = .LIST_CHANGED_DEBOUNCE_MS / 1000)
  invisible(NULL)
}
