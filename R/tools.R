# Tool registration + tools/list + tools/call ----------------------------

#' Define an MCP tool
#'
#' A tool packages a name, JSON-Schema declaration of its arguments, an
#' optional output schema, optional MCP annotations, and an R handler
#' function that the dispatcher calls with parsed arguments. The handler
#' should return a value from one of the `response_*()` constructors.
#'
#' Handlers receive two arguments: `args` (a named list of the validated
#' input arguments) and `ctx` (a context object exposing progress,
#' logging, sampling, elicitation, and roots helpers).
#'
#' @param name Tool identifier (must be unique within a server).
#' @param title Optional human-readable display name. Distinct from
#'   `name` (which is the programmatic identifier) and from
#'   `annotations$title` (legacy shape kept for back-compat with
#'   pre-2025-06-18 clients). Modern clients prefer this top-level
#'   field per spec.
#' @param description Human-readable description.
#' @param input_schema A schema built with [schema()].
#' @param output_schema Optional schema for the tool's structured content.
#' @param annotations Optional named list of MCP annotations (e.g.
#'   `list(readOnlyHint = TRUE)`).
#' @param handler A function `function(args, ctx)`.
#' @param tasks Whether the tool participates in the experimental
#'   tasks lifecycle (default `FALSE`). When `TRUE`, the tool's
#'   `tools/list` descriptor emits `execution.taskSupport = "required"`
#'   so spec-strict clients can discover task support before calling.
#' @param meta Optional named list emitted as `_meta` on the
#'   `tools/list` descriptor. Arbitrary metadata for client-side use.
#' @param bidirectional Set `TRUE` for tools that issue server-to-client
#'   requests (`ctx$request_sampling()`, `ctx$request_elicitation()`,
#'   `ctx$request_roots()`). Such handlers execute on the transport
#'   thread instead of inside a `mirai` daemon so they can access the
#'   live pending-request table. The transport stays responsive because
#'   the handler yields via `later::run_now()` while waiting on the
#'   client.
#' @return A tool descriptor (tagged list) for use with [add_capability()].
#' @export
#' @examples
#' new_tool(
#'   name = "echo",
#'   description = "Echo back the input",
#'   input_schema = schema(list(text = property_string(required = TRUE))),
#'   handler = function(args, ctx) response_text(args$text)
#' )
new_tool <- function(name,
                     description,
                     input_schema,
                     output_schema = NULL,
                     annotations = NULL,
                     handler,
                     tasks = FALSE,
                     bidirectional = FALSE,
                     title = NULL,
                     meta = NULL) {
  stopifnot(is.character(name), length(name) == 1L)
  stopifnot(is.function(handler))
  # SEP-986: tool names must match ^[A-Za-z0-9_./-]+$ and be 1-64 chars.
  if (!grepl("^[A-Za-z0-9_./-]+$", name) || nchar(name) > 64L) {
    stop(sprintf(
      "tool name '%s' violates SEP-986 (allowed: A-Z a-z 0-9 _ . / - ; max 64 chars)",
      name))
  }
  # Validate the shape of annotation hints: per the spec these are all
  # booleans. `title` may be a string. Anything else is rejected so
  # malformed annotations don't ship to clients.
  if (!is.null(annotations)) {
    bool_hints <- c("readOnlyHint", "destructiveHint",
                    "idempotentHint", "openWorldHint")
    for (h in intersect(bool_hints, names(annotations))) {
      if (!is.logical(annotations[[h]]) ||
          length(annotations[[h]]) != 1L) {
        stop(sprintf("tool annotation '%s' must be a scalar logical",
                     h))
      }
    }
    if (!is.null(annotations$title) &&
        !(is.character(annotations$title) &&
          length(annotations$title) == 1L)) {
      stop("tool annotation 'title' must be a scalar character")
    }
  }
  if (!is.null(title) &&
      !(is.character(title) && length(title) == 1L)) {
    stop("tool 'title' must be a scalar character")
  }
  if (!is.null(meta) && (!is.list(meta) || is.null(names(meta)))) {
    stop("tool 'meta' must be a named list")
  }
  out <- list(
    name = name,
    title = title,
    description = as.character(description),
    input_schema = input_schema,
    output_schema = output_schema,
    annotations = annotations,
    handler = handler,
    tasks = isTRUE(tasks),
    bidirectional = isTRUE(bidirectional),
    meta = meta
  )
  attr(out, "mcp_kind") <- "tool"
  out
}

# Wire representation used in tools/list responses.
tool_descriptor <- function(tool) {
  desc <- list(
    name = tool$name,
    description = tool$description,
    inputSchema = tool$input_schema
  )
  if (!is.null(tool$title))         desc$title        <- tool$title
  if (!is.null(tool$output_schema)) desc$outputSchema <- tool$output_schema
  if (!is.null(tool$annotations))   desc$annotations  <- tool$annotations
  if (isTRUE(tool$tasks)) {
    # R's task-mode handlers work via either the synchronous tools/call
    # path or via SEP-1686 task-augmented calls (the task handle on
    # ctx$task is exposed in both cases). Per the spec, "optional"
    # signals to clients that they MAY use callToolStream but are not
    # required to. "required" would force the TS SDK client to refuse
    # plain client.callTool() on this tool, which doesn't match R's
    # actual behaviour.
    desc$execution <- list(taskSupport = "optional")
  }
  if (!is.null(tool$meta))          desc$`_meta`      <- tool$meta
  desc
}

handle_tools_list <- function(server, session, params, msg = NULL) {
  tools <- ls(server$tools, all.names = TRUE)
  out <- lapply(tools, function(n) {
    tool_descriptor(get(n, envir = server$tools, inherits = FALSE))
  })
  list(tools = j_list(out))
}

handle_tools_call <- function(server, session, params, msg) {
  name <- params$name
  if (is.null(name) || !exists(name, envir = server$tools, inherits = FALSE)) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params,
                      sprintf("unknown tool: %s", as.character(name))))
  }
  tool <- get(name, envir = server$tools, inherits = FALSE)
  args <- params$arguments %||% list()
  v <- validate_args(tool$input_schema, args)
  if (!v$ok) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params,
                      "invalid tool arguments",
                      data = list(errors = j_list(v$errors))))
  }
  ctx <- make_ctx(session, msg)
  # Register a cancellation entry. The in-process flag is observed by
  # bidirectional tools (which run on the transport thread); the
  # flag-file path on `ctx$.cancel_path` is observed by non-bidirectional
  # tools running inside `mirai` daemons.
  entry <- cancel_entry_open(session, msg$id)
  ctx$.cancel_path <- entry$flag_path
  # If the tool opts into the tasks lifecycle, create a task entry
  # scoped to this session and expose a `ctx$task` handle that handlers
  # can use to publish progress.
  if (isTRUE(tool$tasks)) {
    store <- ensure_task_store(server)
    task <- task_create(store, tool$name,
                        session_id = session$session_id)
    task_update_status(store, task$id, "running")
    ctx$.task <- make_task_handle(store, task$id)
  }
  # Tool execution is offloaded to a mirai daemon by the dispatcher.
  list(.tool_call = TRUE, tool = tool, args = args, ctx = ctx,
       cancel_entry = entry)
}

# Post-handler conversion of a raw user return into the MCP result shape.
finalize_tool_result <- function(tool, value) {
  norm <- normalize_tool_result(value)
  if (!is.null(tool$output_schema) && !is.null(norm$structuredContent)) {
    v <- validate_args(tool$output_schema, norm$structuredContent)
    if (!v$ok) {
      return(list(content = list(response_text(
        sprintf("structured content failed output_schema: %s",
                paste(v$errors, collapse = "; "))
      )), isError = TRUE))
    }
  }
  norm
}
