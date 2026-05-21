# Prompt registration + prompts/list + prompts/get -----------------------

#' Declare an MCP prompt argument
#'
#' @param name Argument name.
#' @param description Optional description.
#' @param required Whether the argument is required.
#' @return A list shaped for the MCP `arguments` field of a prompt
#'   descriptor.
#' @export
#' @examples
#' new_prompt_argument("city", "City name", required = TRUE)
new_prompt_argument <- function(name, description = NULL,
                                required = FALSE) {
  out <- list(name = as.character(name))
  if (!is.null(description)) out$description <- as.character(description)
  if (isTRUE(required)) out$required <- TRUE
  out
}

#' Define an MCP prompt
#'
#' Handlers receive `args` (a named list of argument values) and `ctx`,
#' and should return either a character message or a list with
#' `description` and `messages` fields. A character return is wrapped into
#' a single-message prompt automatically.
#'
#' Pass `complete = list(argName = function(value, ctx, args) character(...))`
#' to expose argument completions.
#'
#' @param name Prompt identifier.
#' @param description Human-readable description.
#' @param arguments List of [new_prompt_argument()] declarations.
#' @param complete Optional named list of completion functions.
#' @param handler A function `function(args, ctx)`.
#' @return A prompt descriptor (tagged list).
#' @export
#' @examples
#' new_prompt(
#'   name = "greet",
#'   description = "Greet a city",
#'   arguments = list(new_prompt_argument("city", required = TRUE)),
#'   handler = function(args, ctx) sprintf("Hello, %s!", args$city)
#' )
new_prompt <- function(name, description,
                       arguments = list(),
                       complete = NULL,
                       handler) {
  stopifnot(is.function(handler))
  out <- list(
    name = as.character(name),
    description = as.character(description),
    arguments = arguments,
    complete = complete,
    handler = handler
  )
  attr(out, "mcp_kind") <- "prompt"
  out
}

prompt_descriptor <- function(p) {
  list(name = p$name,
       description = p$description,
       arguments = j_list(p$arguments %||% list()))
}

handle_prompts_list <- function(server, session, params, msg = NULL) {
  ps <- ls(server$prompts, all.names = TRUE)
  list(prompts = j_list(lapply(ps, function(n) {
    prompt_descriptor(get(n, envir = server$prompts, inherits = FALSE))
  })))
}

handle_prompts_get <- function(server, session, params, msg) {
  name <- params$name
  if (is.null(name) || !exists(name, envir = server$prompts, inherits = FALSE)) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params,
                      sprintf("unknown prompt: %s", as.character(name))))
  }
  p <- get(name, envir = server$prompts, inherits = FALSE)
  args <- params$arguments %||% list()
  # Required-argument check.
  for (a in p$arguments) {
    if (isTRUE(a$required) && is.null(args[[a$name]])) {
      return(jrpc_error(msg$id, jrpc_codes$invalid_params,
                        sprintf("missing required argument: %s", a$name)))
    }
  }
  list(.prompt_call = TRUE,
       prompt = p,
       args = args,
       ctx = make_ctx(session, msg))
}

# Coerce whatever a prompt handler returned into the MCP shape:
#   { description?, messages: [ { role, content } ] }
finalize_prompt_result <- function(prompt, value) {
  if (is.character(value) && length(value) == 1L) {
    return(list(
      description = prompt$description,
      messages = list(
        list(role = "user",
             content = response_text(value))
      )
    ))
  }
  if (is.list(value) && !is.null(value$messages)) {
    out <- value
    if (is.null(out$description)) out$description <- prompt$description
    return(out)
  }
  if (is.list(value) && is.list(value[[1L]]) && !is.null(value[[1L]]$role)) {
    return(list(description = prompt$description, messages = value))
  }
  list(description = prompt$description,
       messages = list(list(role = "user",
                            content = response_text(to_json(value)))))
}
