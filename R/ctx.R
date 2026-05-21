# McpCtx methods exposed to handlers --------------------------------------

# The ctx object is a plain environment. The functions below are dispatched
# via `$` lookup; we define them as ordinary R functions and bind them
# late so they can reach into the captured `.session` field.

#' @export
`$.McpCtx` <- function(x, name) {
  if (name %in% c("session_id", "client_capabilities", "auth_subject",
                  "auth_scopes", "progress_token", ".session", ".msg",
                  ".task")) {
    return(get0(name, envir = x, inherits = FALSE))
  }
  if (name == "task") {
    return(get0(".task", envir = x, inherits = FALSE))
  }
  switch(name,
    send_log = function(level, message, logger = NULL, data = NULL) {
      send_log(x$.session, level, message, logger, data)
    },
    send_progress = function(progress, total = NULL, message = NULL) {
      send_progress(x$.session, x$progress_token, progress, total, message)
    },
    cancelled = function() {
      mid <- x$.msg$id
      if (is.null(mid)) return(FALSE)
      key <- as.character(mid)
      if (!exists(key, envir = x$.session$cancel, inherits = FALSE)) {
        return(FALSE)
      }
      isTRUE(get(key, envir = x$.session$cancel, inherits = FALSE)$cancelled)
    },
    request_sampling = function(messages, model_preferences = NULL,
                                system_prompt = NULL,
                                max_tokens = 1024L, timeout = 30) {
      request_sampling_impl(x$.session, messages, model_preferences,
                            system_prompt, max_tokens, timeout)
    },
    request_elicitation = function(message, requested_schema,
                                   timeout = 30) {
      request_elicitation_impl(x$.session, message, requested_schema,
                               timeout)
    },
    request_roots = function(timeout = 5) {
      if (!is.null(x$.session$roots_cache)) return(x$.session$roots_cache)
      request_roots_impl(x$.session, timeout)$roots
    },
    notify_resource_updated = function(uri) {
      sess <- x$.session
      if (exists(uri, envir = sess$subs, inherits = FALSE)) {
        sess$send(jrpc_notification(
          "notifications/resources/updated",
          list(uri = uri)))
      }
    },
    notify_list_changed = function(kind = c("tools","resources","prompts")) {
      kind <- match.arg(kind)
      method <- switch(kind,
        tools = "notifications/tools/list_changed",
        resources = "notifications/resources/list_changed",
        prompts = "notifications/prompts/list_changed")
      x$.session$send(jrpc_notification(method))
    },
    get0(name, envir = x, inherits = FALSE)
  )
}

#' @export
`[[.McpCtx` <- `$.McpCtx`

#' @export
print.McpCtx <- function(x, ...) {
  cat("<McpCtx> session=", x$session_id %||% "<none>", "\n", sep = "")
  invisible(x)
}
