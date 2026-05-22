# McpCtx methods exposed to handlers --------------------------------------

# The ctx object is a plain environment. The functions below are dispatched
# via `$` lookup; we define them as ordinary R functions and bind them
# late so they can reach into the captured `.session` field.

#' @export
`$.McpCtx` <- function(x, name) {
  if (name %in% c("session_id", "client_capabilities", "auth_subject",
                  "auth_scopes", "user_id", "user_name", "is_admin",
                  "progress_token", "request_info",
                  "msg_meta", ".session", ".msg", ".task")) {
    return(get0(name, envir = x, inherits = FALSE))
  }
  if (name == "task") {
    return(get0(".task", envir = x, inherits = FALSE))
  }
  switch(name,
    send_log = function(level, message, logger = NULL, data = NULL) {
      # Per SEP-1686: notifications must not flow after a task reaches
      # a terminal status (completed/failed/cancelled). Silently drop.
      task_handle <- get0(".task", envir = x, inherits = FALSE)
      if (!is.null(task_handle) &&
          task_handle$status() %in% c("completed", "failed", "cancelled")) {
        return(invisible(NULL))
      }
      envelope <- send_log(x$.session, level, message, logger, data)
      if (!is.null(envelope) && !is.null(task_handle)) {
        # Mirror the notification into the task's message queue so a
        # later tasks/result call can return queued messages alongside
        # the final result.
        safely(task_handle$append_message(envelope), log = FALSE)
      }
      invisible(envelope)
    },
    send_progress = function(progress, total = NULL, message = NULL) {
      task_handle <- get0(".task", envir = x, inherits = FALSE)
      if (!is.null(task_handle) &&
          task_handle$status() %in% c("completed", "failed", "cancelled")) {
        return(invisible(NULL))
      }
      envelope <- send_progress(x$.session, x$progress_token, progress,
                                total, message,
                                related_request_id = x$.msg$id)
      if (!is.null(envelope) && !is.null(task_handle)) {
        safely(task_handle$append_message(envelope), log = FALSE)
      }
      invisible(envelope)
    },
    cancelled = function() {
      # In-process fast path: bidirectional tools run on the transport
      # thread and share `session$cancel` with the notification handler.
      mid <- x$.msg$id
      if (!is.null(mid)) {
        key <- as.character(mid)
        if (exists(key, envir = x$.session$cancel, inherits = FALSE) &&
            isTRUE(get(key, envir = x$.session$cancel,
                       inherits = FALSE)$cancelled)) {
          return(TRUE)
        }
      }
      # Cross-process path: inside a mirai daemon the env above is a
      # stale snapshot from serialisation. The flag-file is the source
      # of truth — the transport thread touches it on cancel.
      path <- get0(".cancel_path", envir = x, inherits = FALSE)
      if (!is.null(path) && file.exists(path)) return(TRUE)
      FALSE
    },
    on_cancel = function(fn) {
      stopifnot(is.function(fn))
      mid <- x$.msg$id
      if (is.null(mid)) {
        stop("ctx$on_cancel() requires a request id on the message")
      }
      key <- as.character(mid)
      if (!exists(key, envir = x$.session$cancel, inherits = FALSE)) {
        stop("ctx$on_cancel() called outside a cancellable request")
      }
      entry <- get(key, envir = x$.session$cancel, inherits = FALSE)
      entry$on_cancel_fns <- c(entry$on_cancel_fns, list(fn))
      invisible(NULL)
    },
    request_sampling = function(messages, model_preferences = NULL,
                                system_prompt = NULL,
                                max_tokens = 1024L, timeout = 30,
                                tools = NULL, tool_choice = NULL) {
      request_sampling_impl(x$.session, messages, model_preferences,
                            system_prompt, max_tokens, timeout,
                            tools = tools, tool_choice = tool_choice)
    },
    request_sampling_async = function(messages, model_preferences = NULL,
                                      system_prompt = NULL,
                                      max_tokens = 1024L,
                                      ttl = 30,
                                      poll_interval = 0.25,
                                      total_timeout = 60) {
      caps <- x$.session$client_capabilities %||% list()
      if (is.null(caps$sampling)) {
        stop("client did not declare sampling capability")
      }
      call_client_task(x$.session, "sampling/createMessage",
        drop_nulls(list(messages = messages,
                        modelPreferences = model_preferences,
                        systemPrompt = system_prompt,
                        maxTokens = max_tokens)),
        ttl = ttl, poll_interval = poll_interval,
        total_timeout = total_timeout)
    },
    request_elicitation = function(message, requested_schema,
                                   timeout = 30) {
      request_elicitation_impl(x$.session, message, requested_schema,
                               timeout)
    },
    request_elicitation_async = function(message, requested_schema,
                                         ttl = 30,
                                         poll_interval = 0.25,
                                         total_timeout = 60) {
      caps <- x$.session$client_capabilities %||% list()
      if (is.null(caps$elicitation)) {
        stop("client did not declare elicitation capability")
      }
      call_client_task(x$.session, "elicitation/create",
        list(message = message,
             requestedSchema = requested_schema),
        ttl = ttl, poll_interval = poll_interval,
        total_timeout = total_timeout)
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
