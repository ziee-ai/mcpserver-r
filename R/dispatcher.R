# Dispatcher --------------------------------------------------------------

# Synchronous methods that run on the transport thread (no mirai needed).
sync_handlers <- function() list(
  "initialize"                  = handle_initialize,
  "ping"                         = handle_ping,
  "tools/list"                  = handle_tools_list,
  "resources/list"              = handle_resources_list,
  "resources/templates/list"    = handle_resources_templates_list,
  "resources/subscribe"         = function(server, session, params, msg) {
    handle_resources_subscribe(server, session, params, msg)
  },
  "resources/unsubscribe"       = function(server, session, params, msg) {
    handle_resources_unsubscribe(server, session, params, msg)
  },
  "prompts/list"                = handle_prompts_list,
  "logging/setLevel"            = function(server, session, params, msg) {
    handle_logging_set_level(server, session, params, msg)
  },
  "tasks/list"                  = handle_tasks_list,
  "tasks/cancel"                = function(server, session, params, msg) {
    handle_tasks_cancel(server, session, params, msg)
  }
)

# Async methods (each returns a sentinel list that the dispatcher hands off
# to a mirai daemon).
async_handlers <- function() list(
  "tools/call"        = handle_tools_call,
  "resources/read"    = handle_resources_read,
  "prompts/get"       = handle_prompts_get,
  "completion/complete" = handle_completion
)

notification_handlers <- function() list(
  "notifications/initialized"        = handle_initialized,
  "notifications/cancelled"          = function(server, session, params) {
    handle_cancelled(server, session, params)
  },
  "notifications/progress"           = function(server, session, params) {
    handle_progress_in(server, session, params)
  },
  "notifications/roots/list_changed" = function(server, session, params) {
    handle_roots_changed(server, session, params)
  }
)

# initialize / ping handlers ----------------------------------------------

handle_initialize <- function(server, session, params, msg = NULL) {
  session$protocol_version <- params$protocolVersion %||% mcp_protocol_version()
  session$client_capabilities <- params$capabilities %||% list()
  session$client_info <- params$clientInfo %||% list()
  drop_nulls(list(
    protocolVersion = mcp_protocol_version(),
    capabilities = server$capabilities(),
    serverInfo = server$server_info(),
    instructions = server$instructions
  ))
}

handle_ping <- function(server, session, params, msg = NULL) {
  list()
}

handle_initialized <- function(server, session, params) {
  if (isTRUE(session$initialized)) return(invisible(NULL))
  session$initialized <- TRUE
  for (fn in server$on_initialized_hooks) {
    safely(fn(server, session), log = TRUE)
  }
  invisible(NULL)
}

# Top-level route_message -------------------------------------------------

# Returns one of:
#  - NULL (notification handled)
#  - a jrpc_response/jrpc_error envelope (synchronous result)
#  - a list with .async = TRUE for offload to mirai
route_message <- function(server, session, msg) {
  kind <- jrpc_kind(msg)
  switch(kind,
    "request" = {
      method <- msg$method
      sync <- sync_handlers()
      asyn <- async_handlers()
      if (method %in% names(sync)) {
        res <- tryCatch(
          sync[[method]](server, session, msg$params %||% list(), msg),
          error = function(e) jrpc_error(msg$id, jrpc_codes$internal_error,
                                         conditionMessage(e))
        )
        if (is.list(res) && "code" %in% names(res$error %||% list())) return(res)
        return(jrpc_response(msg$id, res))
      }
      if (method %in% names(asyn)) {
        marker <- tryCatch(
          asyn[[method]](server, session, msg$params %||% list(), msg),
          error = function(e) jrpc_error(msg$id, jrpc_codes$internal_error,
                                         conditionMessage(e))
        )
        if (is.list(marker) && "code" %in% names(marker$error %||% list())) {
          return(marker)
        }
        marker$.async <- TRUE
        marker$.method <- method
        marker$.id <- msg$id
        return(marker)
      }
      jrpc_error(msg$id, jrpc_codes$method_not_found,
                 sprintf("method not found: %s", method))
    },
    "notification" = {
      notif <- notification_handlers()
      method <- msg$method
      if (method %in% names(notif)) {
        safely(notif[[method]](server, session, msg$params %||% list()),
               log = TRUE)
      }
      NULL
    },
    "response" = {
      session$resolve_pending(msg$id, msg$result, !is.null(msg$error))
      NULL
    },
    "invalid" = {
      jrpc_error(msg$id %||% NULL, jrpc_codes$invalid_request,
                 "invalid request")
    }
  )
}

# Run an async marker inside a mirai daemon and produce the final result
# envelope (a jrpc_response/jrpc_error). The caller schedules this via
# promises::then() on the transport thread.
finalize_async <- function(marker, server, session) {
  if (!is.null(marker$.tool_call)) {
    tool <- marker$tool; args <- marker$args; ctx <- marker$ctx
    if (isTRUE(tool$bidirectional)) {
      # Run on transport thread so the handler can drive the pending
      # request table directly. Wrap in a resolved promise for uniform
      # downstream handling.
      val <- tryCatch(tool$handler(args, ctx),
                      error = function(e) list(.mcp_err = TRUE,
                                                message = conditionMessage(e)))
      out <- if (is.list(val) && isTRUE(val$.mcp_err)) {
        jrpc_error(marker$.id, jrpc_codes$internal_error,
                   val$message %||% "tool failed")
      } else {
        jrpc_response(marker$.id, finalize_tool_result(tool, val))
      }
      return(inline_promise(out))
    }
    expr <- quote(handler_fn(call_args, call_ctx))
    p <- with_mirai(expr,
                    list(handler_fn = tool$handler,
                         call_args = args,
                         call_ctx = ctx))
    return(promises::then(p,
      onFulfilled = function(v) {
        if (is.list(v) && isTRUE(v$.mcp_err)) {
          return(jrpc_error(marker$.id, jrpc_codes$internal_error,
                            v$message %||% "tool failed"))
        }
        jrpc_response(marker$.id, finalize_tool_result(tool, v))
      },
      onRejected = function(e) {
        jrpc_error(marker$.id, jrpc_codes$internal_error,
                   conditionMessage(e))
      }))
  }
  if (!is.null(marker$.resource_call)) {
    r <- marker$resource; params <- marker$params; ctx <- marker$ctx
    expr <- quote(handler_fn(call_params, call_ctx))
    p <- with_mirai(expr, list(handler_fn = r$handler,
                               call_params = params,
                               call_ctx = ctx))
    return(promises::then(p,
      onFulfilled = function(v) {
        if (is.list(v) && isTRUE(v$.mcp_err)) {
          return(jrpc_error(marker$.id, jrpc_codes$internal_error,
                            v$message))
        }
        jrpc_response(marker$.id,
                      finalize_resource_read(params$uri,
                                             marker$mime_type, v))
      },
      onRejected = function(e) {
        jrpc_error(marker$.id, jrpc_codes$internal_error,
                   conditionMessage(e))
      }))
  }
  if (!is.null(marker$.prompt_call)) {
    p <- marker$prompt; args <- marker$args; ctx <- marker$ctx
    expr <- quote(handler_fn(call_args, call_ctx))
    promise <- with_mirai(expr, list(handler_fn = p$handler,
                                     call_args = args,
                                     call_ctx = ctx))
    return(promises::then(promise,
      onFulfilled = function(v) {
        if (is.list(v) && isTRUE(v$.mcp_err)) {
          return(jrpc_error(marker$.id, jrpc_codes$internal_error,
                            v$message))
        }
        jrpc_response(marker$.id, finalize_prompt_result(p, v))
      },
      onRejected = function(e) {
        jrpc_error(marker$.id, jrpc_codes$internal_error,
                   conditionMessage(e))
      }))
  }
  # Completion result is already final (synchronous compute).
  inline_promise(jrpc_response(marker$.id, marker))
}
