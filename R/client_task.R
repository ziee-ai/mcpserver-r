# Outbound client-task helper -------------------------------------------

# Spec: SEP-1686 task-augmented requests. When the server wants the
# client to perform a long-running operation (e.g. an LLM sample) it
# adds `params.task = { ttl }` to the request; the client immediately
# returns a `CreateTaskResult { task }` envelope and runs the work in
# the background. The server then polls the client's `tasks/get`
# endpoint and finally fetches the result with `tasks/result`.
#
# `ttl` and `pollInterval` are documented in the spec as
# *milliseconds*. The server-side helper accepts seconds at the R
# surface (consistent with the rest of the SDK) and converts.
#
# Caller must be on the transport thread — the underlying
# `call_client_blocking()` drives `later::run_now()` to await each
# leg of the round trip, which only works when `session$pending` is
# live in this process.

call_client_task <- function(session, method, params,
                             ttl = 30, poll_interval = 0.25,
                             total_timeout = 60) {
  caps <- session$client_capabilities %||% list()
  task_caps <- caps$tasks$requests
  # Capability gate: client must opt in to task-augmented variants of
  # the specific request method. The TS SDK's task helpers nest one
  # more key (sampling.createMessage, elicitation.create, tools.call)
  # to disambiguate per-method opt-in within each request family.
  declared <- switch(method,
    "sampling/createMessage" = task_caps$sampling$createMessage,
    "elicitation/create"     = task_caps$elicitation$create,
    "tools/call"             = task_caps$tools$call,
    NULL)
  if (is.null(declared)) {
    stop(sprintf(
      "client did not declare tasks.requests capability for '%s'",
      method))
  }
  ttl_ms <- as.integer(ttl * 1000)
  params$task <- list(ttl = ttl_ms)
  envelope <- call_client_blocking(session, method, params,
                                   timeout = total_timeout)
  task <- envelope$task
  if (is.null(task) || is.null(task$taskId)) {
    stop(sprintf(
      "client returned a non-task envelope for '%s' (no task.taskId)",
      method))
  }
  # The client may advertise its preferred poll interval (ms). We use
  # the smaller of the user's request and the client's hint, but never
  # below 50 ms — sub-50ms polls waste the daemon for no benefit.
  client_interval <- (task$pollInterval %||% 0L) / 1000
  if (client_interval > 0 && client_interval < poll_interval) {
    poll_interval <- max(client_interval, 0.05)
  }
  deadline <- Sys.time() + as.numeric(total_timeout)
  status <- task$status %||% "working"
  while (status %in% c("working", "input_required")) {
    if (Sys.time() > deadline) {
      # Best effort: tell the client to drop the task. We don't wait
      # for a reply.
      safely(session$send(jrpc_request("tasks/cancel",
        list(taskId = task$taskId),
        id = session$new_request_id())), log = FALSE)
      stop(sprintf("client task '%s' timed out", method))
    }
    Sys.sleep(poll_interval)
    get_res <- call_client_blocking(session, "tasks/get",
      list(taskId = task$taskId), timeout = total_timeout)
    task <- get_res$task %||% get_res
    status <- task$status %||% "working"
  }
  if (!identical(status, "completed")) {
    stop(sprintf("client task '%s' ended in status '%s'",
                 method, status))
  }
  call_client_blocking(session, "tasks/result",
    list(taskId = task$taskId), timeout = total_timeout)
}
