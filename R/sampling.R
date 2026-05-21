# Server-to-client request: sampling/createMessage ------------------------

# Called from inside a tool handler (running in a mirai daemon). We send a
# JSON-RPC request out to the client and block on a condition variable
# until the response arrives.

request_sampling_impl <- function(session, messages,
                                  model_preferences = NULL,
                                  system_prompt = NULL,
                                  max_tokens = 1024L,
                                  timeout = 30,
                                  tools = NULL,
                                  tool_choice = NULL) {
  caps <- session$client_capabilities %||% list()
  if (is.null(caps$sampling)) {
    stop("client did not declare sampling capability")
  }
  # SEP-XXXX (TS createMessage v2): server may provide `tools` and
  # `toolChoice` so the client's model can issue tool_use calls.
  # The client must opt in to receiving them via the
  # `sampling.tools` sub-capability.
  if ((!is.null(tools) || !is.null(tool_choice)) &&
      is.null(caps$sampling$tools)) {
    stop(paste("client did not declare sampling.tools capability",
               "required when 'tools' or 'tool_choice' is supplied"))
  }
  validate_sampling_messages(messages)
  call_client_blocking(session, "sampling/createMessage",
                       drop_nulls(list(
                         messages = messages,
                         modelPreferences = model_preferences,
                         systemPrompt = system_prompt,
                         maxTokens = max_tokens,
                         tools = tools,
                         toolChoice = tool_choice)),
                       timeout)
}

# Validate the protocol invariants on a sampling message list:
#   - every message has a role ("user" or "assistant") and content
#   - tool_result content must follow a tool_use in the prior message
#   - tool_result ids must match the immediately-prior tool_use ids
# Throws on violation.
validate_sampling_messages <- function(messages) {
  if (!is.list(messages)) {
    stop("messages must be a list")
  }
  for (i in seq_along(messages)) {
    m <- messages[[i]]
    role <- m$role
    if (!isTRUE(role %in% c("user", "assistant"))) {
      stop(sprintf("message[%d].role must be 'user' or 'assistant'",
                   i))
    }
    cs <- m$content
    if (is.null(cs)) next
    # Content can be a single block or an array of blocks; for the
    # tool_use/tool_result invariants we only need to inspect arrays.
    blocks <- if (is.list(cs) && !is.null(cs$type)) list(cs) else cs
    if (!is.list(blocks)) next
    # Find tool_use ids in this message; tool_results in the NEXT
    # user message must map onto them 1:1 (in any order).
    if (identical(role, "assistant")) {
      use_ids <- vapply(blocks, function(b) {
        if (is.list(b) && identical(b$type, "tool_use")) {
          as.character(b$id %||% NA_character_)
        } else NA_character_
      }, character(1L))
      use_ids <- use_ids[!is.na(use_ids)]
      if (length(use_ids) > 0L) {
        nxt <- messages[[i + 1L]]
        if (is.null(nxt) || !identical(nxt$role, "user")) {
          stop(sprintf(
            "message[%d] issues tool_use but next message is missing or non-user",
            i))
        }
        nxt_blocks <- if (is.list(nxt$content) && !is.null(nxt$content$type)) {
          list(nxt$content)
        } else nxt$content
        if (!is.list(nxt_blocks)) {
          stop(sprintf(
            "message[%d] (next, user) has no content blocks",
            i + 1L))
        }
        result_ids <- vapply(nxt_blocks, function(b) {
          if (is.list(b) && identical(b$type, "tool_result")) {
            as.character(b$tool_use_id %||% NA_character_)
          } else NA_character_
        }, character(1L))
        result_ids <- result_ids[!is.na(result_ids)]
        missing <- setdiff(use_ids, result_ids)
        if (length(missing) > 0L) {
          stop(sprintf(
            "missing tool_result for tool_use id(s): %s",
            paste(missing, collapse = ", ")))
        }
        extra <- setdiff(result_ids, use_ids)
        if (length(extra) > 0L) {
          stop(sprintf(
            "tool_result id(s) without matching tool_use: %s",
            paste(extra, collapse = ", ")))
        }
      }
    }
  }
  invisible(NULL)
}
