# Server-to-client request: sampling/createMessage ------------------------

# Called from inside a tool handler (running in a mirai daemon). We send a
# JSON-RPC request out to the client and block on a condition variable
# until the response arrives.

request_sampling_impl <- function(session, messages,
                                  model_preferences = NULL,
                                  system_prompt = NULL,
                                  max_tokens = 1024L,
                                  timeout = 30) {
  caps <- session$client_capabilities %||% list()
  if (is.null(caps$sampling)) {
    stop("client did not declare sampling capability")
  }
  call_client_blocking(session, "sampling/createMessage",
                       drop_nulls(list(
                         messages = messages,
                         modelPreferences = model_preferences,
                         systemPrompt = system_prompt,
                         maxTokens = max_tokens)),
                       timeout)
}
