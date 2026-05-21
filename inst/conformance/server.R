## Fixture server for the official MCP conformance suite.
## Mirrors the tool/resource/prompt names that
## `@modelcontextprotocol/conformance server` expects.

build_conformance_server <- function() {
  srv <- mcpserver::new_server(
    name = "mcpserver-conformance",
    title = "mcpserver Conformance Fixture",
    version = "0.1.0",
    instructions = "Test fixture exposing the names that @modelcontextprotocol/conformance expects.",
    capabilities = list(
      experimental = list(tasks = list(list = TRUE, cancel = TRUE))
    )
  )

  # ----- tools ---------------------------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "test_simple_text",
    description = "Returns a single text content block.",
    input_schema = mcpserver::schema(list()),
    annotations = list(title = "Simple Text", readOnlyHint = TRUE),
    handler = function(args, ctx) {
      mcpserver::response_text(
        "This is a simple text response for testing.")
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "test_image_content",
    description = "Returns a single image content block (1x1 PNG).",
    input_schema = mcpserver::schema(list()),
    annotations = list(title = "Image Content", readOnlyHint = TRUE),
    handler = function(args, ctx) {
      tiny <- as.raw(c(
        0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a,
        0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52,
        0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
        0x08, 0x06, 0x00, 0x00, 0x00, 0x1f, 0x15, 0xc4,
        0x89, 0x00, 0x00, 0x00, 0x0d, 0x49, 0x44, 0x41,
        0x54, 0x78, 0x9c, 0x63, 0x00, 0x01, 0x00, 0x00,
        0x05, 0x00, 0x01, 0x0d, 0x0a, 0x2d, 0xb4, 0x00,
        0x00, 0x00, 0x00, 0x49, 0x45, 0x4e, 0x44, 0xae,
        0x42, 0x60, 0x82))
      mcpserver::response_image(tiny, "image/png")
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "test_audio_content",
    description = "Returns a single audio content block (mock MP3 bytes).",
    input_schema = mcpserver::schema(list()),
    annotations = list(title = "Audio Content", readOnlyHint = TRUE),
    handler = function(args, ctx) {
      mcpserver::response_audio(charToRaw("mock-audio-bytes"),
                                "audio/wav")
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "test_multiple_content_types",
    description = "Returns multiple content blocks (text, image, text).",
    input_schema = mcpserver::schema(list()),
    annotations = list(title = "Mixed Content", readOnlyHint = TRUE),
    handler = function(args, ctx) {
      tiny <- as.raw(c(
        0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a,
        0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52,
        0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
        0x08, 0x06, 0x00, 0x00, 0x00, 0x1f, 0x15, 0xc4,
        0x89, 0x00, 0x00, 0x00, 0x0d, 0x49, 0x44, 0x41,
        0x54, 0x78, 0x9c, 0x63, 0x00, 0x01, 0x00, 0x00,
        0x05, 0x00, 0x01, 0x0d, 0x0a, 0x2d, 0xb4, 0x00,
        0x00, 0x00, 0x00, 0x49, 0x45, 0x4e, 0x44, 0xae,
        0x42, 0x60, 0x82))
      list(content = list(
        mcpserver::response_text("Multiple content types test:"),
        mcpserver::response_image(tiny, "image/png"),
        mcpserver::response_resource("test://mixed-content-resource",
          text = "mixed content embedded resource",
          mime_type = "text/plain"),
        mcpserver::response_text("End of mixed content.")
      ))
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "test_embedded_resource",
    description = "Returns an embedded resource block.",
    input_schema = mcpserver::schema(list()),
    annotations = list(title = "Embedded Resource", readOnlyHint = TRUE),
    handler = function(args, ctx) {
      mcpserver::response_resource("test://embedded-resource",
        text = "embedded resource body",
        mime_type = "text/plain")
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "test_tool_with_logging",
    description = "Emits a logging notification, then returns text.",
    input_schema = mcpserver::schema(list()),
    annotations = list(title = "Logging Tool", readOnlyHint = FALSE),
    bidirectional = TRUE,
    handler = function(args, ctx) {
      # Conformance suite asserts at least 3 logging messages.
      ctx$send_log("info", "tool start")
      ctx$send_log("info", "tool working")
      ctx$send_log("info", "tool finishing")
      mcpserver::response_text("Logging emitted.")
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "test_tool_with_progress",
    description = "Emits progress notifications then returns text.",
    input_schema = mcpserver::schema(list()),
    annotations = list(title = "Progress Tool", readOnlyHint = FALSE),
    bidirectional = TRUE,
    handler = function(args, ctx) {
      if (!is.null(ctx$progress_token)) {
        for (i in 1:3) {
          ctx$send_progress(i, total = 3,
                            message = sprintf("step %d", i))
        }
      }
      mcpserver::response_text("Progress emitted.")
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "test_error_handling",
    description = "Returns a tool result with isError=true.",
    input_schema = mcpserver::schema(list()),
    annotations = list(title = "Error Handler", readOnlyHint = TRUE),
    handler = function(args, ctx) {
      mcpserver::response_error("intentional tool error")
    }
  ))

  # ----- resources -----------------------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_resource(
    name = "static-text",
    description = "A static text resource.",
    uri = "test://static-text",
    mime_type = "text/plain",
    handler = function(params, ctx) "This is a static text resource."
  ))
  mcpserver::add_capability(srv, mcpserver::new_resource(
    name = "static-binary",
    description = "A static binary resource.",
    uri = "test://static-binary",
    mime_type = "application/octet-stream",
    handler = function(params, ctx) {
      list(blob = charToRaw("static-binary-bytes"))
    }
  ))
  mcpserver::add_capability(srv, mcpserver::new_resource(
    name = "watched-resource",
    description = "A resource that supports subscriptions.",
    uri = "test://watched-resource",
    mime_type = "text/plain",
    handler = function(params, ctx) "Watched resource body."
  ))
  mcpserver::add_capability(srv, mcpserver::new_resource_template(
    name = "templated-data",
    description = "A templated resource keyed by id.",
    uri_template = "test://template/{id}/data",
    mime_type = "text/plain",
    handler = function(params, ctx) {
      sprintf("Template body for id=%s.", params$variables$id)
    }
  ))

  # ----- prompts -------------------------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_prompt(
    name = "test_simple_prompt",
    description = "A simple prompt without arguments.",
    arguments = list(),
    handler = function(args, ctx) "This is a simple prompt."
  ))
  mcpserver::add_capability(srv, mcpserver::new_prompt(
    name = "test_prompt_with_arguments",
    description = "A prompt taking arg1 and arg2.",
    arguments = list(
      mcpserver::new_prompt_argument("arg1", "First argument",
                                     required = TRUE),
      mcpserver::new_prompt_argument("arg2", "Second argument",
                                     required = TRUE)
    ),
    handler = function(args, ctx) {
      sprintf("Prompt with arg1=%s and arg2=%s",
              args$arg1, args$arg2)
    }
  ))
  mcpserver::add_capability(srv, mcpserver::new_prompt(
    name = "test_prompt_with_embedded_resource",
    description = "A prompt that embeds a resource.",
    arguments = list(),
    handler = function(args, ctx) {
      list(
        description = "Prompt with embedded resource",
        messages = list(
          list(role = "user",
               content = mcpserver::response_text(
                 "Inspect this resource:")),
          list(role = "user",
               content = mcpserver::response_resource(
                 "test://embedded-resource",
                 text = "embedded resource body",
                 mime_type = "text/plain"))
        )
      )
    }
  ))
  mcpserver::add_capability(srv, mcpserver::new_prompt(
    name = "test_prompt_with_image",
    description = "A prompt that includes an image.",
    arguments = list(),
    handler = function(args, ctx) {
      tiny <- as.raw(c(
        0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a,
        0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52,
        0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
        0x08, 0x06, 0x00, 0x00, 0x00, 0x1f, 0x15, 0xc4,
        0x89, 0x00, 0x00, 0x00, 0x0d, 0x49, 0x44, 0x41,
        0x54, 0x78, 0x9c, 0x63, 0x00, 0x01, 0x00, 0x00,
        0x05, 0x00, 0x01, 0x0d, 0x0a, 0x2d, 0xb4, 0x00,
        0x00, 0x00, 0x00, 0x49, 0x45, 0x4e, 0x44, 0xae,
        0x42, 0x60, 0x82))
      list(
        description = "Prompt with image",
        messages = list(
          list(role = "user",
               content = mcpserver::response_text("Look at this:")),
          list(role = "user",
               content = mcpserver::response_image(tiny, "image/png"))
        )
      )
    }
  ))

  # ----- conditional tools for sampling / elicitation ------------------

  mcpserver::on_initialized(srv, function(mcp, session) {
    caps <- session$client_capabilities %||% list()
    if (!is.null(caps$sampling)) {
      mcpserver::add_capability(mcp, mcpserver::new_tool(
        name = "test_sampling",
        description = "Asks the client to sample an LLM completion.",
        input_schema = mcpserver::schema(list(
          prompt = mcpserver::property_string(required = TRUE)
        )),
        bidirectional = TRUE,
        handler = function(args, ctx) {
          res <- tryCatch(ctx$request_sampling(
            messages = list(list(role = "user",
                                 content = list(type = "text",
                                                text = args$prompt))),
            max_tokens = 64L, timeout = 30),
            error = function(e) NULL)
          if (is.null(res)) {
            return(mcpserver::response_error("sampling failed"))
          }
          mcpserver::response_text(
            res$content$text %||% jsonlite::toJSON(res, auto_unbox = TRUE))
        }
      ))
    }
    if (!is.null(caps$elicitation)) {
      mcpserver::add_capability(mcp, mcpserver::new_tool(
        name = "test_elicitation",
        description = "Asks the client to elicit input from the user.",
        input_schema = mcpserver::schema(list(
          message = mcpserver::property_string(required = TRUE)
        )),
        bidirectional = TRUE,
        handler = function(args, ctx) {
          res <- tryCatch(ctx$request_elicitation(
            message = args$message,
            requested_schema = mcpserver::schema(list(
              answer = mcpserver::property_string(required = TRUE))),
            timeout = 30),
            error = function(e) NULL)
          if (is.null(res)) {
            return(mcpserver::response_error("elicitation failed"))
          }
          ans <- res$content$answer %||% res$answer %||% ""
          mcpserver::response_text(sprintf("answer: %s", ans))
        }
      ))

      mcpserver::add_capability(mcp, mcpserver::new_tool(
        name = "test_elicitation_sep1034_defaults",
        description = "SEP-1034 elicitation with default values for every primitive type.",
        input_schema = mcpserver::schema(list()),
        bidirectional = TRUE,
        handler = function(args, ctx) {
          res <- tryCatch(ctx$request_elicitation(
            message = "Please confirm or override these defaults",
            requested_schema = list(
              type = "object",
              properties = list(
                name = list(type = "string",
                            default = "John Doe"),
                age = list(type = "integer",
                           default = 30L),
                score = list(type = "number",
                             default = 95.5),
                status = list(type = "string",
                              enum = I(c("active", "inactive",
                                         "pending")),
                              default = "active"),
                verified = list(type = "boolean",
                                default = TRUE))),
            timeout = 30),
            error = function(e) NULL)
          if (is.null(res)) {
            return(mcpserver::response_text(
              "Elicitation completed: action=cancel, content={}"))
          }
          mcpserver::response_text(sprintf(
            "Elicitation completed: action=%s, content=%s",
            res$action %||% "accept",
            jsonlite::toJSON(res$content %||% list(),
                             auto_unbox = TRUE, force = TRUE)))
        }
      ))

      mcpserver::add_capability(mcp, mcpserver::new_tool(
        name = "test_elicitation_sep1330_enums",
        description = "SEP-1330 elicitation with five enum variants.",
        input_schema = mcpserver::schema(list()),
        bidirectional = TRUE,
        handler = function(args, ctx) {
          res <- tryCatch(ctx$request_elicitation(
            message = "Pick options from each variant",
            requested_schema = list(
              type = "object",
              properties = list(
                untitledSingle = list(type = "string",
                                      enum = I(c("option1",
                                                  "option2",
                                                  "option3"))),
                titledSingle = list(
                  type = "string",
                  oneOf = list(
                    list(const = "value1", title = "First Option"),
                    list(const = "value2", title = "Second Option"),
                    list(const = "value3", title = "Third Option"))),
                legacyEnum = list(
                  type = "string",
                  enum = I(c("opt1", "opt2", "opt3")),
                  enumNames = I(c("Option One", "Option Two",
                                  "Option Three"))),
                untitledMulti = list(
                  type = "array",
                  items = list(type = "string",
                               enum = I(c("option1", "option2",
                                          "option3")))),
                titledMulti = list(
                  type = "array",
                  items = list(
                    anyOf = list(
                      list(const = "value1",
                           title = "First Choice"),
                      list(const = "value2",
                           title = "Second Choice"),
                      list(const = "value3",
                           title = "Third Choice")))))),
            timeout = 30),
            error = function(e) NULL)
          if (is.null(res)) {
            return(mcpserver::response_text(
              "Elicitation completed: action=cancel, content={}"))
          }
          mcpserver::response_text(sprintf(
            "Elicitation completed: action=%s, content=%s",
            res$action %||% "accept",
            jsonlite::toJSON(res$content %||% list(),
                             auto_unbox = TRUE, force = TRUE)))
        }
      ))
    }
  })

  srv
}
