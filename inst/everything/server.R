## Reference "everything" demo server.
##
## Builds an McpServer that reproduces the feature surface of the MCP
## reference everything-server in idiomatic R. All 19 tools, 4 resource
## entries (2 templated + 2 static), 4 prompts, cascading completions, and
## the eight-level logging level switch are exposed. Tools that exercise
## server-to-client requests (sampling, elicitation, roots) are registered
## conditionally inside an `on_initialized` hook, so they only appear if
## the connected client declared the matching capability.

build_everything_server <- function() {
  srv <- mcpserver::new_server(
    name = "mcpserver-everything",
    version = "0.1.0",
    instructions = "Reference everything server reproduced with mcpserver."
  )

  # Tools always available --------------------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "echo",
    description = "Echoes the input back.",
    input_schema = mcpserver::schema(list(
      message = mcpserver::property_string("Message to echo", required = TRUE)
    )),
    handler = function(args, ctx) {
      mcpserver::response_text(args$message)
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "add",
    description = "Add two numbers.",
    input_schema = mcpserver::schema(list(
      a = mcpserver::property_number("First operand", required = TRUE),
      b = mcpserver::property_number("Second operand", required = TRUE)
    )),
    output_schema = mcpserver::schema(list(
      sum = mcpserver::property_number("Resulting sum", required = TRUE)
    )),
    handler = function(args, ctx) {
      mcpserver::response_structured(
        list(mcpserver::response_text(as.character(args$a + args$b))),
        list(sum = args$a + args$b)
      )
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "printEnv",
    description = "Return selected environment variables.",
    input_schema = mcpserver::schema(list()),
    handler = function(args, ctx) {
      vars <- c("HOME", "PATH", "LANG")
      out <- as.list(Sys.getenv(vars))
      mcpserver::response_text(jsonlite::toJSON(out, auto_unbox = TRUE,
                                                pretty = TRUE))
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "longRunningOperation",
    description = "A long-running operation emitting progress.",
    input_schema = mcpserver::schema(list(
      duration = mcpserver::property_integer("Steps", default = 3L)
    )),
    handler = function(args, ctx) {
      steps <- as.integer(args$duration %||% 3L)
      for (i in seq_len(steps)) {
        if (ctx$cancelled()) {
          return(mcpserver::response_error("cancelled"))
        }
        ctx$send_progress(i, total = steps,
                          message = paste("step", i))
        Sys.sleep(0.05)
      }
      mcpserver::response_text(sprintf("completed %d steps", steps))
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "getTinyImage",
    description = "Return a 1x1 transparent PNG.",
    input_schema = mcpserver::schema(list()),
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
    name = "annotatedMessage",
    description = "Return content blocks with annotations.",
    input_schema = mcpserver::schema(list(
      includeImage = mcpserver::property_boolean("Add image", default = FALSE)
    )),
    handler = function(args, ctx) {
      # MCP annotations.audience is an array of audience tags.
      txt <- mcpserver::response_text("annotated",
        annotations = list(audience = I("user"), priority = 0.9))
      if (isTRUE(args$includeImage)) {
        return(list(content = list(txt,
          mcpserver::response_text("image-placeholder",
            annotations = list(audience = I("assistant"))))))
      }
      list(content = list(txt))
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "getResourceReference",
    description = "Return an embedded resource block.",
    input_schema = mcpserver::schema(list(
      resourceId = mcpserver::property_string("Id", required = TRUE)
    )),
    handler = function(args, ctx) {
      uri <- sprintf("demo://resource/dynamic/text/%s", args$resourceId)
      mcpserver::response_resource(uri,
        text = paste("Embedded resource", args$resourceId),
        mime_type = "text/plain")
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "getResourceLinks",
    description = "Return resource link blocks pointing to dynamic resources.",
    input_schema = mcpserver::schema(list(
      count = mcpserver::property_integer("How many", default = 3L)
    )),
    handler = function(args, ctx) {
      n <- as.integer(args$count %||% 3L)
      links <- lapply(seq_len(n), function(i) {
        mcpserver::response_resource_link(
          sprintf("demo://resource/dynamic/text/%d", i),
          name = sprintf("doc-%d", i),
          mime_type = "text/plain")
      })
      list(content = links)
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "setLogLevel",
    description = "Set the active logging level.",
    input_schema = mcpserver::schema(list(
      level = mcpserver::property_enum(
        c("debug","info","notice","warning","error",
          "critical","alert","emergency"),
        required = TRUE)
    )),
    handler = function(args, ctx) {
      ctx$send_log(args$level,
                   sprintf("log level set to %s", args$level))
      mcpserver::response_text(sprintf("logging set to %s", args$level))
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "toggleSimulatedLogging",
    description = "Emit a burst of logging messages at all eight levels.",
    input_schema = mcpserver::schema(list(
      bursts = mcpserver::property_integer("Number of bursts", default = 1L)
    )),
    handler = function(args, ctx) {
      bursts <- as.integer(args$bursts %||% 1L)
      levels <- c("debug","info","notice","warning",
                  "error","critical","alert","emergency")
      for (b in seq_len(bursts)) {
        for (lvl in levels) {
          ctx$send_log(lvl,
            sprintf("simulated %s message (burst %d)", lvl, b))
        }
      }
      mcpserver::response_text(sprintf("emitted %d bursts", bursts))
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "toggleSubscriberUpdates",
    description = "Emit resources/updated notifications for the dynamic resource set.",
    input_schema = mcpserver::schema(list(
      count = mcpserver::property_integer("How many updates", default = 3L)
    )),
    handler = function(args, ctx) {
      n <- as.integer(args$count %||% 3L)
      for (i in seq_len(n)) {
        ctx$notify_resource_updated(
          sprintf("demo://resource/dynamic/text/%d", i))
      }
      mcpserver::response_text(sprintf("emitted %d updates", n))
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "gzipFileAsResource",
    description = "gzip a short input string and return it as an embedded resource blob.",
    input_schema = mcpserver::schema(list(
      content = mcpserver::property_string("Content to gzip", required = TRUE),
      name = mcpserver::property_string("Resource name", default = "archive.gz")
    )),
    handler = function(args, ctx) {
      tmp <- tempfile(fileext = ".gz")
      on.exit(unlink(tmp), add = TRUE)
      con <- gzfile(tmp, open = "wb")
      writeBin(charToRaw(args$content), con)
      close(con)
      bytes <- readBin(tmp, what = "raw", n = file.info(tmp)$size)
      mcpserver::response_resource(
        sprintf("demo://gz/%s",
                args$name %||% "archive.gz"),
        blob = bytes,
        mime_type = "application/gzip")
    }
  ))

  # Conditional tools that depend on client capabilities --------------------

  mcpserver::on_initialized(srv, function(mcp, session) {
    caps <- session$client_capabilities %||% list()

    if (!is.null(caps$sampling)) {
      mcpserver::add_capability(mcp, mcpserver::new_tool(
        name = "sampleLLM",
        description = "Ask the client to sample an LLM completion.",
        input_schema = mcpserver::schema(list(
          prompt = mcpserver::property_string(
            "Prompt to send to the LLM", required = TRUE)
        )),
        bidirectional = TRUE,
        handler = function(args, ctx) {
          res <- tryCatch(ctx$request_sampling(
            messages = list(list(
              role = "user",
              content = list(type = "text", text = args$prompt))),
            max_tokens = 64L,
            timeout = 15),
            error = function(e) NULL)
          if (is.null(res)) {
            return(mcpserver::response_error(
              "sampling request to client failed"))
          }
          mcpserver::response_text(
            res$content$text %||% jsonlite::toJSON(res, auto_unbox = TRUE))
        }
      ))
    }

    if (!is.null(caps$elicitation)) {
      mcpserver::add_capability(mcp, mcpserver::new_tool(
        name = "startElicitation",
        description = "Ask the client to elicit input from the user.",
        input_schema = mcpserver::schema(list(
          message = mcpserver::property_string(
            "Question to ask the user", required = TRUE)
        )),
        bidirectional = TRUE,
        handler = function(args, ctx) {
          res <- tryCatch(ctx$request_elicitation(
            message = args$message,
            requested_schema = mcpserver::schema(list(
              answer = mcpserver::property_string("Free-text answer",
                                                  required = TRUE))),
            timeout = 15),
            error = function(e) NULL)
          if (is.null(res)) {
            return(mcpserver::response_error(
              "elicitation request to client failed"))
          }
          ans <- res$content$answer %||% res$answer %||% ""
          mcpserver::response_text(sprintf("you said: %s", ans))
        }
      ))
    }

    if (!is.null(caps$roots)) {
      mcpserver::add_capability(mcp, mcpserver::new_tool(
        name = "listRoots",
        description = "Ask the client for its root list.",
        input_schema = mcpserver::schema(list()),
        bidirectional = TRUE,
        handler = function(args, ctx) {
          roots <- tryCatch(ctx$request_roots(timeout = 5),
                            error = function(e) {
                              conditionMessage(e)
                            })
          if (is.character(roots)) {
            return(mcpserver::response_error(
              paste("roots request to client failed:", roots)))
          }
          mcpserver::response_text(jsonlite::toJSON(roots,
                                                    auto_unbox = TRUE,
                                                    force = TRUE))
        }
      ))
    }
  })

  # Resources ---------------------------------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_resource(
    name = "readme",
    description = "Static readme",
    uri = "demo://static/readme",
    mime_type = "text/markdown",
    handler = function(params, ctx) "# mcpserver everything demo"
  ))
  mcpserver::add_capability(srv, mcpserver::new_resource(
    name = "manifest",
    description = "Static JSON manifest",
    uri = "demo://static/manifest",
    mime_type = "application/json",
    handler = function(params, ctx) {
      list(text = '{"name":"mcpserver-everything","version":"0.1.0"}')
    }
  ))
  mcpserver::add_capability(srv, mcpserver::new_resource_template(
    name = "dynamic-text",
    description = "Dynamically generated text resources",
    uri_template = "demo://resource/dynamic/text/{id}",
    mime_type = "text/plain",
    handler = function(params, ctx) {
      sprintf("Resource #%s body.", params$variables$id)
    }
  ))
  mcpserver::add_capability(srv, mcpserver::new_resource_template(
    name = "dynamic-blob",
    description = "Dynamically generated binary resources",
    uri_template = "demo://resource/dynamic/blob/{id}",
    mime_type = "application/octet-stream",
    handler = function(params, ctx) {
      list(blob = charToRaw(sprintf("blob-%s", params$variables$id)))
    }
  ))

  # Prompts ----------------------------------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_prompt(
    name = "simple",
    description = "A simple parameterless prompt",
    arguments = list(),
    handler = function(args, ctx) "Tell me a joke."
  ))
  mcpserver::add_capability(srv, mcpserver::new_prompt(
    name = "args-prompt",
    description = "A prompt with required arguments",
    arguments = list(
      mcpserver::new_prompt_argument("city", "City name", required = TRUE),
      mcpserver::new_prompt_argument("style", "Tone")
    ),
    handler = function(args, ctx) {
      style <- args$style %||% "friendly"
      sprintf("Greet a visitor to %s in a %s tone.", args$city, style)
    }
  ))
  mcpserver::add_capability(srv, mcpserver::new_prompt(
    name = "completions-prompt",
    description = "Prompt exercising cascading completions",
    arguments = list(
      mcpserver::new_prompt_argument("department", required = TRUE),
      mcpserver::new_prompt_argument("name", required = TRUE)
    ),
    complete = list(
      department = function(value, ctx, args) {
        opts <- c("Engineering", "Marketing", "Sales")
        grep(value, opts, value = TRUE, ignore.case = TRUE)
      },
      name = function(value, ctx, args) {
        dept <- args$department %||% ""
        opts <- switch(dept,
          Engineering = c("Alice", "Bob"),
          Marketing   = c("Carol", "Dan"),
          Sales       = c("Eve",   "Frank"),
          c("Ada", "Babbage"))
        grep(value, opts, value = TRUE, ignore.case = TRUE)
      }
    ),
    handler = function(args, ctx) {
      sprintf("Greet %s from %s.", args$name, args$department)
    }
  ))
  mcpserver::add_capability(srv, mcpserver::new_prompt(
    name = "embedded-resource-prompt",
    description = "Prompt with an embedded resource",
    arguments = list(
      mcpserver::new_prompt_argument("id", required = TRUE)
    ),
    handler = function(args, ctx) {
      list(
        description = "Embedded resource prompt",
        messages = list(
          list(role = "user",
               content = mcpserver::response_text(
                 paste("Look at resource", args$id))),
          list(role = "user",
               content = mcpserver::response_resource(
                 sprintf("demo://resource/dynamic/text/%s", args$id),
                 text = paste("inline body for", args$id),
                 mime_type = "text/plain"))
        )
      )
    }
  ))

  srv
}
