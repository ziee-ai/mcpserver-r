## Reference "everything" demo server (TypeScript parity).
##
## Mirrors `src/everything/everything.ts` from
## modelcontextprotocol/servers. Tool, resource and prompt names match
## the TypeScript reference verbatim so client identifiers carry across
## both servers. Behaviours that are spec-significant — periodic
## simulated logging, periodic subscriber updates, `syncRoots` startup
## delay, `_meta.progressToken` gating, instructions-from-file — are
## reproduced.

# Server-level state for the toggle timers. One env per server build so
# state is bound to the McpServer instance via `srv$extension_state`.
.new_everything_state <- function() {
  st <- new.env(parent = emptyenv())
  st$logging_on <- FALSE       # toggleable 5s simulated logging
  st$subscriber_on <- FALSE    # toggleable 5s subscriber updates
  st$roots_cache <- list()     # per-session cached roots
  st$started_at <- Sys.time()
  st
}

# Schedule a recurring callback every `delay_s` seconds using `later`,
# guarded by a boolean predicate so the loop ends cleanly when toggled
# off. Returns invisibly.
.schedule_recurring <- function(delay_s, predicate, body) {
  step <- function() {
    if (!isTRUE(predicate())) return(invisible(NULL))
    safely(body(), log = FALSE)
    later::later(step, delay = delay_s)
  }
  later::later(step, delay = delay_s)
  invisible(NULL)
}

# MIME inference for the static docs directory.
.mime_for_ext <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    "md"   = "text/markdown",
    "txt"  = "text/plain",
    "json" = "application/json",
    "html" = "text/html",
    "application/octet-stream")
}

# Tool helper: pretty-print a value as text.
.as_text <- function(value) {
  if (is.character(value) && length(value) == 1L) return(value)
  jsonlite::toJSON(value, auto_unbox = TRUE, pretty = TRUE, force = TRUE)
}

build_everything_server <- function() {
  docs_dir <- system.file("everything", "docs", package = "mcpserver")
  if (!nzchar(docs_dir)) docs_dir <- file.path("inst", "everything", "docs")

  instructions <- tryCatch(
    paste(readLines(file.path(docs_dir, "instructions.md"),
                    warn = FALSE), collapse = "\n"),
    error = function(e)
      "mcpserver reference everything server.")

  srv <- mcpserver::new_server(
    name = "mcp-servers/everything",
    title = "Everything Reference Server",
    version = "2.0.0",
    instructions = instructions,
    capabilities = list(
      experimental = list(
        tasks = list(list = TRUE, cancel = TRUE)
      )
    )
  )

  state <- .new_everything_state()
  srv$extension_state <- state

  # ----- echo -----------------------------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "echo",
    description = "Echoes the input back to the caller, prefixed with 'Echo: '.",
    input_schema = mcpserver::schema(list(
      message = mcpserver::property_string(
        "Message to echo", required = TRUE)
    )),
    annotations = list(
      title = "Echo",
      readOnlyHint = TRUE,
      idempotentHint = TRUE,
      openWorldHint = FALSE),
    handler = function(args, ctx) {
      mcpserver::response_text(paste0("Echo: ", args$message))
    }
  ))

  # ----- get-sum --------------------------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "get-sum",
    description = "Adds two numbers and returns the result as text.",
    input_schema = mcpserver::schema(list(
      a = mcpserver::property_number("First operand", required = TRUE),
      b = mcpserver::property_number("Second operand", required = TRUE)
    )),
    annotations = list(
      title = "Get Sum",
      readOnlyHint = TRUE, idempotentHint = TRUE,
      openWorldHint = FALSE),
    handler = function(args, ctx) {
      mcpserver::response_text(sprintf("The sum of %g and %g is %g.",
                                       args$a, args$b, args$a + args$b))
    }
  ))

  # ----- get-structured-content ----------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "get-structured-content",
    description = "Returns structured weather data alongside human-readable text.",
    input_schema = mcpserver::schema(list(
      location = mcpserver::property_string(
        "Location to fetch weather for", required = TRUE)
    )),
    output_schema = mcpserver::schema(list(
      temperature = mcpserver::property_number(
        "Temperature in Celsius", required = TRUE),
      conditions = mcpserver::property_string(
        "Conditions, e.g. clear", required = TRUE),
      humidity = mcpserver::property_number(
        "Humidity percentage", required = TRUE)
    )),
    annotations = list(title = "Get Structured Content",
                       readOnlyHint = TRUE, openWorldHint = FALSE),
    handler = function(args, ctx) {
      data <- list(
        temperature = 22.5,
        conditions = "Partly cloudy",
        humidity = 65)
      mcpserver::response_structured(
        list(mcpserver::response_text(
          sprintf("Weather in %s: %s, %g°C, %g%% humidity",
                  args$location, data$conditions,
                  data$temperature, data$humidity))),
        data)
    }
  ))

  # ----- get-env --------------------------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "get-env",
    description = "Returns the server's process environment as JSON.",
    input_schema = mcpserver::schema(list()),
    annotations = list(title = "Get Environment",
                       readOnlyHint = TRUE, openWorldHint = FALSE),
    handler = function(args, ctx) {
      env <- as.list(Sys.getenv())
      mcpserver::response_text(jsonlite::toJSON(env,
                                                auto_unbox = TRUE,
                                                pretty = TRUE))
    }
  ))

  # ----- trigger-long-running-operation --------------------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "trigger-long-running-operation",
    description = "Runs an operation that emits progress notifications. Gated by `_meta.progressToken`.",
    input_schema = mcpserver::schema(list(
      duration = mcpserver::property_number("Total seconds", default = 1),
      steps = mcpserver::property_integer("Number of progress steps",
                                          default = 4L)
    )),
    annotations = list(title = "Long-running operation",
                       readOnlyHint = FALSE, openWorldHint = FALSE),
    handler = function(args, ctx) {
      total <- as.numeric(args$duration %||% 1)
      steps <- as.integer(args$steps %||% 4L)
      step_dt <- total / max(1L, steps)
      for (i in seq_len(steps)) {
        if (ctx$cancelled()) {
          return(mcpserver::response_error("cancelled"))
        }
        # Only emit progress when the client actually sent a token.
        if (!is.null(ctx$progress_token)) {
          ctx$send_progress(i, total = steps,
                            message = sprintf("step %d/%d", i, steps))
        }
        Sys.sleep(step_dt)
      }
      mcpserver::response_text(sprintf(
        "Operation completed in %.2f seconds (%d steps)", total, steps))
    }
  ))

  # ----- get-tiny-image -------------------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "get-tiny-image",
    description = "Returns a tri-block message: intro text + 1x1 PNG + outro text.",
    input_schema = mcpserver::schema(list()),
    annotations = list(title = "Get Tiny Image",
                       readOnlyHint = TRUE, openWorldHint = FALSE),
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
        mcpserver::response_text("This is a tiny image:"),
        mcpserver::response_image(tiny, "image/png"),
        mcpserver::response_text("The image above is the MCP tiny image marker.")
      ))
    }
  ))

  # ----- get-annotated-message -----------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "get-annotated-message",
    description = "Returns content blocks with audience and priority annotations.",
    input_schema = mcpserver::schema(list(
      messageType = mcpserver::property_enum(
        c("error", "success", "debug"),
        description = "Type of annotated message", required = TRUE),
      includeImage = mcpserver::property_boolean(
        "Include the tiny image too", default = FALSE)
    )),
    annotations = list(title = "Get Annotated Message",
                       readOnlyHint = TRUE, openWorldHint = FALSE),
    handler = function(args, ctx) {
      msg_type <- args$messageType
      text_block <- switch(msg_type,
        "error"   = mcpserver::response_text(
          "Error: operation failed",
          annotations = list(audience = I(c("user", "assistant")),
                             priority = 1.0)),
        "success" = mcpserver::response_text(
          "Operation completed successfully",
          annotations = list(audience = I("user"), priority = 0.7)),
        "debug"   = mcpserver::response_text(
          "Debug: internal state dumped",
          annotations = list(audience = I("assistant"), priority = 0.3)))
      blocks <- list(text_block)
      if (isTRUE(args$includeImage)) {
        tiny <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a,
                         0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52,
                         0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
                         0x08, 0x06, 0x00, 0x00, 0x00, 0x1f, 0x15, 0xc4,
                         0x89, 0x00, 0x00, 0x00, 0x0d, 0x49, 0x44, 0x41,
                         0x54, 0x78, 0x9c, 0x63, 0x00, 0x01, 0x00, 0x00,
                         0x05, 0x00, 0x01, 0x0d, 0x0a, 0x2d, 0xb4, 0x00,
                         0x00, 0x00, 0x00, 0x49, 0x45, 0x4e, 0x44, 0xae,
                         0x42, 0x60, 0x82))
        blocks <- c(blocks, list(mcpserver::response_image(
          tiny, "image/png",
          annotations = list(audience = I("user"), priority = 0.5))))
      }
      list(content = blocks)
    }
  ))

  # ----- get-resource-reference ----------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "get-resource-reference",
    description = "Returns three blocks: intro text, an embedded resource, outro text.",
    input_schema = mcpserver::schema(list(
      resourceType = mcpserver::property_enum(
        c("Text", "Blob"),
        description = "Whether to embed the text or blob variant",
        required = TRUE),
      resourceId = mcpserver::property_string(
        "Numeric id", required = TRUE)
    )),
    annotations = list(title = "Get Resource Reference",
                       readOnlyHint = TRUE, openWorldHint = FALSE),
    handler = function(args, ctx) {
      kind <- tolower(args$resourceType)
      uri <- sprintf("demo://resource/dynamic/%s/%s",
                     kind, args$resourceId)
      embedded <- if (identical(kind, "blob")) {
        mcpserver::response_resource(
          uri,
          blob = charToRaw(sprintf("blob-%s", args$resourceId)),
          mime_type = "application/octet-stream")
      } else {
        mcpserver::response_resource(
          uri,
          text = sprintf("Resource #%s body.", args$resourceId),
          mime_type = "text/plain")
      }
      list(content = list(
        mcpserver::response_text(
          sprintf("Embedding %s resource %s:",
                  args$resourceType, args$resourceId)),
        embedded,
        mcpserver::response_text("End of embedded resource.")
      ))
    }
  ))

  # ----- get-resource-links --------------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "get-resource-links",
    description = "Returns intro text + N alternating resource_link blocks.",
    input_schema = mcpserver::schema(list(
      count = mcpserver::property_integer(
        "How many links (1..10)", default = 3L, minimum = 1L,
        maximum = 10L)
    )),
    annotations = list(title = "Get Resource Links",
                       readOnlyHint = TRUE, openWorldHint = FALSE),
    handler = function(args, ctx) {
      n <- as.integer(args$count %||% 3L)
      links <- lapply(seq_len(n), function(i) {
        if (i %% 2L == 1L) {
          mcpserver::response_resource_link(
            sprintf("demo://resource/dynamic/text/%d", i),
            name = sprintf("doc-%d", i),
            description = sprintf("Text resource %d", i),
            mime_type = "text/plain")
        } else {
          mcpserver::response_resource_link(
            sprintf("demo://resource/dynamic/blob/%d", i),
            name = sprintf("blob-%d", i),
            description = sprintf("Blob resource %d", i),
            mime_type = "application/octet-stream")
        }
      })
      list(content = c(list(mcpserver::response_text(
        sprintf("Here are %d resource links:", n))), links))
    }
  ))

  # ----- gzip-file-as-resource -----------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "gzip-file-as-resource",
    description = "Gzips the provided content and returns it as an embedded resource.",
    input_schema = mcpserver::schema(list(
      content = mcpserver::property_string(
        "Content to gzip", required = TRUE),
      name = mcpserver::property_string(
        "Resource name", default = "archive.gz"),
      outputType = mcpserver::property_enum(
        c("resource", "resource_link"),
        description = "Return the bytes inline or just a link",
        default = "resource")
    )),
    annotations = list(title = "Gzip File as Resource",
                       readOnlyHint = FALSE, openWorldHint = FALSE),
    handler = function(args, ctx) {
      tmp <- tempfile(fileext = ".gz")
      on.exit(unlink(tmp), add = TRUE)
      con <- gzfile(tmp, open = "wb")
      writeBin(charToRaw(args$content), con)
      close(con)
      bytes <- readBin(tmp, what = "raw", n = file.info(tmp)$size)
      uri <- sprintf("demo://resource/session/%s",
                     args$name %||% "archive.gz")
      if (identical(args$outputType, "resource_link")) {
        mcpserver::response_resource_link(
          uri, name = args$name %||% "archive.gz",
          mime_type = "application/gzip")
      } else {
        mcpserver::response_resource(
          uri, blob = bytes,
          mime_type = "application/gzip")
      }
    }
  ))

  # ----- toggle-simulated-logging --------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "toggle-simulated-logging",
    description = "Toggles a 5-second interval that emits random-level log messages.",
    input_schema = mcpserver::schema(list(
      enable = mcpserver::property_boolean(
        "Enable or disable", required = TRUE)
    )),
    annotations = list(title = "Toggle Simulated Logging",
                       readOnlyHint = FALSE, openWorldHint = FALSE),
    bidirectional = TRUE,
    handler = function(args, ctx) {
      sess <- ctx$.session
      st <- ctx$.session$server$extension_state
      if (isTRUE(args$enable)) {
        if (isTRUE(st$logging_on)) {
          return(mcpserver::response_text("simulated logging already on"))
        }
        st$logging_on <- TRUE
        levels <- c("debug","info","notice","warning","error",
                    "critical","alert","emergency")
        .schedule_recurring(5,
          predicate = function() isTRUE(st$logging_on),
          body = function() {
            lvl <- sample(levels, 1L)
            mcpserver:::send_log(sess, lvl,
              sprintf("simulated %s message at %s",
                      lvl, format(Sys.time(), "%H:%M:%S")))
          })
        mcpserver::response_text("simulated logging enabled (5s interval)")
      } else {
        st$logging_on <- FALSE
        mcpserver::response_text("simulated logging disabled")
      }
    }
  ))

  # ----- toggle-subscriber-updates -------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "toggle-subscriber-updates",
    description = "Toggles a 5-second interval that emits resources/updated notifications for subscribed URIs.",
    input_schema = mcpserver::schema(list(
      enable = mcpserver::property_boolean(
        "Enable or disable", required = TRUE)
    )),
    annotations = list(title = "Toggle Subscriber Updates",
                       readOnlyHint = FALSE, openWorldHint = FALSE),
    bidirectional = TRUE,
    handler = function(args, ctx) {
      sess <- ctx$.session
      st <- sess$server$extension_state
      if (isTRUE(args$enable)) {
        if (isTRUE(st$subscriber_on)) {
          return(mcpserver::response_text("subscriber updates already on"))
        }
        st$subscriber_on <- TRUE
        .schedule_recurring(5,
          predicate = function() isTRUE(st$subscriber_on),
          body = function() {
            for (uri in ls(sess$subs, all.names = TRUE)) {
              sess$send(mcpserver:::jrpc_notification(
                "notifications/resources/updated",
                list(uri = uri)))
            }
          })
        mcpserver::response_text("subscriber updates enabled (5s interval)")
      } else {
        st$subscriber_on <- FALSE
        mcpserver::response_text("subscriber updates disabled")
      }
    }
  ))

  # ----- simulate-research-query (tasks lifecycle) ---------------------

  mcpserver::add_capability(srv, mcpserver::new_tool(
    name = "simulate-research-query",
    description = "Multi-stage research task: enqueue, run, report — exercises ctx$task.",
    input_schema = mcpserver::schema(list(
      topic = mcpserver::property_string("Topic", required = TRUE),
      steps = mcpserver::property_integer("Number of stages",
                                          default = 3L,
                                          minimum = 1L, maximum = 10L)
    )),
    annotations = list(title = "Simulate Research Query",
                       readOnlyHint = TRUE, openWorldHint = FALSE),
    tasks = TRUE,
    handler = function(args, ctx) {
      stages <- as.integer(args$steps %||% 3L)
      ctx$task$append_message(list(
        type = "text",
        text = sprintf("research started on '%s' (%d stages)",
                       args$topic, stages)))
      report_lines <- character(0L)
      for (i in seq_len(stages)) {
        if (isTRUE(ctx$task$cancelled())) {
          return(mcpserver::response_error("research cancelled"))
        }
        line <- sprintf("stage %d/%d: surveyed '%s'",
                        i, stages, args$topic)
        ctx$task$append_message(list(type = "text", text = line))
        if (!is.null(ctx$progress_token)) {
          ctx$send_progress(i, total = stages,
                            message = sprintf("stage %d", i))
        }
        report_lines <- c(report_lines, line)
        Sys.sleep(0.05)
      }
      ctx$task$update_status("completed")
      report <- paste(c(
        sprintf("# Research report: %s", args$topic),
        "",
        report_lines,
        "",
        sprintf("Task id: %s", ctx$task$id)),
        collapse = "\n")
      mcpserver::response_text(report)
    }
  ))

  # ----- Conditional tools (registered after initialize) ---------------

  mcpserver::on_initialized(srv, function(mcp, session) {
    caps <- session$client_capabilities %||% list()

    # 350ms deferred roots sync per TS reference (server/roots.ts).
    if (!is.null(caps$roots)) {
      later::later(function() {
        roots <- tryCatch(
          mcpserver:::call_client_blocking(
            session, "roots/list", NULL, 5),
          error = function(e) NULL)
        if (!is.null(roots)) {
          mcp$extension_state$roots_cache <- roots$roots
          mcpserver:::send_log(session, "info",
            sprintf("syncRoots: %d roots cached",
                    length(roots$roots %||% list())))
        }
      }, delay = 0.35)
    }

    if (!is.null(caps$sampling)) {
      mcpserver::add_capability(mcp, mcpserver::new_tool(
        name = "trigger-sampling-request",
        description = "Asks the client to sample an LLM completion.",
        input_schema = mcpserver::schema(list(
          prompt = mcpserver::property_string(
            "Prompt to send to the LLM", required = TRUE),
          maxTokens = mcpserver::property_integer(
            "Maximum tokens", default = 100L),
          systemPrompt = mcpserver::property_string(
            "Optional system prompt"),
          temperature = mcpserver::property_number(
            "Sampling temperature", default = 0.7,
            minimum = 0, maximum = 2)
        )),
        annotations = list(title = "Trigger Sampling Request",
                           readOnlyHint = TRUE, openWorldHint = TRUE),
        bidirectional = TRUE,
        handler = function(args, ctx) {
          res <- tryCatch(ctx$request_sampling(
            messages = list(list(
              role = "user",
              content = list(type = "text", text = args$prompt))),
            system_prompt = args$systemPrompt,
            max_tokens = as.integer(args$maxTokens %||% 100L),
            timeout = 30),
            error = function(e) conditionMessage(e))
          if (is.character(res)) {
            return(mcpserver::response_error(
              paste("sampling request failed:", res)))
          }
          mcpserver::response_text(jsonlite::toJSON(
            res, auto_unbox = TRUE, force = TRUE, pretty = TRUE))
        }
      ))
    }

    if (!is.null(caps$elicitation)) {
      mcpserver::add_capability(mcp, mcpserver::new_tool(
        name = "trigger-elicitation-request",
        description = "Asks the client to elicit structured input from the user.",
        input_schema = mcpserver::schema(list(
          message = mcpserver::property_string(
            "Question to ask the user", required = TRUE)
        )),
        annotations = list(title = "Trigger Elicitation Request",
                           readOnlyHint = TRUE, openWorldHint = TRUE),
        bidirectional = TRUE,
        handler = function(args, ctx) {
          res <- tryCatch(ctx$request_elicitation(
            message = args$message,
            requested_schema = mcpserver::schema(list(
              answer = mcpserver::property_string(
                "Free-text answer", required = TRUE),
              confidence = mcpserver::property_number(
                "Confidence 0..1", minimum = 0, maximum = 1)
            )),
            timeout = 30),
            error = function(e) conditionMessage(e))
          if (is.character(res)) {
            return(mcpserver::response_error(
              paste("elicitation request failed:", res)))
          }
          action <- res$action %||% "accept"
          if (identical(action, "decline")) {
            return(mcpserver::response_text("User declined."))
          }
          if (identical(action, "cancel")) {
            return(mcpserver::response_text("User cancelled."))
          }
          ans <- res$content$answer %||% res$answer %||% ""
          mcpserver::response_text(sprintf("You said: %s", ans))
        }
      ))
    }

    # SEP-1686 client-task requests. Server-side trigger tools that
    # send the matching request with `params.task = { ttl }` and then
    # drive the polling round-trip through `ctx$request_*_async()`.
    if (!is.null(caps$tasks$requests$sampling$createMessage) &&
        !is.null(caps$sampling)) {
      mcpserver::add_capability(mcp, mcpserver::new_tool(
        name = "trigger-sampling-request-async",
        description = paste(
          "Asks the client to sample an LLM completion using the",
          "task-augmented (SEP-1686) flow: the server sends",
          "sampling/createMessage with params.task = { ttl } and",
          "polls tasks/get until the client reports completion."),
        input_schema = mcpserver::schema(list(
          prompt = mcpserver::property_string(
            "Prompt to send to the LLM", required = TRUE),
          maxTokens = mcpserver::property_integer(
            "Maximum tokens", default = 100L),
          systemPrompt = mcpserver::property_string(
            "Optional system prompt"),
          ttl = mcpserver::property_integer(
            "Task retention window in seconds", default = 30L),
          pollInterval = mcpserver::property_number(
            "Polling interval in seconds", default = 0.25)
        )),
        annotations = list(title = "Trigger Sampling Request (Async)",
                           readOnlyHint = TRUE, openWorldHint = TRUE),
        bidirectional = TRUE,
        handler = function(args, ctx) {
          res <- tryCatch(ctx$request_sampling_async(
            messages = list(list(
              role = "user",
              content = list(type = "text", text = args$prompt))),
            system_prompt = args$systemPrompt,
            max_tokens = as.integer(args$maxTokens %||% 100L),
            ttl = as.integer(args$ttl %||% 30L),
            poll_interval = as.numeric(args$pollInterval %||% 0.25),
            total_timeout = max(60,
              as.integer(args$ttl %||% 30L) * 2L)),
            error = function(e) conditionMessage(e))
          if (is.character(res)) {
            return(mcpserver::response_error(
              paste("async sampling request failed:", res)))
          }
          mcpserver::response_text(jsonlite::toJSON(
            res, auto_unbox = TRUE, force = TRUE, pretty = TRUE))
        }
      ))
    }

    if (!is.null(caps$tasks$requests$elicitation$create) &&
        !is.null(caps$elicitation)) {
      mcpserver::add_capability(mcp, mcpserver::new_tool(
        name = "trigger-elicitation-request-async",
        description = paste(
          "Asks the client to elicit structured input using the",
          "task-augmented (SEP-1686) flow."),
        input_schema = mcpserver::schema(list(
          message = mcpserver::property_string(
            "Question to ask the user", required = TRUE),
          ttl = mcpserver::property_integer(
            "Task retention window in seconds", default = 30L),
          pollInterval = mcpserver::property_number(
            "Polling interval in seconds", default = 0.25)
        )),
        annotations = list(title = "Trigger Elicitation Request (Async)",
                           readOnlyHint = TRUE, openWorldHint = TRUE),
        bidirectional = TRUE,
        handler = function(args, ctx) {
          res <- tryCatch(ctx$request_elicitation_async(
            message = args$message,
            requested_schema = mcpserver::schema(list(
              answer = mcpserver::property_string(
                "Free-text answer", required = TRUE),
              confidence = mcpserver::property_number(
                "Confidence 0..1", minimum = 0, maximum = 1)
            )),
            ttl = as.integer(args$ttl %||% 30L),
            poll_interval = as.numeric(args$pollInterval %||% 0.25),
            total_timeout = max(60,
              as.integer(args$ttl %||% 30L) * 2L)),
            error = function(e) conditionMessage(e))
          if (is.character(res)) {
            return(mcpserver::response_error(
              paste("async elicitation request failed:", res)))
          }
          action <- res$action %||% "accept"
          if (identical(action, "decline")) {
            return(mcpserver::response_text("User declined."))
          }
          if (identical(action, "cancel")) {
            return(mcpserver::response_text("User cancelled."))
          }
          ans <- res$content$answer %||% res$answer %||% ""
          mcpserver::response_text(sprintf("You said: %s", ans))
        }
      ))
    }

    if (!is.null(caps$roots)) {
      mcpserver::add_capability(mcp, mcpserver::new_tool(
        name = "get-roots-list",
        description = "Returns the cached client roots list (synced 350ms after init).",
        input_schema = mcpserver::schema(list()),
        annotations = list(title = "Get Roots List",
                           readOnlyHint = TRUE, openWorldHint = FALSE),
        bidirectional = TRUE,
        handler = function(args, ctx) {
          cached <- mcp$extension_state$roots_cache
          if (length(cached) == 0L) {
            res <- tryCatch(ctx$request_roots(timeout = 5),
                            error = function(e) conditionMessage(e))
            if (is.character(res)) {
              return(mcpserver::response_error(
                paste("roots request failed:", res)))
            }
            cached <- res
            mcp$extension_state$roots_cache <- cached
          }
          if (length(cached) == 0L) {
            return(mcpserver::response_text(
              "Client supports roots but reported none."))
          }
          lines <- vapply(cached, function(r) {
            sprintf(" - %s%s", r$uri,
                    if (!is.null(r$name))
                      sprintf(" (%s)", r$name) else "")
          }, character(1L))
          mcpserver::response_text(paste(c("Roots:", lines),
                                         collapse = "\n"))
        }
      ))
    }
  })

  # ----- Resources -----------------------------------------------------

  # Static documentation resources discovered from the docs directory.
  if (nzchar(docs_dir) && dir.exists(docs_dir)) {
    for (doc in list.files(docs_dir, full.names = TRUE, recursive = FALSE)) {
      base <- basename(doc)
      uri <- sprintf("demo://resource/static/document/%s",
                     utils::URLencode(base, reserved = TRUE))
      mime <- .mime_for_ext(doc)
      local({
        path <- doc; mime_local <- mime; base_local <- base
        mcpserver::add_capability(srv, mcpserver::new_resource(
          name = base_local,
          description = sprintf("Static documentation file: %s", base_local),
          uri = uri,
          mime_type = mime_local,
          handler = function(params, ctx) {
            list(text = paste(readLines(path, warn = FALSE),
                              collapse = "\n"),
                 mimeType = mime_local)
          }))
      })
    }
  }

  # Two templated dynamic resource families (text + blob).
  mcpserver::add_capability(srv, mcpserver::new_resource_template(
    name = "dynamic-text",
    description = "Dynamically generated text resources",
    uri_template = "demo://resource/dynamic/text/{id}",
    mime_type = "text/plain",
    complete = list(
      id = function(value, ctx, args) {
        if (grepl("^[0-9]+$", value) && as.integer(value) > 0L) {
          return(value)
        }
        character(0L)
      }
    ),
    handler = function(params, ctx) {
      sprintf("Resource #%s body.", params$variables$id)
    }
  ))
  mcpserver::add_capability(srv, mcpserver::new_resource_template(
    name = "dynamic-blob",
    description = "Dynamically generated binary resources",
    uri_template = "demo://resource/dynamic/blob/{id}",
    mime_type = "application/octet-stream",
    complete = list(
      id = function(value, ctx, args) {
        if (grepl("^[0-9]+$", value) && as.integer(value) > 0L) {
          return(value)
        }
        character(0L)
      }
    ),
    handler = function(params, ctx) {
      list(blob = charToRaw(sprintf("blob-%s", params$variables$id)))
    }
  ))

  # ----- Prompts -------------------------------------------------------

  mcpserver::add_capability(srv, mcpserver::new_prompt(
    name = "simple-prompt",
    description = "A simple prompt without arguments.",
    arguments = list(),
    handler = function(args, ctx)
      "This is a simple prompt without arguments."
  ))

  mcpserver::add_capability(srv, mcpserver::new_prompt(
    name = "args-prompt",
    description = "Prompt asking for the weather in a city.",
    arguments = list(
      mcpserver::new_prompt_argument("city", "City name", required = TRUE),
      mcpserver::new_prompt_argument("state", "Optional state code")
    ),
    handler = function(args, ctx) {
      loc <- args$city
      if (!is.null(args$state)) {
        loc <- sprintf("%s, %s", args$city, args$state)
      }
      sprintf("What's the weather in %s?", loc)
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_prompt(
    name = "completable-prompt",
    description = "Prompt exercising cascading argument completions.",
    arguments = list(
      mcpserver::new_prompt_argument("department", required = TRUE),
      mcpserver::new_prompt_argument("name", required = TRUE)
    ),
    complete = list(
      department = function(value, ctx, args) {
        opts <- c("Engineering", "Sales", "Marketing", "Support")
        grep(value, opts, value = TRUE, ignore.case = TRUE)
      },
      name = function(value, ctx, args) {
        dept <- args$department %||% ""
        opts <- switch(dept,
          Engineering = c("Alice", "Bob", "Charlie"),
          Sales       = c("David", "Eve", "Frank"),
          Marketing   = c("Grace", "Henry", "Iris"),
          Support     = c("John", "Kim", "Lee"),
          c())
        grep(value, opts, value = TRUE, ignore.case = TRUE)
      }
    ),
    handler = function(args, ctx) {
      sprintf("Compose a message from %s in %s.",
              args$name, args$department)
    }
  ))

  mcpserver::add_capability(srv, mcpserver::new_prompt(
    name = "resource-prompt",
    description = "Prompt that embeds a dynamic resource.",
    arguments = list(
      mcpserver::new_prompt_argument("resourceType", required = TRUE),
      mcpserver::new_prompt_argument("resourceId", required = TRUE)
    ),
    complete = list(
      resourceType = function(value, ctx, args) {
        opts <- c("Text", "Blob")
        grep(value, opts, value = TRUE, ignore.case = TRUE)
      },
      resourceId = function(value, ctx, args) {
        if (grepl("^[0-9]+$", value) && as.integer(value) > 0L) {
          value
        } else character(0L)
      }
    ),
    handler = function(args, ctx) {
      kind <- tolower(args$resourceType)
      uri <- sprintf("demo://resource/dynamic/%s/%s",
                     kind, args$resourceId)
      embedded <- if (identical(kind, "blob")) {
        mcpserver::response_resource(
          uri, blob = charToRaw(sprintf("blob-%s", args$resourceId)),
          mime_type = "application/octet-stream")
      } else {
        mcpserver::response_resource(
          uri, text = sprintf("Resource #%s body.", args$resourceId),
          mime_type = "text/plain")
      }
      list(
        description = "Embedded resource prompt",
        messages = list(
          list(role = "user",
               content = mcpserver::response_text(
                 sprintf("Inspect resource %s (%s):",
                         args$resourceId, args$resourceType))),
          list(role = "user", content = embedded)
        )
      )
    }
  ))

  srv
}
