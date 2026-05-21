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

# Fetch the payload `gzip-file-as-resource` will compress. Accepts:
#   - http(s)://URL  — fetched with httr2, env-var gated size + time +
#                       allowed-domains. Fails closed on disallowed
#                       schemes (ftp, file, javascript, ...).
#   - data:<mime>;base64,<payload> — decoded in-process.
#   - any other string — treated as inline content.
gzip_fetch_payload <- function(data) {
  if (is.null(data) || identical(data, "")) {
    stop("'data' is required")
  }
  if (grepl("^data:", data, perl = TRUE)) {
    m <- regmatches(data, regexec(
      "^data:([^;,]*)(?:;([^,]*))?,(.*)$", data))[[1L]]
    if (length(m) < 4L) stop("malformed data: URI")
    payload <- m[[4L]]
    if (identical(m[[3L]], "base64")) {
      return(list(bytes = jsonlite::base64_dec(payload)))
    }
    return(list(bytes = charToRaw(utils::URLdecode(payload))))
  }
  if (grepl("^https?://", data, ignore.case = TRUE)) {
    max_size <- as.integer(Sys.getenv(
      "MCPSERVER_GZIP_MAX_FETCH_SIZE", "10485760"))
    max_time_ms <- as.integer(Sys.getenv(
      "MCPSERVER_GZIP_MAX_FETCH_TIME_MILLIS", "30000"))
    allowed <- strsplit(Sys.getenv("MCPSERVER_GZIP_ALLOWED_DOMAINS",
                                   ""), ",", fixed = TRUE)[[1L]]
    allowed <- allowed[nzchar(allowed)]
    if (length(allowed) > 0L) {
      host <- sub("^https?://", "", data, ignore.case = TRUE)
      host <- sub("[/:?#].*$", "", host)
      if (!any(vapply(allowed,
                      function(a) identical(tolower(host),
                                            tolower(a)),
                      logical(1L)))) {
        stop(sprintf(
          "host '%s' not in MCPSERVER_GZIP_ALLOWED_DOMAINS", host))
      }
    }
    resp <- httr2::request(data) |>
      httr2::req_timeout(max(1, max_time_ms / 1000)) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) >= 400L) {
      stop(sprintf("HTTP %d from %s",
                   httr2::resp_status(resp), data))
    }
    raw <- httr2::resp_body_raw(resp)
    if (length(raw) > max_size) {
      stop(sprintf("response exceeded MCPSERVER_GZIP_MAX_FETCH_SIZE = %d",
                   max_size))
    }
    return(list(bytes = raw))
  }
  if (grepl("^[a-z][a-z0-9+.-]*://", data, ignore.case = TRUE)) {
    stop(sprintf("unsupported URL scheme in '%s'", data))
  }
  # Plain inline content (backwards compatible with the pre-Phase-D
  # signature that took a `content` string).
  list(bytes = charToRaw(data))
}

# Topic-aware disambiguation options for simulate-research-query. Mirrors
# the TS everything-server's `getInterpretationsForTopic`.
topic_interpretations <- function(topic) {
  t <- tolower(as.character(topic %||% ""))
  if (identical(t, "python")) {
    return(list(
      list(const = "programming",
           title = "Python programming language"),
      list(const = "snake",
           title = "Python (the snake)"),
      list(const = "comedy",
           title = "Monty Python")))
  }
  if (identical(t, "java")) {
    return(list(
      list(const = "programming",
           title = "Java programming language"),
      list(const = "island", title = "Java (the island)"),
      list(const = "coffee", title = "Java (coffee)")))
  }
  list(
    list(const = "technical",
         title = sprintf("Technical aspects of %s", topic)),
    list(const = "historical",
         title = sprintf("Historical context of %s", topic)),
    list(const = "current",
         title = sprintf("Current developments in %s", topic)))
}

build_everything_server <- function() {
  docs_dir <- system.file("everything", "docs", package = "mcpserver")
  if (!nzchar(docs_dir)) docs_dir <- file.path("inst", "everything", "docs")
  tiny_image_src <- system.file("everything", "tiny-image.R",
                                package = "mcpserver")
  if (!nzchar(tiny_image_src)) {
    tiny_image_src <- file.path("inst", "everything", "tiny-image.R")
  }
  if (file.exists(tiny_image_src)) source(tiny_image_src, local = TRUE)

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
      list(content = list(
        mcpserver::response_text("This is a tiny image:"),
        mcpserver::response_image(MCP_TINY_IMAGE, "image/png"),
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
        blocks <- c(blocks, list(mcpserver::response_image(
          MCP_TINY_IMAGE, "image/png",
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
    description = paste("Gzips the provided data and exposes it as a",
                        "session-scoped resource. Accepts inline content,",
                        "a `data:` URI, or an `http(s)://` URL."),
    input_schema = mcpserver::schema(list(
      data = mcpserver::property_string(
        "Content to gzip, a data: URI, or an http(s):// URL.",
        required = TRUE),
      name = mcpserver::property_string(
        "Resource name", default = "archive.gz"),
      outputType = mcpserver::property_enum(
        c("resource", "resource_link"),
        description = "Return the bytes inline or just a link",
        default = "resource_link")
    )),
    annotations = list(title = "Gzip File as Resource",
                       readOnlyHint = FALSE, openWorldHint = TRUE),
    bidirectional = TRUE,
    handler = function(args, ctx) {
      payload <- tryCatch(
        gzip_fetch_payload(args$data),
        error = function(e) list(.err = conditionMessage(e)))
      if (!is.null(payload$.err)) {
        return(mcpserver::response_error(
          paste("gzip-file-as-resource fetch failed:", payload$.err)))
      }
      tmp <- tempfile(fileext = ".gz")
      on.exit(unlink(tmp), add = TRUE)
      con <- gzfile(tmp, open = "wb")
      writeBin(payload$bytes, con)
      close(con)
      bytes <- readBin(tmp, what = "raw", n = file.info(tmp)$size)
      name <- args$name %||% "archive.gz"
      uri <- sprintf("demo://resource/session/%s",
                     utils::URLencode(name, reserved = TRUE))
      # Register the gzipped bytes as a session-scoped resource so
      # the resource_link returned below resolves on subsequent
      # resources/read calls within this session.
      mcpserver:::session_resource_register(
        ctx$.session,
        uri = uri,
        mime_type = "application/gzip",
        name = name,
        description = sprintf("Gzipped archive: %s", name),
        handler = function(params, ctx_inner) {
          list(blob = bytes, mimeType = "application/gzip")
        })
      if (identical(args$outputType, "resource_link")) {
        mcpserver::response_resource_link(
          uri, name = name,
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
                                          minimum = 1L, maximum = 10L),
      ambiguous = mcpserver::property_boolean(
        "When TRUE, mid-task elicit topic interpretation",
        default = FALSE)
    )),
    annotations = list(title = "Simulate Research Query",
                       readOnlyHint = TRUE, openWorldHint = FALSE),
    tasks = TRUE,
    # Bidirectional so the handler runs on the transport thread:
    # task store mutations propagate live (daemons can't update the
    # parent's store) and ctx$request_elicitation can drive the
    # pending-request table.
    bidirectional = TRUE,
    handler = function(args, ctx) {
      stages <- as.integer(args$steps %||% 3L)
      interpretation <- NULL
      ctx$task$append_message(list(
        type = "text",
        text = sprintf("research started on '%s' (%d stages)",
                       args$topic, stages)))
      report_lines <- character(0L)
      for (i in seq_len(stages)) {
        if (isTRUE(ctx$task$cancelled())) {
          return(mcpserver::response_error("research cancelled"))
        }
        # SEP-1686 input_required path: half-way through, if the
        # client opted in to elicitation, flip the task to
        # input_required and ask the client to disambiguate the
        # topic. Per TS reference, only fires when args$ambiguous.
        if (i == max(1L, ceiling(stages / 2L)) &&
            isTRUE(args$ambiguous) && is.null(interpretation) &&
            !is.null(ctx$client_capabilities$elicitation)) {
          ctx$task$update_status("input_required")
          options <- topic_interpretations(args$topic)
          choice <- tryCatch(ctx$request_elicitation(
            message = sprintf(
              "Topic '%s' is ambiguous. Pick an interpretation:",
              args$topic),
            requested_schema = list(
              type = "object",
              properties = list(
                interpretation = list(
                  type = "string",
                  oneOf = options)),
              required = list("interpretation")),
            timeout = 30),
            error = function(e) NULL)
          ctx$task$update_status("running")
          if (!is.null(choice) &&
              identical(choice$action %||% "accept", "accept")) {
            interpretation <- choice$content$interpretation %||% NULL
            ctx$task$append_message(list(
              type = "text",
              text = sprintf("user picked interpretation: %s",
                             interpretation)))
          } else {
            ctx$task$append_message(list(
              type = "text",
              text = "elicitation declined/unavailable; using default"))
          }
        }
        line <- sprintf("stage %d/%d: surveyed '%s'%s",
                        i, stages, args$topic,
                        if (!is.null(interpretation)) {
                          sprintf(" (%s)", interpretation)
                        } else "")
        ctx$task$append_message(list(type = "text", text = line))
        if (!is.null(ctx$progress_token)) {
          ctx$send_progress(i, total = stages,
                            message = sprintf("stage %d", i))
        }
        report_lines <- c(report_lines, line)
        Sys.sleep(0.05)
      }
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
      # Kitchen-sink schema matching the TS everything-server's
      # trigger-elicitation-request: every primitive, every enum
      # variant, format validators. Only `name` is required; the
      # rest carry defaults or are optional.
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
          requested <- list(
            type = "object",
            properties = list(
              name = list(type = "string",
                          description = "Your name"),
              check = list(type = "boolean",
                           description = "I agree to the terms"),
              firstLine = list(type = "string",
                               description = "Optional opening line",
                               default = "Hello, world!"),
              email = list(type = "string", format = "email",
                           description = "Contact email"),
              homepage = list(type = "string", format = "uri",
                              description = "Personal homepage"),
              birthdate = list(type = "string", format = "date",
                               description = "Date of birth"),
              integer = list(type = "integer",
                             description = "Lucky integer",
                             minimum = 1L, maximum = 100L,
                             default = 42L),
              number = list(type = "number",
                            description = "Favourite number",
                            minimum = 0, maximum = 1000,
                            default = 3.14),
              untitledSingleSelectEnum = list(
                type = "string",
                description = "Pick a Friend",
                enum = I(c("Rachel", "Monica", "Phoebe",
                           "Joey", "Chandler", "Ross"))),
              untitledMultipleSelectEnum = list(
                type = "array",
                description = "Pick up to 3 instruments",
                items = list(type = "string",
                             enum = I(c("Guitar", "Piano", "Violin",
                                        "Drums", "Bass")))),
              titledSingleSelectEnum = list(
                type = "string",
                description = "Pick a level",
                oneOf = list(
                  list(const = "beginner", title = "Beginner"),
                  list(const = "intermediate", title = "Intermediate"),
                  list(const = "advanced", title = "Advanced"))),
              titledMultipleSelectEnum = list(
                type = "array",
                description = "Pick interests",
                items = list(anyOf = list(
                  list(const = "ai", title = "Artificial Intelligence"),
                  list(const = "ml", title = "Machine Learning"),
                  list(const = "stats", title = "Statistics")))),
              legacyTitledEnum = list(
                type = "string",
                description = "Pick using legacy titles",
                enum = I(c("red", "green", "blue")),
                enumNames = I(c("Red", "Green", "Blue")))
            ),
            required = list("name")
          )
          res <- tryCatch(ctx$request_elicitation(
            message = args$message,
            requested_schema = requested,
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
          # Pretty-print whatever the user supplied (defaults are
          # applied by validate_elicit_response on the server side).
          content <- res$content %||% list()
          lines <- vapply(names(content), function(k) {
            v <- content[[k]]
            if (is.list(v) || length(v) > 1L) {
              v <- paste(unlist(v), collapse = ", ")
            }
            sprintf(" - %s: %s", k, as.character(v))
          }, character(1L))
          mcpserver::response_text(paste(c(
            sprintf("Elicitation accepted. Collected %d fields:",
                    length(lines)),
            lines), collapse = "\n"))
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
