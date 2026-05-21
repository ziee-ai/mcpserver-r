# completion/complete handler --------------------------------------------

handle_completion <- function(server, session, params, msg) {
  ref <- params$ref %||% list()
  arg <- params$argument %||% list()
  ctx_args <- params$context$arguments %||% list()
  if (is.null(arg$name)) {
    return(jrpc_error(msg$id, jrpc_codes$invalid_params,
                      "missing argument.name"))
  }
  value <- arg$value %||% ""

  values <- character(0L)
  switch(ref$type %||% "",
    "ref/prompt" = {
      name <- ref$name
      if (!is.null(name) &&
          exists(name, envir = server$prompts, inherits = FALSE)) {
        p <- get(name, envir = server$prompts, inherits = FALSE)
        fn <- (p$complete %||% list())[[arg$name]]
        if (is.function(fn)) {
          values <- as.character(fn(value,
                                    make_ctx(session, msg),
                                    ctx_args))
        }
      }
    },
    "ref/resource" = {
      uri <- ref$uri
      # Find a template by uri or name.
      tpl <- NULL
      if (!is.null(uri) &&
          exists(uri, envir = server$resource_templates, inherits = FALSE)) {
        tpl <- get(uri, envir = server$resource_templates,
                   inherits = FALSE)
      } else {
        for (n in ls(server$resource_templates, all.names = TRUE)) {
          t <- get(n, envir = server$resource_templates,
                   inherits = FALSE)
          if (identical(t$uri_template, uri) || identical(t$name, uri)) {
            tpl <- t; break
          }
        }
      }
      if (!is.null(tpl)) {
        fn <- (tpl$complete %||% list())[[arg$name]]
        if (is.function(fn)) {
          values <- as.character(fn(value,
                                    make_ctx(session, msg),
                                    ctx_args))
        }
      }
    }
  )

  list(completion = list(
    values = j_list(values),
    total = length(values),
    hasMore = FALSE
  ))
}
