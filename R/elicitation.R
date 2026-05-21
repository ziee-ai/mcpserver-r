# Server-to-client request: elicitation/create ----------------------------

request_elicitation_impl <- function(session, message,
                                     requested_schema,
                                     timeout = 30) {
  caps <- session$client_capabilities %||% list()
  if (is.null(caps$elicitation)) {
    stop("client did not declare elicitation capability")
  }
  res <- call_client_blocking(session, "elicitation/create",
                              list(message = message,
                                   requestedSchema = requested_schema),
                              timeout)
  # Validate the action + content against the requested schema before
  # returning to the handler.
  validated <- validate_elicit_response(requested_schema, res)
  if (!isTRUE(validated$ok)) {
    stop(sprintf("elicitation response invalid: %s",
                 paste(validated$errors, collapse = "; ")))
  }
  validated$value
}

# Validate an `ElicitResult` against the schema we sent in the request.
# Supports:
#   * SEP-1034 defaults: if a field is missing and the schema declares
#     `default`, the default is applied to the returned content.
#   * SEP-1330 enum shapes: untitled/titled single, legacy enumNames,
#     untitled/titled multi.
validate_elicit_response <- function(requested_schema, res) {
  action <- res$action %||% "accept"
  if (action %in% c("decline", "cancel")) {
    return(list(ok = TRUE, value = res, errors = character(0L)))
  }
  if (is.null(res$content) || !is.list(res$content)) {
    return(list(ok = FALSE,
                errors = "accept response missing content object"))
  }
  props <- requested_schema$properties %||% list()
  required <- as.character(requested_schema$required %||%
                             character(0L))
  errors <- character(0L)
  filled <- res$content
  for (name in names(props)) {
    p <- props[[name]]
    if (is.null(filled[[name]])) {
      if (!is.null(p$default)) {
        # SEP-1034: apply the schema's default.
        filled[[name]] <- p$default
      } else if (name %in% required) {
        errors <- c(errors,
                    sprintf("missing required field '%s'", name))
      }
      next
    }
    err <- validate_elicit_field(name, p, filled[[name]])
    if (length(err) > 0L) errors <- c(errors, err)
  }
  if (length(errors) > 0L) {
    return(list(ok = FALSE, errors = errors))
  }
  res$content <- filled
  list(ok = TRUE, value = res, errors = character(0L))
}

validate_elicit_field <- function(name, schema, value) {
  errors <- character(0L)
  # Basic type checks.
  type <- schema$type %||% NULL
  if (!is.null(type)) {
    if (identical(type, "string") && !is.character(value)) {
      errors <- c(errors,
                  sprintf("'%s' must be a string", name))
    } else if (identical(type, "integer") &&
               !(is.numeric(value) &&
                 all(value == as.integer(value)))) {
      errors <- c(errors,
                  sprintf("'%s' must be an integer", name))
    } else if (identical(type, "number") && !is.numeric(value)) {
      errors <- c(errors,
                  sprintf("'%s' must be a number", name))
    } else if (identical(type, "boolean") && !is.logical(value)) {
      errors <- c(errors,
                  sprintf("'%s' must be a boolean", name))
    } else if (identical(type, "array") && !is.list(value) &&
               !is.atomic(value)) {
      errors <- c(errors,
                  sprintf("'%s' must be an array", name))
    }
  }
  # Format validators (SEP-1034 listed formats).
  if (!is.null(schema$format) && is.character(value)) {
    case <- schema$format
    pattern <- switch(case,
      "email" = "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
      "uri"   = "^[A-Za-z][A-Za-z0-9+.-]*://",
      "date"  = "^\\d{4}-\\d{2}-\\d{2}$",
      "date-time" =
        "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}",
      NULL)
    if (!is.null(pattern) && !grepl(pattern, value)) {
      errors <- c(errors,
                  sprintf("'%s' does not match format '%s'",
                          name, case))
    }
  }
  # Bounds.
  if (!is.null(schema$minimum) && is.numeric(value) &&
      any(value < schema$minimum)) {
    errors <- c(errors,
                sprintf("'%s' below minimum %g", name, schema$minimum))
  }
  if (!is.null(schema$maximum) && is.numeric(value) &&
      any(value > schema$maximum)) {
    errors <- c(errors,
                sprintf("'%s' above maximum %g", name, schema$maximum))
  }
  if (!is.null(schema$minLength) && is.character(value) &&
      any(nchar(value) < schema$minLength)) {
    errors <- c(errors,
                sprintf("'%s' shorter than minLength %d",
                        name, schema$minLength))
  }
  if (!is.null(schema$maxLength) && is.character(value) &&
      any(nchar(value) > schema$maxLength)) {
    errors <- c(errors,
                sprintf("'%s' longer than maxLength %d",
                        name, schema$maxLength))
  }
  # SEP-1330 enum shapes.
  if (!is.null(schema$enum)) {
    allowed <- as.character(unlist(schema$enum))
    if (!all(as.character(value) %in% allowed)) {
      errors <- c(errors,
                  sprintf("'%s' must be one of: %s",
                          name, paste(allowed, collapse = ", ")))
    }
  }
  if (!is.null(schema$oneOf)) {
    consts <- vapply(schema$oneOf,
                     function(c) as.character(c$const %||% ""),
                     character(1L))
    if (!as.character(value) %in% consts) {
      errors <- c(errors,
                  sprintf("'%s' must match one of: %s",
                          name, paste(consts, collapse = ", ")))
    }
  }
  if (identical(schema$type, "array") && !is.null(schema$items)) {
    items <- schema$items
    vals <- if (is.list(value)) unlist(value) else value
    if (!is.null(items$enum)) {
      allowed <- as.character(unlist(items$enum))
      if (!all(as.character(vals) %in% allowed)) {
        errors <- c(errors,
                    sprintf("'%s' items must be subset of: %s",
                            name, paste(allowed, collapse = ", ")))
      }
    }
    if (!is.null(items$anyOf)) {
      consts <- vapply(items$anyOf,
                       function(c) as.character(c$const %||% ""),
                       character(1L))
      if (!all(as.character(vals) %in% consts)) {
        errors <- c(errors,
                    sprintf("'%s' items must be subset of: %s",
                            name, paste(consts, collapse = ", ")))
      }
    }
  }
  errors
}
