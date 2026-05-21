# JSON Schema builders + validation ---------------------------------------

#' Build a JSON Schema object
#'
#' Constructs a draft-07 JSON Schema describing tool input arguments. The
#' resulting list is shaped to be serialised directly into the MCP
#' `inputSchema` (or `outputSchema`) field of a tool descriptor.
#'
#' Property declarations are supplied as a named list; the property name
#' becomes the JSON Schema key. A property is required either when its
#' helper sets `required = TRUE` or when its name appears in the `required`
#' argument to `schema()`.
#'
#' @param properties Named list of property descriptors built with
#'   [property_string()], [property_number()], etc.
#' @param type Schema type (default `"object"`).
#' @param additional_properties Whether properties not listed are allowed
#'   (default `FALSE`).
#' @param required Optional character vector of required property names.
#'   Properties built with `required = TRUE` are also added.
#' @return A list shaped as a JSON Schema object.
#' @export
#' @examples
#' schema(
#'   properties = list(
#'     name = property_string("User name", required = TRUE),
#'     age  = property_integer("Age", minimum = 0)
#'   )
#' )
schema <- function(properties = list(),
                   type = "object",
                   additional_properties = FALSE,
                   required = NULL) {
  stopifnot(is.list(properties))
  req <- character(0L)
  cleaned <- list()
  for (nm in names(properties)) {
    p <- properties[[nm]]
    if (isTRUE(p$.required)) req <- c(req, nm)
    p$.required <- NULL
    cleaned[[nm]] <- p
  }
  req <- unique(c(req, as.character(required)))
  # Force properties to serialise as `{}` rather than `[]` when empty.
  out <- list(type = type,
              properties = if (length(cleaned) == 0L)
                j_empty_obj() else cleaned)
  out$additionalProperties <- isTRUE(additional_properties)
  # Force `required` to serialise as a JSON array even when length 1.
  if (length(req) > 0L) out$required <- I(req)
  out
}

property_common <- function(type, description, default,
                            extras = list(), required = FALSE) {
  out <- list(type = type)
  if (!is.null(description)) out$description <- description
  if (!is.null(default))     out$default     <- default
  array_fields <- c("enum", "required")
  for (k in names(extras)) {
    v <- extras[[k]]
    if (is.null(v)) next
    if (k %in% array_fields && is.atomic(v)) {
      out[[k]] <- I(v)
    } else {
      out[[k]] <- v
    }
  }
  out$.required <- isTRUE(required)
  out
}

#' Build a string property descriptor
#'
#' @param description Optional human-readable description.
#' @param enum Optional character vector restricting allowed values.
#' @param default Optional default value.
#' @param format Optional JSON Schema string format (e.g. `"email"`).
#' @param min_length,max_length Optional length bounds.
#' @param pattern Optional regular expression the value must match.
#' @param required Whether this property is required (default `FALSE`).
#' @return A property descriptor list.
#' @export
#' @examples
#' property_string("Username", min_length = 1, required = TRUE)
property_string <- function(description = NULL, enum = NULL, default = NULL,
                            format = NULL, min_length = NULL,
                            max_length = NULL, pattern = NULL,
                            required = FALSE) {
  property_common("string", description, default,
                  list(enum = enum, format = format,
                       minLength = min_length, maxLength = max_length,
                       pattern = pattern),
                  required = required)
}

#' Build a numeric (floating-point) property descriptor
#'
#' @param description Optional description.
#' @param minimum,maximum Optional value bounds.
#' @param default Optional default value.
#' @param required Whether this property is required.
#' @return A property descriptor list.
#' @export
#' @examples
#' property_number("Temperature", minimum = 0)
property_number <- function(description = NULL, minimum = NULL,
                            maximum = NULL, default = NULL,
                            required = FALSE) {
  property_common("number", description, default,
                  list(minimum = minimum, maximum = maximum),
                  required = required)
}

#' Build an integer property descriptor
#'
#' @param description Optional description.
#' @param minimum,maximum Optional value bounds.
#' @param default Optional default value.
#' @param required Whether this property is required.
#' @return A property descriptor list.
#' @export
#' @examples
#' property_integer("Count", minimum = 0L)
property_integer <- function(description = NULL, minimum = NULL,
                             maximum = NULL, default = NULL,
                             required = FALSE) {
  property_common("integer", description, default,
                  list(minimum = minimum, maximum = maximum),
                  required = required)
}

#' Build a boolean property descriptor
#'
#' @param description Optional description.
#' @param default Optional default value.
#' @param required Whether this property is required.
#' @return A property descriptor list.
#' @export
#' @examples
#' property_boolean("Verbose")
property_boolean <- function(description = NULL, default = NULL,
                             required = FALSE) {
  property_common("boolean", description, default,
                  list(), required = required)
}

#' Build an array property descriptor
#'
#' @param items Item schema (built with another `property_*()`).
#' @param description Optional description.
#' @param min_items,max_items Optional length bounds.
#' @param required Whether this property is required.
#' @return A property descriptor list.
#' @export
#' @examples
#' property_array(property_string(), description = "Tags")
property_array <- function(items, description = NULL,
                           min_items = NULL, max_items = NULL,
                           required = FALSE) {
  items_clean <- items
  items_clean$.required <- NULL
  property_common("array", description, NULL,
                  list(items = items_clean,
                       minItems = min_items, maxItems = max_items),
                  required = required)
}

#' Build an object property descriptor
#'
#' @param properties Named list of property descriptors.
#' @param description Optional description.
#' @param additional_properties Whether unlisted keys are allowed (default
#'   `FALSE`).
#' @param required Whether this property itself is required.
#' @return A property descriptor list.
#' @export
#' @examples
#' property_object(list(name = property_string()))
property_object <- function(properties = list(), description = NULL,
                            additional_properties = FALSE,
                            required = FALSE) {
  inner <- schema(properties = properties,
                  additional_properties = additional_properties)
  inner$description <- description
  inner$.required <- isTRUE(required)
  inner
}

#' Build an enum string property descriptor
#'
#' @param values Character vector of allowed values.
#' @param description Optional description.
#' @param default Optional default value.
#' @param required Whether this property is required.
#' @return A property descriptor list.
#' @export
#' @examples
#' property_enum(c("low", "high"), default = "low")
property_enum <- function(values, description = NULL, default = NULL,
                          required = FALSE) {
  stopifnot(is.character(values), length(values) >= 1L)
  property_string(description = description, enum = values,
                  default = default, required = required)
}

# Schema validation -------------------------------------------------------

# Compile a schema into a jsonvalidate validator (cached on the schema
# itself via an attribute).
compile_validator <- function(sch) {
  v <- attr(sch, "validator")
  if (!is.null(v)) return(v)
  json <- to_json(sch)
  validator <- jsonvalidate::json_validator(json, engine = "ajv")
  attr(sch, "validator") <- validator
  validator
}

# Validate `args` (a list) against `sch`. Returns list(ok, errors) where
# errors is a character vector when ok is FALSE.
validate_args <- function(sch, args) {
  if (is.null(sch)) return(list(ok = TRUE, errors = character(0L)))
  v <- compile_validator(sch)
  payload <- to_json(args %||% list())
  ok <- isTRUE(v(payload))
  if (ok) return(list(ok = TRUE, errors = character(0L)))
  errs <- attr(v(payload, verbose = TRUE), "errors")
  msgs <- if (is.data.frame(errs)) {
    apply(errs, 1L, function(r) paste(r, collapse = ": "))
  } else {
    character(0L)
  }
  list(ok = FALSE, errors = as.character(msgs))
}
