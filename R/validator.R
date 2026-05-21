# Pluggable JSON schema validator.
#
# Default uses jsonvalidate (ajv). Users can swap in their own engine
# by supplying a function-of-schema-returning-function-of-payload
# to new_server(schema_validator = ...).

#' Construct an MCP schema validator
#'
#' Returns a callable: given a JSON Schema list, returns a function
#' that validates a payload and produces `list(ok, errors)`.
#'
#' @param engine `"ajv"` (default, via jsonvalidate) or `"none"`
#'   (accepts every payload — useful for tests and stubs).
#' @return A function `function(schema) -> function(args) -> list(ok, errors)`.
#' @export
#' @examples
#' v <- new_validator()
#' check <- v(schema(list(x = property_integer(required = TRUE))))
#' check(list(x = 42L))
new_validator <- function(engine = c("ajv", "none")) {
  engine <- match.arg(engine)
  if (identical(engine, "none")) {
    function(sch) function(args) list(ok = TRUE, errors = character(0L))
  } else {
    function(sch) {
      function(args) validate_args(sch, args)
    }
  }
}
