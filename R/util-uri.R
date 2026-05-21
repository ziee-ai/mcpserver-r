# RFC 6570 URI template expansion (level-1 subset) ------------------------
#
# The reference everything server only uses simple `{var}` placeholders; we
# implement that subset and parse a matching URI back to its variables.

# Extract the named placeholders from a template, e.g. "demo://x/{id}".
uri_template_vars <- function(template) {
  regmatches(template, gregexpr("\\{[^}]+\\}", template))[[1L]] |>
    gsub("[{}]", "", x = _)
}

# Expand a URI template against a named list of variable values.
uri_template_expand <- function(template, vars) {
  for (k in names(vars)) {
    template <- sub(paste0("\\{", k, "\\}"),
                    utils::URLencode(as.character(vars[[k]])),
                    template, fixed = FALSE)
  }
  template
}

# Match a concrete URI against a template, returning the variable values as
# a named list or NULL if the URI does not match.
uri_template_match <- function(template, uri) {
  vars <- uri_template_vars(template)
  if (length(vars) == 0L) {
    return(if (identical(template, uri)) list() else NULL)
  }
  pattern <- template
  for (v in vars) {
    pattern <- sub(paste0("\\{", v, "\\}"),
                   sprintf("(?<%s>[^/]+)", v),
                   pattern, fixed = FALSE)
  }
  pattern <- paste0("^", pattern, "$")
  m <- regmatches(uri, regexec(pattern, uri, perl = TRUE))[[1L]]
  if (length(m) == 0L) return(NULL)
  out <- as.list(m[-1L])
  names(out) <- vars
  out
}
