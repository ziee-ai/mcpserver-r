# RFC 6570 URI template expansion + matching.
#
# Supports all four levels:
#   Level 1: {var}                — simple, full encoding
#   Level 2: {+var}, {#var}       — reserved / fragment expansion
#   Level 3: {.var}, {/var},      — label / path-segment / path-style
#            {;var}, {?var}, {&var} / query / form-continuation
#   Level 4: {var*}                — explosion; {a,b} multi-var
#
# Mirrors `packages/core/src/shared/uriTemplate.ts` in the TS SDK.

# ----- character classes ----------------------------------------------

# Unreserved per RFC 3986: ALPHA / DIGIT / - . _ ~
.UNRESERVED <- "A-Za-z0-9\\-._~"
# Reserved set per RFC 3986: gen-delims + sub-delims
.RESERVED_ALLOWED <- ":/?#[]@!$&'()*+,;=%"

# Percent-encode a single character that's outside the allowed set.
.pct_encode_char <- function(ch, allowed) {
  if (grepl(allowed, ch, perl = TRUE)) return(ch)
  raw <- charToRaw(ch)
  paste0("%", toupper(format(as.hexmode(as.integer(raw)), width = 2L)),
         collapse = "")
}

# Encode a value for placement in a URI template expansion. `allowed`
# is a character class (without brackets) of bytes to pass through
# unencoded. Everything else is percent-encoded byte-by-byte.
.pct_encode <- function(value, allowed) {
  if (length(value) == 0L || identical(value, "")) return("")
  chars <- strsplit(as.character(value), "", fixed = TRUE)[[1L]]
  cls <- paste0("[", allowed, "]")
  out <- vapply(chars, .pct_encode_char, character(1L), allowed = cls)
  paste0(out, collapse = "")
}

.UNRESERVED_CLASS <- paste0(.UNRESERVED)
.RESERVED_CLASS <- paste0(.UNRESERVED, ":/?#\\[\\]@!\\$&'\\(\\)\\*\\+,;=%")

# ----- parsing -------------------------------------------------------

# Parse a single expression body (without the surrounding {}) into a
# list(operator, vars). Each var is list(name, exploded, prefix_len).
.parse_expression <- function(body) {
  op <- ""
  first <- substr(body, 1L, 1L)
  if (first %in% c("+", "#", ".", "/", ";", "?", "&")) {
    op <- first
    body <- substr(body, 2L, nchar(body))
  }
  parts <- strsplit(body, ",", fixed = TRUE)[[1L]]
  vars <- lapply(parts, function(p) {
    exploded <- FALSE
    prefix_len <- NA_integer_
    if (endsWith(p, "*")) {
      exploded <- TRUE
      p <- substr(p, 1L, nchar(p) - 1L)
    }
    if (grepl(":", p, fixed = TRUE)) {
      sp <- strsplit(p, ":", fixed = TRUE)[[1L]]
      p <- sp[[1L]]
      prefix_len <- as.integer(sp[[2L]])
    }
    list(name = p, exploded = exploded, prefix_len = prefix_len)
  })
  list(operator = op, vars = vars)
}

# Locate all `{...}` expressions in a template and return their
# expression info plus the literal text between them.
.tokenise_template <- function(template) {
  tokens <- list()
  pos <- 1L
  n <- nchar(template)
  while (pos <= n) {
    open <- regexpr("\\{", substr(template, pos, n))
    if (open == -1L) {
      tokens[[length(tokens) + 1L]] <- list(
        kind = "literal",
        text = substr(template, pos, n))
      break
    }
    start_abs <- pos + open - 1L
    if (start_abs > pos) {
      tokens[[length(tokens) + 1L]] <- list(
        kind = "literal",
        text = substr(template, pos, start_abs - 1L))
    }
    close_abs <- regexpr("\\}", substr(template, start_abs, n))
    if (close_abs == -1L) {
      stop("unterminated URI template expression in: ", template)
    }
    end_abs <- start_abs + close_abs - 1L
    body <- substr(template, start_abs + 1L, end_abs - 1L)
    tokens[[length(tokens) + 1L]] <- list(
      kind = "expression",
      body = body,
      parsed = .parse_expression(body))
    pos <- end_abs + 1L
  }
  tokens
}

# ----- expansion -----------------------------------------------------

# Settings per operator. R doesn't allow `""` as a list-literal key
# at parse time, so we build the lookup at runtime.
.op_cfg <- function(op) {
  switch(op,
    "+" = list(first = "",  sep = ",", named = FALSE, ifempty = "",  allow = .RESERVED_CLASS),
    "#" = list(first = "#", sep = ",", named = FALSE, ifempty = "",  allow = .RESERVED_CLASS),
    "." = list(first = ".", sep = ".", named = FALSE, ifempty = "",  allow = .UNRESERVED_CLASS),
    "/" = list(first = "/", sep = "/", named = FALSE, ifempty = "",  allow = .UNRESERVED_CLASS),
    ";" = list(first = ";", sep = ";", named = TRUE,  ifempty = "",  allow = .UNRESERVED_CLASS),
    "?" = list(first = "?", sep = "&", named = TRUE,  ifempty = "=", allow = .UNRESERVED_CLASS),
    "&" = list(first = "&", sep = "&", named = TRUE,  ifempty = "=", allow = .UNRESERVED_CLASS),
    # Default (level 1) — empty operator string.
    list(first = "",  sep = ",", named = FALSE, ifempty = "",  allow = .UNRESERVED_CLASS))
}

# Expand a single (named or positional) variable per the operator's
# rules. Returns "" when the value is undefined.
.expand_var <- function(spec, value, opcfg) {
  if (is.null(value) ||
      (is.character(value) && length(value) == 0L)) {
    return(NULL)  # undefined — skip
  }
  is_list <- is.list(value) || (is.atomic(value) &&
                                length(value) > 1L)
  if (!is_list) {
    str <- as.character(value)
    if (!is.na(spec$prefix_len) &&
        nchar(str) > spec$prefix_len) {
      str <- substr(str, 1L, spec$prefix_len)
    }
    enc <- .pct_encode(str, opcfg$allow)
    if (opcfg$named) {
      sep <- if (nchar(enc) == 0L) opcfg$ifempty else "="
      paste0(spec$name, sep, enc)
    } else {
      enc
    }
  } else {
    items <- as.list(value)
    if (length(items) == 0L) return(NULL)
    is_named <- !is.null(names(items)) &&
                any(nzchar(names(items)))
    if (spec$exploded) {
      if (is_named) {
        pairs <- mapply(function(k, v) {
          enc_v <- .pct_encode(as.character(v), opcfg$allow)
          paste0(.pct_encode(k, opcfg$allow), "=", enc_v)
        }, names(items), items, SIMPLIFY = TRUE,
        USE.NAMES = FALSE)
        return(paste(pairs, collapse = opcfg$sep))
      }
      if (opcfg$named) {
        bits <- vapply(items, function(v)
          paste0(spec$name, "=",
                 .pct_encode(as.character(v), opcfg$allow)),
          character(1L))
        return(paste(bits, collapse = opcfg$sep))
      }
      bits <- vapply(items, function(v)
        .pct_encode(as.character(v), opcfg$allow),
        character(1L))
      return(paste(bits, collapse = opcfg$sep))
    }
    # Not exploded — comma-join inside the value.
    if (is_named) {
      flat <- vapply(seq_along(items), function(i)
        sprintf("%s,%s",
                .pct_encode(names(items)[[i]], opcfg$allow),
                .pct_encode(as.character(items[[i]]), opcfg$allow)),
        character(1L))
    } else {
      flat <- vapply(items, function(v)
        .pct_encode(as.character(v), opcfg$allow),
        character(1L))
    }
    str <- paste(flat, collapse = ",")
    if (opcfg$named) {
      sep <- if (nchar(str) == 0L) opcfg$ifempty else "="
      paste0(spec$name, sep, str)
    } else {
      str
    }
  }
}

# Expand a template against a named list of variables.
uri_template_expand <- function(template, vars) {
  tokens <- .tokenise_template(template)
  out <- character(0L)
  for (tok in tokens) {
    if (identical(tok$kind, "literal")) {
      out <- c(out, tok$text)
      next
    }
    op <- tok$parsed$operator
    opcfg <- .op_cfg(op)
    bits <- character(0L)
    for (spec in tok$parsed$vars) {
      ex <- .expand_var(spec, vars[[spec$name]], opcfg)
      if (!is.null(ex)) bits <- c(bits, ex)
    }
    if (length(bits) > 0L) {
      out <- c(out, opcfg$first, paste(bits, collapse = opcfg$sep))
    }
  }
  paste(out, collapse = "")
}

# Return the names of every variable referenced by a template.
uri_template_vars <- function(template) {
  tokens <- .tokenise_template(template)
  vars <- character(0L)
  for (tok in tokens) {
    if (identical(tok$kind, "expression")) {
      vars <- c(vars,
                vapply(tok$parsed$vars,
                       function(v) v$name, character(1L)))
    }
  }
  unique(vars)
}

# ----- matching (reverse) --------------------------------------------

# Build a regex that matches a concrete URI emitted by the template.
# Each expression becomes a capture group per variable.
.escape_literal <- function(s) {
  gsub("([\\.\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1",
       s, perl = TRUE)
}

uri_template_match <- function(template, uri) {
  tokens <- .tokenise_template(template)
  pattern <- ""
  group_names <- character(0L)
  for (tok in tokens) {
    if (identical(tok$kind, "literal")) {
      pattern <- paste0(pattern, .escape_literal(tok$text))
      next
    }
    op <- tok$parsed$operator
    opcfg <- .op_cfg(op)
    prefix <- opcfg$first
    sep <- opcfg$sep
    body <- ""
    for (i in seq_along(tok$parsed$vars)) {
      spec <- tok$parsed$vars[[i]]
      group_names <- c(group_names, spec$name)
      # Use a permissive class per operator (a non-greedy match
      # constrained by the next literal). For the default operator
      # variables cannot contain "/"; the reserved/fragment operators
      # can.
      no_slash <- !(op %in% c("+", "#"))
      ch_class <- if (no_slash) "[^/]" else "."
      if (opcfg$named) {
        bit <- sprintf("%s=([%s]*)",
                       .escape_literal(spec$name),
                       if (no_slash) "^/" else ".")
        # Replace placeholder with regex group capturing greedy.
        bit <- sprintf("%s=(%s+?)",
                       .escape_literal(spec$name), ch_class)
      } else {
        bit <- sprintf("(%s+?)", ch_class)
      }
      if (i == 1L) {
        body <- bit
      } else {
        body <- paste0(body, .escape_literal(sep), bit)
      }
    }
    pattern <- paste0(pattern, "(?:", .escape_literal(prefix),
                      body, ")?")
  }
  full <- paste0("^", pattern, "$")
  m <- regmatches(uri, regexec(full, uri, perl = TRUE))[[1L]]
  if (length(m) == 0L) return(NULL)
  caps <- m[-1L]
  if (length(caps) != length(group_names)) {
    # Mismatched groups — match did not produce the expected captures.
    return(NULL)
  }
  out <- as.list(caps)
  names(out) <- group_names
  # URL-decode captured values per the spec.
  out <- lapply(out, function(v) {
    if (is.na(v) || identical(v, "")) return(v)
    utils::URLdecode(as.character(v))
  })
  out
}
