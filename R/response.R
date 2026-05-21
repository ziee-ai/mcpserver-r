# Tool / prompt response constructors -------------------------------------

# Internal: tag a list with a class so the dispatcher can recognise it as
# a constructed MCP response and stop walking further.
as_content <- function(x, type) {
  x$type <- type
  x
}

#' Build a text content block
#'
#' @param text Character scalar.
#' @param annotations Optional MCP annotations list (e.g.
#'   `list(audience = "user", priority = 0.5)`).
#' @return A list with a `"text"` `type` tag, ready to be returned from a
#'   tool, resource, or prompt handler.
#' @export
#' @examples
#' response_text("hello world")
response_text <- function(text, annotations = NULL) {
  out <- list(type = "text", text = as.character(text))
  if (!is.null(annotations)) out$annotations <- annotations
  out
}

# Encode a binary payload (raw vector or already-base64 character) for the
# MCP `data` field, which must be a base64 string.
encode_blob <- function(data) {
  if (is.character(data) && length(data) == 1L) return(data)
  if (is.raw(data)) return(jsonlite::base64_enc(data))
  stop("response binary data must be raw or base64 character")
}

#' Build an image content block
#'
#' @param data Raw bytes or base64-encoded character.
#' @param mime_type MIME type, e.g. `"image/png"`.
#' @param annotations Optional MCP annotations list.
#' @return A content list with `type = "image"`.
#' @export
#' @examples
#' response_image(charToRaw("not actually a png"), "image/png")
response_image <- function(data, mime_type, annotations = NULL) {
  out <- list(type = "image",
              data = encode_blob(data),
              mimeType = as.character(mime_type))
  if (!is.null(annotations)) out$annotations <- annotations
  out
}

#' Build an audio content block
#'
#' @param data Raw bytes or base64-encoded character.
#' @param mime_type MIME type, e.g. `"audio/mpeg"`.
#' @param annotations Optional MCP annotations list.
#' @return A content list with `type = "audio"`.
#' @export
#' @examples
#' response_audio(charToRaw("not actually mp3"), "audio/mpeg")
response_audio <- function(data, mime_type, annotations = NULL) {
  out <- list(type = "audio",
              data = encode_blob(data),
              mimeType = as.character(mime_type))
  if (!is.null(annotations)) out$annotations <- annotations
  out
}

#' Build a video content block
#'
#' Video content is not in the stable MCP content-type list yet but the
#' spec allows custom content blocks; this helper emits a `"video"` block
#' shaped like image/audio for forward compatibility.
#'
#' @param data Raw bytes or base64-encoded character.
#' @param mime_type MIME type, e.g. `"video/mp4"`.
#' @param annotations Optional MCP annotations list.
#' @return A content list with `type = "video"`.
#' @export
#' @examples
#' response_video(charToRaw("not actually mp4"), "video/mp4")
response_video <- function(data, mime_type, annotations = NULL) {
  out <- list(type = "video",
              data = encode_blob(data),
              mimeType = as.character(mime_type))
  if (!is.null(annotations)) out$annotations <- annotations
  out
}

#' Build a file resource block from a path
#'
#' Reads the file from disk, base64-encodes it, and returns an embedded
#' `resource` content block. When `mime_type` is `NULL` we guess from the
#' filename extension via [tools::file_ext()].
#'
#' @param path File path.
#' @param mime_type Optional MIME type override.
#' @param uri Optional URI to advertise; defaults to `file://<path>`.
#' @param annotations Optional MCP annotations list.
#' @return A content list with `type = "resource"`.
#' @export
#' @examples
#' tmp <- tempfile(fileext = ".txt")
#' writeLines("hi", tmp)
#' response_file(tmp)
response_file <- function(path, mime_type = NULL, uri = NULL,
                          annotations = NULL) {
  bytes <- readBin(path, what = "raw",
                   n = file.info(path)$size %||% 0L)
  ext <- tools::file_ext(path)
  mt <- mime_type %||% switch(tolower(ext),
    "txt" = "text/plain",
    "json" = "application/json",
    "md" = "text/markdown",
    "png" = "image/png",
    "jpg" = , "jpeg" = "image/jpeg",
    "pdf" = "application/pdf",
    "application/octet-stream")
  uri_v <- uri %||% sprintf("file://%s",
                            normalizePath(path, mustWork = FALSE))
  inner <- list(uri = uri_v, mimeType = mt, blob = encode_blob(bytes))
  out <- list(type = "resource", resource = inner)
  if (!is.null(annotations)) out$annotations <- annotations
  out
}

#' Build an embedded resource content block
#'
#' Returns a `resource` content block whose body is inlined (either text or
#' a base64-encoded blob), suitable for embedding inside a tool or prompt
#' result.
#'
#' @param uri Resource URI.
#' @param text Optional text body.
#' @param blob Optional raw or base64 blob body.
#' @param mime_type Optional MIME type.
#' @return A content list with `type = "resource"`.
#' @export
#' @examples
#' response_resource("demo://x", text = "embedded")
response_resource <- function(uri, text = NULL, blob = NULL,
                              mime_type = NULL) {
  inner <- list(uri = as.character(uri))
  if (!is.null(mime_type)) inner$mimeType <- as.character(mime_type)
  if (!is.null(text))      inner$text     <- as.character(text)
  if (!is.null(blob))      inner$blob     <- encode_blob(blob)
  list(type = "resource", resource = inner)
}

#' Build a resource link content block
#'
#' Used by tools to point at a resource that the client should fetch
#' separately via `resources/read`, rather than inlining its bytes in the
#' response.
#'
#' @param uri Resource URI.
#' @param name Optional human-readable name.
#' @param description Optional description.
#' @param mime_type Optional MIME type.
#' @return A content list with `type = "resource_link"`.
#' @export
#' @examples
#' response_resource_link("demo://doc/42", name = "Doc 42")
response_resource_link <- function(uri, name = NULL,
                                   description = NULL,
                                   mime_type = NULL) {
  out <- list(type = "resource_link",
              uri = as.character(uri))
  if (!is.null(name))        out$name        <- as.character(name)
  if (!is.null(description)) out$description <- as.character(description)
  if (!is.null(mime_type))   out$mimeType    <- as.character(mime_type)
  out
}

#' Build a tool error result
#'
#' Wraps a text content block and marks the tool result as an error so the
#' client can render it appropriately. The JSON-RPC envelope itself still
#' returns success; only the inner `isError` flag is set.
#'
#' @param message Error message.
#' @param code Optional application-specific code (numeric).
#' @param data Optional extra error data.
#' @return A list with `isError = TRUE` and a text content block.
#' @export
#' @examples
#' response_error("tool failed")
response_error <- function(message, code = NULL, data = NULL) {
  out <- list(
    isError = TRUE,
    content = list(response_text(as.character(message)))
  )
  if (!is.null(code) || !is.null(data)) {
    out$error <- drop_nulls(list(code = code, data = data))
  }
  out
}

#' Build a structured tool result
#'
#' Tools that declare an `output_schema` should return both human-facing
#' content blocks and a machine-readable `structuredContent` payload that
#' conforms to the schema.
#'
#' @param content_list List of content blocks (e.g. one or more
#'   [response_text()] entries).
#' @param structured_content Named list matching the tool's `output_schema`.
#' @return A list wrapping both fields.
#' @export
#' @examples
#' response_structured(
#'   list(response_text("ok")),
#'   list(value = 42L)
#' )
response_structured <- function(content_list, structured_content) {
  list(content = content_list,
       structuredContent = structured_content)
}

# Internal: turn whatever a handler returned into the canonical
#   { content = [...], isError? = TRUE, structuredContent? = ... }
# shape expected by `tools/call`.
normalize_tool_result <- function(value) {
  if (is.list(value) && "isError" %in% names(value) && "content" %in% names(value)) {
    return(value)
  }
  if (is.list(value) && "content" %in% names(value)) {
    return(value)
  }
  if (is.list(value) && !is.null(value$type)) {
    return(list(content = list(value), isError = FALSE))
  }
  if (is.list(value) && length(value) > 0L &&
      is.list(value[[1L]]) && !is.null(value[[1L]]$type)) {
    return(list(content = value, isError = FALSE))
  }
  if (is.character(value)) {
    return(list(content = list(response_text(value)), isError = FALSE))
  }
  list(content = list(response_text(to_json(value))), isError = FALSE)
}
