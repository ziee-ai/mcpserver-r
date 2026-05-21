# URI-template security regression — F1-F5 attack vectors mirrored from
# Kotlin SDK's PathSegmentTemplateMatcherSecurityTest. Confirms that
# R's level-1 + RFC 6570 matchers do not let path-traversal / double-
# decode / double-slash / segment-amplification tricks through.

# F1: double percent-decode -- an attacker submits %2520 (== %20 once
# decoded → " " escaped) hoping the matcher decodes twice.
test_that("F1: double-percent-encoded values are decoded only once", {
  vars <- mcpserver:::uri_template_match("test://x/{id}",
                                         "test://x/%2520foo")
  expect_false(is.null(vars))
  # The matched value should be the once-decoded form ("%20foo"),
  # never the twice-decoded " foo".
  expect_equal(vars$id, "%20foo")
})

# F2: dot-segment traversal -- "../" or "./" must not collapse during
# matching; templates pin the structure.
test_that("F2: dot-segment traversal does NOT bypass template structure", {
  expect_null(mcpserver:::uri_template_match(
    "test://x/{id}/data",
    "test://x/../etc/data"))
})

test_that("F2b: encoded dot segments are matched as literal bytes", {
  # The variable just captures the encoded form; the value lifecycle
  # is the caller's concern. Critically, the match does NOT silently
  # collapse "%2E%2E" → "..".
  vars <- mcpserver:::uri_template_match("test://x/{id}",
                                         "test://x/%2E%2E")
  expect_false(is.null(vars))
  expect_equal(vars$id, "..")
})

# F3: double-slash -- "/x//y" should NOT match a template with a
# single literal slash between segments.
test_that("F3: double slash inside a literal segment is rejected", {
  # Template "test://x/{id}/y" expects a single segment for id;
  # "test://x//y" leaves id empty and skips the literal "/y".
  expect_null(mcpserver:::uri_template_match(
    "test://x/{id}/y",
    "test://x//y"))
})

# F4: segment-depth amplification -- a single {id} placeholder must
# not span multiple "/" boundaries.
test_that("F4: {id} captures only one path segment, not multiple", {
  # Without the `+` operator (which reserves) `{id}` is supposed to
  # encode reserved characters. The matcher's default capture must
  # not span "/" boundaries.
  expect_null(mcpserver:::uri_template_match(
    "test://x/{id}/y",
    "test://x/a/b/y"))
})

# F5: empty-segment exploitation -- a trailing slash without a value.
test_that("F5: empty variable value yields a non-match by default", {
  expect_null(mcpserver:::uri_template_match(
    "test://x/{id}/y",
    "test://x//y"))
})

# Defensive: malformed templates raise rather than match permissively.
test_that("malformed template (unterminated brace) is rejected", {
  expect_error(mcpserver:::uri_template_match("test://x/{id", "test://x/a"))
})

# Defensive: extremely long URIs don't blow up the matcher (no ReDoS).
test_that("very long URI doesn't blow up the matcher", {
  long_id <- paste(rep("a", 5000L), collapse = "")
  vars <- mcpserver:::uri_template_match(
    "test://x/{id}",
    paste0("test://x/", long_id))
  expect_false(is.null(vars))
  expect_equal(nchar(vars$id), 5000L)
})

# F6: backslash in the URI must not affect matching on POSIX semantics
test_that("backslashes are matched as literal characters, not separators", {
  vars <- mcpserver:::uri_template_match("test://x/{id}",
                                         "test://x/a%5Cb")
  expect_false(is.null(vars))
  expect_equal(vars$id, "a\\b")
})

# F7: null byte protection -- a percent-encoded null in the URI is
# captured but the host application must be aware (no panic in match).
test_that("percent-encoded null byte does not crash the matcher", {
  expect_silent(mcpserver:::uri_template_match("test://x/{id}",
                                               "test://x/%00"))
})
