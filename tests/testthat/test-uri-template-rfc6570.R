# Full RFC 6570 URI template tests (levels 1-4).
# Ports cases from packages/core/test/shared/uriTemplate.test.ts.

expand <- mcpserver:::uri_template_expand
match_ <- mcpserver:::uri_template_match
vars   <- mcpserver:::uri_template_vars

# --- Level 1 ---------------------------------------------------------

test_that("level 1: simple expansion encodes reserved characters", {
  expect_equal(expand("/x/{v}", list(v = "hello")), "/x/hello")
  expect_equal(expand("/x/{v}", list(v = "hello world")),
               "/x/hello%20world")
  expect_equal(expand("/x/{v}", list(v = "a/b")), "/x/a%2Fb")
})

test_that("level 1: multiple variables are comma-joined", {
  expect_equal(expand("/x/{a,b}",
                      list(a = "1", b = "2")), "/x/1,2")
})

test_that("level 1: undefined variables collapse to nothing", {
  expect_equal(expand("/x/{a}/{b}",
                      list(a = "1")), "/x/1/")
})

# --- Level 2: reserved + fragment -----------------------------------

test_that("level 2 +: reserved expansion lets / : ? @ etc. pass through", {
  expect_equal(expand("/x/{+path}",
                      list(path = "a/b/c")), "/x/a/b/c")
  expect_equal(expand("{+url}",
                      list(url = "http://a.b/x?q=1")),
               "http://a.b/x?q=1")
})

test_that("level 2 #: fragment expansion prefixes #", {
  expect_equal(expand("x{#frag}",
                      list(frag = "section1")),
               "x#section1")
})

# --- Level 3 --------------------------------------------------------

test_that("level 3 .: label expansion prefixes .", {
  expect_equal(expand("file{.ext}", list(ext = "html")),
               "file.html")
})

test_that("level 3 /: path-segment expansion prefixes /", {
  expect_equal(expand("x{/segment}",
                      list(segment = "foo")), "x/foo")
})

test_that("level 3 ;: path-style emits name=value", {
  expect_equal(expand("/x{;p}", list(p = "v")), "/x;p=v")
  expect_equal(expand("/x{;p}", list(p = "")), "/x;p")
})

test_that("level 3 ?: query expansion emits ?name=value", {
  expect_equal(expand("/x{?q}", list(q = "search")),
               "/x?q=search")
})

test_that("level 3 &: form-continuation expansion prefixes &", {
  expect_equal(expand("/x?a=1{&b}", list(b = "2")),
               "/x?a=1&b=2")
})

test_that("level 3 multi-variable in single expression", {
  expect_equal(expand("/x{?a,b,c}",
                      list(a = "1", b = "2", c = "3")),
               "/x?a=1&b=2&c=3")
})

# --- Level 4: explosion ---------------------------------------------

test_that("level 4 *: array explosion emits separate values", {
  expect_equal(expand("/x{?tags*}",
                      list(tags = c("a", "b", "c"))),
               "/x?tags=a&tags=b&tags=c")
})

test_that("level 4 *: named-list explosion emits key=value pairs", {
  expect_equal(expand("/x{?p*}",
                      list(p = list(a = "1", b = "2"))),
               "/x?a=1&b=2")
})

test_that("level 4: prefix length truncates a value", {
  expect_equal(expand("/x/{var:3}",
                      list(var = "hello")), "/x/hel")
})

# --- Matching (reverse) ---------------------------------------------

test_that("match level 1 simple template", {
  v <- match_("/x/{id}", "/x/42")
  expect_equal(v$id, "42")
})

test_that("match level 1 multi-variable returns the captures", {
  v <- match_("/x/{a}/{b}", "/x/1/2")
  expect_equal(v$a, "1")
  expect_equal(v$b, "2")
})

test_that("match returns NULL when the URI doesn't fit", {
  expect_null(match_("/x/{id}", "/y/42"))
  expect_null(match_("/x/{id}", "/x/42/extra"))
})

test_that("match URL-decodes captured values", {
  v <- match_("/x/{id}", "/x/hello%20world")
  expect_equal(v$id, "hello world")
})

# --- Edge cases / security ------------------------------------------

test_that("uri_template_vars enumerates every referenced variable", {
  expect_setequal(vars("/x{?a,b}/{c}/{d*}"),
                  c("a", "b", "c", "d"))
})

test_that("expand tolerates undefined variables silently", {
  expect_equal(expand("/x{?a,b}", list(a = "1")), "/x?a=1")
})

test_that("expand tolerates empty multi-var expressions", {
  expect_equal(expand("/x{?a,b}", list()), "/x")
})

test_that("extremely long template still parses (no ReDoS)", {
  long_template <- paste0("/x/{a}",
                           paste(rep("/{b}", 200L), collapse = ""))
  expect_no_error(expand(long_template,
                         list(a = "1", b = "2")))
})

test_that("malformed template (unterminated brace) errors", {
  expect_error(expand("/x/{id", list(id = "1")),
               "unterminated")
})

test_that("template with empty expression body expands to empty", {
  # {} is technically illegal per RFC 6570; we tolerate it as a no-op.
  expect_no_error(expand("/x/{}", list()))
})

test_that("deeply nested literal segments survive expansion", {
  tpl <- paste(rep("/a", 200L), collapse = "")
  tpl <- paste0(tpl, "/{id}")
  out <- expand(tpl, list(id = "x"))
  expect_match(out, "/x$", fixed = FALSE)
})
