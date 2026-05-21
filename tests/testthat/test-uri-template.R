test_that("uri_template_match round-trip works for level-1 templates", {
  vars <- mcpserver:::uri_template_match("demo://x/{id}",
                                         "demo://x/42")
  expect_equal(vars$id, "42")
  expect_null(mcpserver:::uri_template_match("demo://x/{id}",
                                             "other://y/1"))
})

test_that("uri_template_expand inserts URL-encoded values", {
  out <- mcpserver:::uri_template_expand("demo://x/{id}",
                                         list(id = "a b"))
  expect_match(out, "demo://x/a%20b", fixed = TRUE)
})
