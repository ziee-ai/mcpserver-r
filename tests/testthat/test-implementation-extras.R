# 2025-11-25 BaseMetadata fields on serverInfo.

test_that("serverInfo carries description, websiteUrl, icons when set", {
  srv <- new_server("demo",
                    description = "A demo server.",
                    website_url = "https://demo.example",
                    icons = list(list(src = "icon-32.png",
                                       mimeType = "image/png")))
  info <- srv$server_info()
  expect_equal(info$description, "A demo server.")
  expect_equal(info$websiteUrl, "https://demo.example")
  expect_equal(info$icons[[1L]]$src, "icon-32.png")
})

test_that("serverInfo omits fields when not set", {
  srv <- new_server("demo")
  info <- srv$server_info()
  expect_null(info$description)
  expect_null(info$websiteUrl)
  expect_null(info$icons)
})
