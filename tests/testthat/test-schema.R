test_that("can load custom metadata schema", {
  cache$custom_metadata_schema <- NULL
  s <- custom_metadata_schema(list())
  expect_type(s, "character")
  expect_length(s, 1)
  expect_identical(custom_metadata_schema(list()), s)
})


test_that("can add plugin schemas to metadata", {
  schema <- structure('{"type": "object"}', class = "json")
  config <- list(plugins = list(a = list(schema = schema)))
  s <- custom_metadata_schema(config)
  d <- jsonlite::fromJSON(s)
  expect_equal(
    d$properties$plugins,
    list(type = "object",
         properties = list(a = list(type = "object"))))
})


test_that("no plugin schema by default", {
  s <- custom_metadata_schema(list())
  d <- jsonlite::fromJSON(s)
  expect_length(d$properties$plugins, 0)
})
