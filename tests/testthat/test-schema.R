test_that("can load custom metadata schema", {
  cache$custom_metadata_schema <- NULL
  s <- custom_metadata_schema()
  expect_type(s, "character")
  expect_length(s, 1)
  expect_identical(custom_metadata_schema(), s)
})
