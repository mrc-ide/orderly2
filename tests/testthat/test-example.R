test_that("can use custom path", {
  path <- withr::local_tempfile()
  res <- suppressMessages(orderly_example(example = "simple", dest = path))
  expect_equal(res, path)
  expect_equal(orderly_list_src(root = path), "data")
})


test_that("can set config", {
  res <- suppressMessages(orderly_example(example = "simple",
                                          use_file_store = TRUE,
                                          path_archive = NULL))
  config <- orderly_config(res)
  expect_true(config$core$use_file_store)
  expect_null(config$core$path_archive)
})


test_that("error if path exists", {
  path <- withr::local_tempdir()
  expect_error(orderly_example(example = "simple", dest = path),
               "The path '.+' must not exist")
})


test_that("don't provide filename with example.db", {
  expect_error(
    orderly_example_show(example = "example.db", file = "DESCRIPTION"),
    "Don't use 'file' with 'example.db'",
    fixed = TRUE)
})


test_that("can show files from example.db", {
  res <- evaluate_promise(
    orderly_example_show("R/plugin.R", example = "example.db"))
  expect_match(res$messages, "R/plugin.R", fixed = TRUE, all = FALSE)
  expect_match(res$messages, "db_config <- function(data, filename) {",
               fixed = TRUE, all = FALSE)
})


test_that("can show files from the ecxamples", {
  res <- evaluate_promise(
    orderly_example_show("shared"))
  expect_match(res$messages, "src/shared/shared.R", fixed = TRUE, all = FALSE)
  expect_match(res$messages,
               "Pull in the file 'shared/palette.R' as 'cols.R'",
               fixed = TRUE, all = FALSE)
})
