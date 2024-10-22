test_that("can use custom path", {
  path <- withr::local_tempfile()
  res <- suppressMessages(orderly_example("default", dest = path))
  expect_equal(res, path)
  expect_equal(orderly_list_src(root = path), "data")
})


test_that("can set config", {
  res <- suppressMessages(orderly_example("default",
                                          use_file_store = TRUE,
                                          path_archive = NULL))
  config <- orderly_config(res)
  expect_true(config$core$use_file_store)
  expect_null(config$core$path_archive)
})


test_that("error if path exists", {
  path <- withr::local_tempdir()
  expect_error(orderly_example("default", dest = path),
               "The path '.+' must not exist")
})
