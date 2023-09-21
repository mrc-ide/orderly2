test_that("can run simple case in separate directory", {
  info <- test_prepare_orderly_example_separate("explicit")
  id <- orderly_run_quietly("explicit", envir = new.env(),
                            root = info$outpack, root_src = info$src)
  expect_type(id, "character")
  expect_true(file.exists(file.path(info$src, "draft")))
  expect_false(file.exists(file.path(info$src, "archive")))
  expect_false(file.exists(file.path(info$outpack, "draft")))
  expect_true(file.exists(file.path(info$outpack, "archive")))
  expect_true(file.exists(file.path(info$outpack, "archive", "explicit", id)))
})
