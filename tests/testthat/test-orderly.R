test_that("no candidates returns empty character vector", {
  path <- test_prepare_orderly_example(character())
  expect_equal(withr::with_dir(path, orderly_list_src()), character())
  expect_equal(orderly_list_src(path), character())
})


test_that("find expected candidates", {
  v <- c("data", "depends", "depends-params", "description")
  path <- test_prepare_orderly_example(v)
  expect_equal(withr::with_dir(path, orderly_list_src()), v)
  expect_equal(orderly_list_src(path), v)
})


test_that("ignore paths without orderly.R", {
  v <- c("data", "depends", "depends-params", "description")
  path <- test_prepare_orderly_example(v)
  unlink(file.path(path, "src", v[1], "orderly.R"))
  expect_equal(orderly_list_src(path), v[-1])
})
