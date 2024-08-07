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


test_that("ignore paths without orderly file", {
  v <- c("data", "depends", "depends-params", "description")
  path <- test_prepare_orderly_example(v)
  fs::file_delete(file.path(path, "src", v[1], "data.R"))
  expect_equal(orderly_list_src(path), v[-1])
})


test_that("no candidates returns empty character vector", {
  path <- test_prepare_orderly_example(character())
  fs::dir_delete(file.path(path, "src"))
  expect_setequal(dir(path, all.files = TRUE, no.. = TRUE),
                  c(".outpack", "orderly_config.yml"))
  expect_equal(withr::with_dir(path, orderly_list_src()), character())
  expect_equal(orderly_list_src(path), character())
})


test_that("can create empty orderly report", {
  path <- test_prepare_orderly_example(character())
  expect_message(
    orderly_new("foo", root = path),
    "Created 'src/foo/foo.R'")
  path_orderly <- file.path(path, "src", "foo", "foo.R")
  expect_true(file.exists(path_orderly))
  txt <- readLines(path_orderly)
  expect_match(txt[[1]], "This is an orderly script")
})


test_that("can create a totally blank orderly report", {
  path <- test_prepare_orderly_example(character())
  expect_message(
    orderly_new("foo", template = FALSE, root = path),
    "Created 'src/foo/foo.R'")
  path_orderly <- file.path(path, "src", "foo", "foo.R")
  expect_true(file.exists(path_orderly))
  expect_equal(readLines(path_orderly), character())
})


test_that("error if orderly file exists already", {
  path <- test_prepare_orderly_example("data")
  expect_error(orderly_new("data", root = path),
               "'src/data/data.R' already exists")
  expect_error(orderly_new("data", force = TRUE, root = path),
               "'src/data/data.R' already exists")
})


test_that("error two orderly file exist already", {
  path <- test_prepare_orderly_example("two-orderly-files")
  expect_error(orderly_new("two-orderly-files", force = TRUE, root = path),
               paste("Please only create two-orderly-files.R file,",
                     "orderly.R has been deprecated"))
})


test_that("error if a non-directory file is found in the src dir", {
  path <- test_prepare_orderly_example(character())
  file.create(file.path(path, "src", "foo"))
  err <- expect_error(
    orderly_new("foo", template = FALSE, root = path),
    "'src/foo' already exists, but is not a directory")
  expect_equal(
    err$body,
    c(i = "This file really should not be here, you might need to tidy up"))
})


test_that("allow creation of foo.R in existing src/foo if force is given", {
  path <- test_prepare_orderly_example(character())
  fs::dir_create(file.path(path, "src", "foo"))
  file.create(file.path(path, "src", "foo", "deprebar"))
  err <- expect_error(
    orderly_new("foo", root = path),
    "'src/foo/' already exists and contains files")
  expect_equal(
    err$body,
    c(i = paste("If you want to add a foo.R to this directory,",
                "rerun `orderly_new()` with `force = TRUE`")))
  expect_message(
    orderly_new("foo", force = TRUE, root = path),
    "Created 'src/foo/foo.R'")
  expect_true(file.exists(file.path(path, "src/foo/foo.R")))
})


test_that("allow creation of orderly file in existing empty dir", {
  path <- test_prepare_orderly_example(character())
  fs::dir_create(file.path(path, "src", "foo"))
  expect_message(
    orderly_new("foo", root = path),
    "Created 'src/foo/foo.R'")
  expect_true(file.exists(file.path(path, "src/foo/foo.R")))
})


test_that("disallow template arguments", {
  path <- test_prepare_orderly_example(character())
  expect_error(
    orderly_new("foo", template = "other", root = path),
    "'template' must be 'NULL' or 'FALSE' for now")
  expect_false(file.exists(file.path(path, "src/foo")))
})
