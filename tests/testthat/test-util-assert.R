test_that("assert_scalar", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(numeric(0)), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")
})


test_that("assert_character", {
  expect_silent(assert_character("a"))
  expect_error(assert_character(1), "must be character")
  expect_error(assert_character(TRUE), "must be character")
})


test_that("assert_scalar_atomic", {
  expect_silent(assert_scalar_atomic(TRUE))
  expect_silent(assert_scalar_atomic(1))
  expect_silent(assert_scalar_atomic("a"))
  expect_error(assert_scalar_atomic(list(1)), "must be atomic")
})


test_that("assert_named", {
  expect_error(assert_named(1), "must be named")
  expect_error(assert_named(setNames(1:2, c("a", "a")), TRUE),
               "must have unique names")
  expect_silent(assert_named(setNames(1:2, c("a", "a")), FALSE))
})


test_that("assert_is", {
  expect_error(assert_is("x", "foo"), "must be a foo")
  expect_silent(assert_is(structure("x", class = "foo"), "foo"))
})


test_that("assert_file_exists", {
  tmp <- normalizePath(tempdir(), mustWork = TRUE)
  path <- tempfile(tmpdir = tmp)
  expect_error(assert_file_exists(path), "File does not exist")
  file.create(path)
  expect_silent(assert_file_exists(path))
  expect_silent(assert_file_exists(basename(path), workdir = tmp))
})


test_that("assert_file_exists: error in case", {
  mockery::stub(assert_file_exists, "file_exists",
                structure(c(TRUE, FALSE, FALSE),
                          incorrect_case = c(FALSE, TRUE, FALSE),
                          correct_case = c("FOO" = "foo")))
  expect_error(assert_file_exists(c("bar", "FOO", "gaz")),
               "File does not exist: 'FOO' (should be 'foo'), 'gaz'",
               fixed = TRUE)
})


test_that("assert_is_directory", {
  path <- tempfile(tmpdir = normalizePath(tempdir()))
  expect_error(assert_is_directory(path), "File does not exist")
  file.create(path)
  expect_error(assert_is_directory(path), "File exists but is not a directory")
  expect_silent(assert_is_directory("."))
})
