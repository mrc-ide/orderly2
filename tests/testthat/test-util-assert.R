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


test_that("assert_logical", {
  expect_silent(assert_logical(TRUE))
  expect_error(assert_logical(1), "must be logical")
  expect_error(assert_logical("true"), "must be logical")
})


test_that("assert_simple_scalar_atomic", {
  expect_silent(assert_simple_scalar_atomic(TRUE))
  expect_silent(assert_simple_scalar_atomic(1))
  expect_silent(assert_simple_scalar_atomic("a"))
  expect_error(assert_simple_scalar_atomic(list(1)), "must be atomic")
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
  tmp <- normalise_path(tempdir())
  path <- tempfile(tmpdir = tmp)
  expect_error(assert_file_exists(path, "File"), "File does not exist")
  file.create(path)
  expect_silent(assert_file_exists(path, "File"))
})


test_that("assert_file_exists_relative works checks if files exist", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "c"))
  expect_error(assert_file_exists_relative("a", tmp, "File"),
               "File does not exist: 'a'")
  expect_error(assert_file_exists_relative(c("a", "b"), tmp, "File"),
               "Files do not exist: 'a', 'b'")
  expect_error(assert_file_exists_relative(c("a", "b", "c", "d"), tmp, "File"),
               "Files do not exist: 'a', 'b', 'd'")
  expect_silent(assert_file_exists_relative("c", tmp, "File"))
})


test_that("assert_file_exists_relative informs about case mismatch", {
  testthat::skip_if_not_installed("mockery")
  mock_file_exists <- mockery::mock(TRUE, cycle = TRUE)
  mockery::stub(assert_file_exists_relative, "file_exists", mock_file_exists)

  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "a"))
  fs::dir_create(file.path(tmp, "b/c"))
  file.create(file.path(tmp, "b/c/d"))

  err <- expect_error(
    assert_file_exists_relative("A", tmp, "File"),
    "File does not exist: 'A'")
  expect_length(err$body, 3)
  expect_equal(names(err$body), c("i", "i", "i"))
  expect_equal(err$body[[1]], "For 'A', did you mean 'a'?")
  expect_match(err$body[[2]], "If you don't use the canonical case for a file")
  expect_match(err$body[[3]], "Looked within directory '.+'")

  err <- expect_error(
    assert_file_exists_relative(c("A", "b/C/d"), tmp, "File"),
    "Files do not exist: 'A', 'b/C/d'")
  expect_length(err$body, 4)
  expect_equal(names(err$body), c("i", "i", "i", "i"))
  expect_equal(err$body[[1]], "For 'A', did you mean 'a'?")
  expect_equal(err$body[[2]], "For 'b/C/d', did you mean 'b/c/d'?")
  expect_match(err$body[[3]], "If you don't use the canonical case for a file")
  expect_match(err$body[[4]], "Looked within directory '.+'")

  err <- expect_error(
    assert_file_exists_relative(c("A", "b/X/d"), tmp, "File"),
    "Files do not exist: 'A', 'b/X/d'")
  expect_length(err$body, 3)
  expect_equal(names(err$body), c("i", "i", "i"))
  expect_equal(err$body[[1]], "For 'A', did you mean 'a'?")
  expect_match(err$body[[3]], "If you don't use the canonical case for a file")
  expect_match(err$body[[4]], "Looked within directory '.+'")
})


test_that("assert_is_directory", {
  path <- tempfile(tmpdir = normalise_path(tempdir()))
  expect_error(assert_is_directory(path), "Directory does not exist")
  file.create(path)
  expect_error(assert_is_directory(path),
               "Path exists but is not a directory")
  expect_silent(assert_is_directory("."))
})


test_that("assert_relative_path", {
  workdir <- getwd()
  expect_error(assert_relative_path(getwd(), "File", workdir),
               "File must be a relative path",
               fixed = TRUE)
  expect_silent(assert_relative_path("relpath", "File", workdir))
  expect_silent(assert_relative_path("a/b/c", "File", workdir))

  expect_error(
    assert_relative_path("../my/path", "File", workdir),
    "must not contain '..' (parent directory) components",
    fixed = TRUE)
  expect_error(
    assert_relative_path("my/../../path", "File", workdir),
    "must not contain '..' (parent directory) components",
    fixed = TRUE)
})


test_that("match_value", {
  expect_error(match_value("foo", letters), "must be one of")
  expect_silent(match_value("a", letters))
})
