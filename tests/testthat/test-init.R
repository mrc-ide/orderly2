test_that("Error if invalid outpack arguments are given", {
  path <- withr::local_tempfile()
  expect_error(
    orderly_outpack_init(path, list(a = 1)),
    "Unknown argument in 'outpack_args': 'a', see ?outpack_args for details",
    fixed = TRUE)
  expect_error(
    orderly_outpack_init(path, list(a = 1, b = 2)),
    paste("Unknown argument in 'outpack_args': 'a', 'b',",
          "see ?outpack_args for details"),
    fixed = TRUE)
  ## More complex case, mix of valid and invalid:
  args <- list(a = 1, path_archive = NULL,
               use_file_store = TRUE, path = "x")
  expect_error(
    orderly_outpack_init(path, args),
    paste("Unknown argument in 'outpack_args': 'a', 'path',",
          "see ?outpack_args for details"),
    fixed = TRUE)
})


test_that("initialise outpack with passed args", {
  path <- withr::local_tempfile()
  args <- list(path_archive = NULL, use_file_store = TRUE,
               require_complete_tree = TRUE)
  root <- orderly_outpack_init(path, args)
  expect_true(root$config$core$use_file_store)
  expect_true(root$config$core$require_complete_tree)
  expect_null(root$config$core$path_archive)
})


test_that("can write out customised gitignore", {
  path <- withr::local_tempdir()
  write_gitignore(path, "foo")
  file <- file.path(path, ".gitignore")
  expect_true(file.exists(file))
  contents <- readLines(file)
  expect_match(contents, "^foo/$", all = FALSE)
})


test_that("Initialisation requires empty directory", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "file"))
  expect_error(orderly_init(tmp),
               "'path', if it already exists, must be an empty directory")
})


test_that("Can initialise a new orderly root", {
  tmp <- withr::local_tempdir()
  root <- orderly_init(tmp)
  expect_true(file.exists(tmp))
  expect_s3_class(root, "orderly_root")
  expect_s3_class(root$outpack, "outpack_root")
  expect_equal(root$config, list())
})
