test_that("Configuration must be empty", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  fs::dir_create(tmp)
  writeLines(c(empty_config_contents(), "a: 1"),
             file.path(tmp, "orderly_config.yml"))
  expect_error(orderly_config_read(tmp),
               "Unknown fields in .+: a")
})


test_that("Configuration must exist", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  fs::dir_create(tmp)
  outpack_init_no_orderly(tmp)
  expect_error(orderly_config_read(tmp),
               "Orderly configuration does not exist: 'orderly_config.yml'")
})


test_that("error of opening an outpack root that is not an orderly root", {
  tmp <- withr::local_tempfile()
  root <- outpack_init_no_orderly(tmp)

  err <- expect_error(
    withr::with_dir(tmp, root_open(".", FALSE, TRUE)),
    "Did not find 'orderly_config.yml' in '.",
    fixed = TRUE)
  expect_equal(
    err$body,
    c(x = paste("Your directory has an '.outpack/' path, so is a valid",
                "outpack root, but does not contain 'orderly_config.yml' so",
                "cannot be used as an orderly root"),
      i = 'Please run orderly2::orderly_init(".") to initialise',
      i = "See ?orderly_init for more arguments to this function"))
})


test_that("pass back a root", {
  path_outpack <- withr::local_tempfile()
  root_outpack <- outpack_init_no_orderly(path_outpack)
  path_orderly <- test_prepare_orderly_example(character())
  root_orderly <- root_open(path_orderly, FALSE, TRUE)

  expect_identical(root_open(root_orderly, FALSE, FALSE), root_orderly)
  expect_identical(root_open(root_orderly, FALSE, TRUE), root_orderly)
  expect_identical(root_open(root_outpack, FALSE, FALSE), root_outpack)
  expect_error(
    root_open(root_outpack, FALSE, TRUE),
    sprintf("Did not find 'orderly_config.yml' in '%s'", root_outpack$path))
})
