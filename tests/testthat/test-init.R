test_that("can initialise a repo with orderly but no .outpack directory", {
  path <- test_prepare_orderly_example("data")
  parent <- dirname(path)
  base <- basename(path)
  unlink(file.path(path, ".outpack"), recursive = TRUE)
  err <- expect_error(
    withr::with_dir(parent, orderly_run("data", root = base)),
    sprintf("orderly directory '%s' not initialise", base))
  expect_equal(
    err$body,
    c(x = "Did not find an '.outpack' directory within path",
      i = sprintf('Please run orderly2::orderly_init("%s") to initialise',
                  base),
      i = "See ?orderly_init for more arguments to this function"))

  withr::with_dir(parent, orderly_init(base, logging_console = FALSE))
  root <- orderly_root_open(path, FALSE)
  expect_false(root$config$logging$console)
  expect_true(is_directory(file.path(path, ".outpack")))

  id <- withr::with_dir(parent, orderly_run("data", root = base))
  expect_type(id, "character")
})
