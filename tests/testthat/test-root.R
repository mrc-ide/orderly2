test_that("Configuration must be empty", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  fs::dir_create(tmp)
  writeLines(c(empty_config_contents(), "a: 1"),
             file.path(tmp, "orderly_config.yml"))
  expect_error(orderly_config(tmp),
               "Unknown fields in .+: a")
})


test_that("Configuration must exist", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  fs::dir_create(tmp)
  outpack_init(tmp, logging_console = FALSE)
  expect_error(orderly_config(tmp),
               "Orderly configuration does not exist: 'orderly_config.yml'")
  expect_error(orderly_root(tmp, FALSE),
               "Orderly configuration does not exist: 'orderly_config.yml'")
})


test_that("Initialisation requires empty directory", {
  tmp <- tempfile()
  fs::dir_create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  file.create(file.path(tmp, "file"))
  expect_error(orderly_init(tmp, logging_console = FALSE),
               "'path', if it already exists, must be an empty directory")
})


test_that("Can initialise a new orderly root", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  root <- orderly_init(tmp, logging_console = FALSE)
  expect_true(file.exists(tmp))
  expect_s3_class(root, "orderly_root")
  expect_s3_class(root$outpack, "outpack_root")
  expect_equal(root$config,
               list(minimum_orderly_version = numeric_version("1.99.0")))
})


test_that("Can validate global resources", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  root <- orderly_init(tmp, logging_console = FALSE)
  writeLines(c(empty_config_contents(),
               "global_resources: global"),
             file.path(tmp, "orderly_config.yml"))
  expect_error(orderly_config(tmp),
               "Global resource directory does not exist: 'global'")
  dir.create(file.path(tmp, "global"))
  cfg <- orderly_config(tmp)
  expect_equal(cfg$global_resources, "global")
})
