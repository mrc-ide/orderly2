test_that("Configuration must be empty", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  fs::dir_create(tmp)
  writeLines("a: 1", file.path(tmp, "orderly_config.yml"))
  expect_error(orderly_config(tmp),
               "Unknown fields in .+: a")
})


test_that("Configuration must exist", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  fs::dir_create(tmp)
  outpack::outpack_init(tmp)
  expect_error(orderly_config(tmp),
               "Orderly configuration does not exist: 'orderly_config.yml'")
  expect_error(orderly_root(tmp, FALSE),
               "Orderly configuration does not exist: 'orderly_config.yml'")
})


test_that("Can validate global resources", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  root <- orderly_init(tmp)
  writeLines("global_resources: global", file.path(tmp, "orderly_config.yml"))
  expect_error(orderly_config(tmp),
               "Global resource directory does not exist: 'global'")
  dir.create(file.path(tmp, "global"))
  cfg <- orderly_config(tmp)
  expect_equal(cfg$global_resources, "global")
})
