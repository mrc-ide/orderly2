test_that("Missing or empty orderly_envir files are NULL", {
  expect_null(orderly_envir_read(tempfile()))
  path <- test_prepare_orderly_example("explicit")
  expect_null(orderly_envir_read(path))
  file.create(file.path(path, "orderly_envir.yml"))
  expect_null(orderly_envir_read(path))
})


test_that("Can read an orderly environment file", {
  path <- test_prepare_orderly_example("explicit")
  writeLines("A: value1\nB: value2", file.path(path, "orderly_envir.yml"))
  expect_equal(orderly_envir_read(path), c(A = "value1", B = "value2"))
})


test_that("environment files must be really simple", {
  path <- test_prepare_orderly_example("explicit")
  writeLines("A: value1\nB: [1, 2]", file.path(path, "orderly_envir.yml"))
  expect_error(
    orderly_envir_read(path),
    "Expected all elements of orderly_envir.yml to be scalar (check 'B')",
    fixed = TRUE)
})


test_that("can interpolate values into an orderly configuration", {
  ## This is a silly test because we'd never normally interpolate in
  ## the global resources - this is really to support databases via
  ## the plugin....
  path <- test_prepare_orderly_example("global")
  expect_equal(orderly_root(path, FALSE)$config$global_resources, "global")

  writeLines("global_resources: $PATH_GLOBAL",
             file.path(path, "orderly_config.yml"))
  expect_error(
    orderly_root(path, FALSE),
    paste0("Environment variable 'PATH_GLOBAL' is not set\n\t",
           "(used in orderly_config.yml$global_resources)"),
    fixed = TRUE)
  withr::with_envvar(
    c(PATH_GLOBAL = "global"),
    expect_equal(orderly_root(path, FALSE)$config$global_resources, "global"))
  writeLines("PATH_GLOBAL: global", file.path(path, "orderly_envir.yml"))
  expect_equal(orderly_root(path, FALSE)$config$global_resources, "global")
})
