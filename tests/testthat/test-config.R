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
  err <- expect_error(
    orderly_envir_read(path),
    "All elements of 'orderly_envir.yml' must be scalar",
    fixed = TRUE)
  expect_equal(err$body[1],
               c(x = "Expected 'B' to be scalar, but had length 2"))
  expect_match(err$body[[2]],
               "^Working directory was '.+'$")
})


test_that("can validate minimum required version", {
  expect_error(
    orderly_config_validate_minimum_orderly_version("1.4.5", "orderly.yml"),
    "Detected old orderly project, you need to migrate to orderly 2",
    fixed = TRUE)
  expect_error(
    orderly_config_validate_minimum_orderly_version("99.0.0", "orderly.yml"),
    sprintf("orderly version '99.0.0' is required, but only '%s' installed",
            current_orderly_version()),
    fixed = TRUE)
  expect_equal(
    orderly_config_validate_minimum_orderly_version("1.99.0", "orderly.yml"),
    numeric_version("1.99.0"))
})
