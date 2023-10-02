test_that("can detect orderly directory", {
  path <- test_prepare_orderly_example("explicit")
  envir <- new.env()
  id <- orderly_run_quietly("explicit", root = path, envir = envir)

  expect_error(
    detect_orderly_interactive_path(path),
    "Failed to detect orderly path at")
  expect_error(
    detect_orderly_interactive_path(file.path(path, "src")),
    "Failed to detect orderly path at")
  root <- detect_orderly_interactive_path(file.path(path, "src", "explicit"))
  expect_equal(path, root)
})
