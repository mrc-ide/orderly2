test_that("can detect orderly directory", {
  path <- test_prepare_orderly_example("explicit")
  env <- new.env()
  id <- orderly_run("explicit", root = path, envir = env)

  expect_error(
    detect_orderly_interactive_path(path),
    "Failed to detect orderly path at")
  expect_error(
    detect_orderly_interactive_path(file.path(path, "src")),
    "Failed to detect orderly path at")
  root <- detect_orderly_interactive_path(file.path(path, "src", "explicit"))
  expect_s3_class(root, "orderly_root")
})
