test_that("can detect orderly directory", {
  root <- test_prepare_orderly_example("explicit")
  detected_root <- detect_orderly_interactive_path(
    file.path(root, "src", "explicit"))
  expect_equal(detected_root, root)
})

test_that("errors when working directory is not report", {
  root <- test_prepare_orderly_example("explicit")

  expect_error(
    detect_orderly_interactive_path(root),
    "Working directory .* is not a valid orderly report.")

  expect_error(
    detect_orderly_interactive_path(file.path(root, "src")),
    "Working directory .* is not a valid orderly report.")
})

test_that("suggests changing working directory", {
  root <- test_prepare_orderly_example(c("explicit", "implicit"))

  e <- expect_error(detect_orderly_interactive_path(
    path = file.path(root, "src"),
    editor_path = file.path(root, "src", "implicit", "orderly.R")),
    "Working directory .* is not a valid orderly report")
  expect_match(e$body[[1]], paste(
    "Use `setwd(.*)` to set the working directory",
    "to the report currently open in RStudio"))

  w <- expect_warning(detect_orderly_interactive_path(
    path = file.path(root, "src", "explicit"),
    editor_path = file.path(root, "src", "implicit", "orderly.R")),
    "Working directory .* does not match the report currently open in RStudio")
  expect_match(w$body[[1]], "Use `setwd(.*)` to switch working directories")
})

test_that("does not unnecessarily suggest changing working directory", {
  root <- test_prepare_orderly_example("explicit")

  # Editor path is already the current working directory
  expect_no_warning(detect_orderly_interactive_path(
    path = file.path(root, "src", "explicit"),
    editor_path = file.path(root, "src", "explicit", "orderly.R")
  ))

  # Editor path is not an orderly report
  expect_no_warning(detect_orderly_interactive_path(
    path = file.path(root, "src", "explicit"),
    editor_path = file.path(root, "orderly_config.yml")
  ))

  # Editor path is an unsaved file
  expect_no_warning(detect_orderly_interactive_path(
    path = file.path(root, "src", "explicit"),
    editor_path = "Untitled"
  ))
})

test_that("can validate interactive parameters", {
  mock_readline <- mockery::mock("TRUE", "100", "1.23", '"string"')
  mockery::stub(get_parameter_interactive, "readline", mock_readline)
  expect_equal(get_parameter_interactive("foo"), TRUE)
  expect_equal(get_parameter_interactive("foo"), 100)
  expect_equal(get_parameter_interactive("foo"), 1.23)
  expect_equal(get_parameter_interactive("foo"), "string")
})


test_that("can error when interactive parameters are invlid", {
  mock_readline <- mockery::mock("", "hey ho", "string", "c(1, 2)")
  mockery::stub(get_parameter_interactive, "readline", mock_readline)
  err <- expect_error(
    get_parameter_interactive("foo"),
    "Expected a value for parameter 'foo'")
  expect_null(err$body)

  err <- expect_error(
    get_parameter_interactive("foo"),
    "Failed to parse value for parameter 'foo'")
  expect_length(err$body, 2)
  expect_equal(err$body[1], c(x = "Was given: hey ho"))
  expect_match(err$body[[2]], "If entering a string, you must use quotes")

  err <- expect_error(
    get_parameter_interactive("foo"),
    "Invalid input for parameter 'foo'")
  expect_equal(
    cli::ansi_strip(err$body[[1]]),
    'Did you mean: "string" (in quotes)?')
  expect_match(err$body[[2]], "If entering a string, you must use quotes")

  err <- expect_error(
    get_parameter_interactive("foo"),
    "Invalid input for parameter 'foo'")
  expect_equal(err$body[1], c(i = "Must be a simple boolean, number or string"))
})
