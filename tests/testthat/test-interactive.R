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
    editor_path = file.path(root, "src", "implicit", "implicit.R")),
    "Working directory .* is not a valid orderly report")
  expect_match(e$body[[1]], paste(
    "Use `setwd(.*)` to set the working directory",
    "to the report currently open in RStudio"))

  w <- expect_warning(detect_orderly_interactive_path(
    path = file.path(root, "src", "explicit"),
    editor_path = file.path(root, "src", "implicit", "implicit.R")),
    "Working directory .* does not match the report currently open in RStudio")
  expect_match(w$body[[1]], "Use `setwd(.*)` to switch working directories")
})

test_that("does not unnecessarily suggest changing working directory", {
  root <- test_prepare_orderly_example("explicit")

  # Editor path is already the current working directory
  expect_no_warning(detect_orderly_interactive_path(
    path = file.path(root, "src", "explicit"),
    editor_path = file.path(root, "src", "explicit", "explicit.R")
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

test_that("rstudio API is not called when unavailable", {
  testthat::skip_if_not_installed("mockery")
  mock_rstudio_available <- mockery::mock(FALSE)
  mock_rstudio_context <- mockery::mock()
  mockery::stub(
    rstudio_get_current_active_editor_path,
    "is_testing",
    mockery::mock(FALSE))
  mockery::stub(
    rstudio_get_current_active_editor_path,
    "rstudioapi::isAvailable",
    mock_rstudio_available)
  mockery::stub(
    rstudio_get_current_active_editor_path,
    "rstudioapi::getSourceEditorContext",
    mockery::mock(FALSE))
  expect_null(rstudio_get_current_active_editor_path())
  mockery::expect_called(mock_rstudio_available, 1)
  mockery::expect_called(mock_rstudio_context, 0)
})

test_that("rstudio API is used to find current editor path", {
  testthat::skip_if_not_installed("mockery")
  mockery::stub(
    rstudio_get_current_active_editor_path,
    "is_testing",
    mockery::mock(FALSE))
  mockery::stub(
    rstudio_get_current_active_editor_path,
    "rstudioapi::isAvailable",
    mockery::mock(TRUE))
  mockery::stub(
    rstudio_get_current_active_editor_path,
    "rstudioapi::getSourceEditorContext",
    mockery::mock(list(path = "/path/to/file")))
  expect_equal(rstudio_get_current_active_editor_path(), "/path/to/file")
})

test_that("can validate interactive parameters", {
  mock_readline <- mockery::mock("TRUE", "100", "1.23", '"string"')
  mockery::stub(get_parameter_interactive, "readline", mock_readline)
  expect_equal(get_parameter_interactive("foo"), TRUE)
  expect_equal(get_parameter_interactive("foo"), 100)
  expect_equal(get_parameter_interactive("foo"), 1.23)
  expect_equal(get_parameter_interactive("foo"), "string")
})


test_that("can prompt for parameter using defaults", {
  mock_readline <- mockery::mock("", '"a"')
  mockery::stub(get_parameter_interactive, "readline", mock_readline)

  msg <- testthat::capture_messages(
    res <- get_parameter_interactive("foo", "b"))
  expect_equal(res, "b")
  expect_match(msg, "Using default value for 'foo': b\\b")

  msg <- testthat::capture_messages(
    res <- get_parameter_interactive("foo", "b"))
  expect_equal(res, "a")
  expect_equal(msg, character())
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


test_that("error if prompting for parameters in non-interactive session", {
  rlang::local_interactive(FALSE)
  expect_error(
    prompt_parameters_interactive(list(a = NULL)),
    "Can't interactively fetch parameters in non-interactive session")
})


test_that("prompt for parameters interactively", {
  rlang::local_interactive(TRUE)
  mock_get <- mockery::mock(1, 2, 3)
  testthat::local_mocked_bindings(get_parameter_interactive = mock_get)
  expect_message(
    res <- prompt_parameters_interactive(
      list(a = NULL, b = 20, c = NULL), NULL),
    "Please enter values for 3 parameters")
  expect_equal(res, strict_list(a = 1, b = 2, c = 3, .name = "parameters"))
  mockery::expect_called(mock_get, 3)
  expect_equal(
    mockery::mock_args(mock_get)[[1]],
    list("a", NULL, NULL))
  expect_equal(
    mockery::mock_args(mock_get)[[2]],
    list("b", 20, NULL))
  expect_equal(
    mockery::mock_args(mock_get)[[3]],
    list("c", NULL, NULL))
})
