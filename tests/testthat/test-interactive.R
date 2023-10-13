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
