test_that("prevent missing parameters", {
  expect_error(
    check_parameters(list(), list(a = NULL)),
    "Missing parameters: 'a'")
  expect_error(
    check_parameters(list(), list(a = NULL, b = NULL)),
    "Missing parameters: 'a', 'b'")
  expect_error(
    check_parameters(list(a = 1), list(a = NULL, b = NULL)),
    "Missing parameters: 'b'")
  expect_error(
    check_parameters(list(a = 1), list(a = NULL, b = NULL, c = NULL)),
    "Missing parameters: 'b', 'c'")
  expect_error(
    check_parameters(list(a = 1), list(a = NULL, b = 2, c = NULL)),
    "Missing parameters: 'c'")

  err <- expect_error(
    check_parameters(list(thing = 1), list(things = NULL)),
    "Missing parameters: 'things'")
  expect_equal(
    err$body,
    c(i = "You have extra parameters, possibly misspelt?",
      "*" = "'things': could be your 'thing'"))
})


test_that("prevent extra parameters", {
  err <- expect_error(
    check_parameters(list(a = 1), NULL),
    "Parameters given, but none declared")
  expect_equal(err$body,
               c(i = "Did you forget 'orderly::orderly_parameters()"))

  expect_error(
    check_parameters(list(a = 1, b = 2), list()),
    "Extra parameters: 'a', 'b'")
  expect_error(
    check_parameters(list(a = 1, b = 2), list(a = NULL)),
    "Extra parameters: 'b'")
})


test_that("prevent extra parameters that might be misspelt optional ones", {
  err <- expect_error(
    check_parameters(list(apple = 1), list(apples = 2)),
    "Extra parameters: 'apple'")
  expect_equal(
    err$body,
    c("i" = "You have extra parameters, possibly misspelt?",
      "*" = "'apple': should perhaps be 'apples'"))
})


test_that("combine default and given parameters", {
  expect_equal(
    check_parameters(list(a = 1, b = 2), list(a = NULL, b = NULL)),
    list(a = 1, b = 2))
  expect_equal(
    check_parameters(list(b = 2, a = 1), list(a = 10, b = 20)),
    list(a = 1, b = 2))
  expect_equal(
    check_parameters(list(a = 1), list(a = NULL, b = 20)),
    list(a = 1, b = 20))
  expect_equal(
    check_parameters(NULL, list(a = 10, b = 20)),
    list(a = 10, b = 20))
})


test_that("do nothing when no spec given", {
  envir <- new.env()
  expect_equal(check_parameters_interactive(envir, NULL, NULL),
               strict_list(.name = "parameters"))
  expect_equal(ls(envir), character())
})


## Very unlikely but is at least consistent. The other option would be
## to error if empty parameters are provided.
test_that("return empty pars when no spec given but target present", {
  envir <- new.env()
  expected <- strict_list(.name = "parameters")
  expect_equal(check_parameters_interactive(envir, NULL, "target"),
               expected)
  expect_equal(ls(envir), "target")
  expect_equal(envir$target, expected)
})


test_that("set defaults into environment if missing", {
  envir <- new.env()
  res <- check_parameters_interactive(envir, list(a = 1, b = 2), NULL, TRUE)
  expect_equal(res, strict_list(a = 1, b = 2, .name = "parameters"))
  expect_setequal(names(envir), c("a", "b"))
  expect_equal(envir$a, 1)
  expect_equal(envir$b, 2)
})


test_that("fill in missing parameters", {
  envir <- new.env()
  spec <- list(a = NULL, b = NULL, c = NULL)
  pars <- strict_list(a = 1, b = 2, c = 3, .name = "parameters")
  mock_get <- mockery::mock(envir$pars <- pars)
  mockery::stub(check_parameters_interactive,
                "prompt_parameters_interactive",
                mock_get)

  res <- check_parameters_interactive(envir, spec, "pars", NULL)
  expect_equal(res, pars)
  mockery::expect_called(mock_get, 1)
  expect_equal(mockery::mock_args(mock_get)[[1]], list(spec, NULL))
  expect_equal(as.list(envir), list(pars = pars))

  expect_message(
    res <- check_parameters_interactive(envir, spec, "pars", NULL),
    "Reusing previous parameters in 'pars'")
  expect_equal(res, pars)
  mockery::expect_called(mock_get, 1)
})


test_that("fill in missing parameters", {
  envir <- new.env()
  spec <- list(a = NULL, b = NULL, c = NULL)
  pars <- strict_list(a = 1, b = 2, c = 3, .name = "parameters")
  mock_get <- mockery::mock(envir$pars <- pars)
  mockery::stub(check_parameters_interactive,
                "prompt_parameters_interactive",
                mock_get)

  envir$pars <- strict_list(a = 1, b = 2, .name = "parameters")

  expect_message(
    res <- check_parameters_interactive(envir, spec, "pars", NULL),
    "Ignoring parameters in 'pars' which are different")

  expect_equal(res, pars)
  mockery::expect_called(mock_get, 1)
  expect_equal(mockery::mock_args(mock_get)[[1]], list(spec, NULL))
  expect_equal(as.list(envir), list(pars = pars))
})


test_that("require non-default parameters are present in environment", {
  withr::local_options(orderly_interactive_parameters_missing_error = TRUE)
  envir <- list2env(list(b = 3, c = 4), parent = new.env())
  expect_error(
    get_missing_parameters_interactive(c("a", "b", "c"), NULL, envir),
    "Missing parameters: 'a'")
})


test_that("prompt for missing parameters", {
  withr::local_options(orderly_interactive_parameters_missing_error = NULL)
  envir <- list2env(list(c = 4), parent = new.env())
  mock_get <- mockery::mock(1, 2)
  mockery::stub(get_missing_parameters_interactive, "get_parameter_interactive",
                mock_get)
  expect_message(
    get_missing_parameters_interactive(c("a", "b", "c"), envir),
    "Please enter values for 2 missing parameters:")
  mockery::expect_called(mock_get, 2)
  expect_equal(mockery::mock_args(mock_get),
               list(list("a", NULL, NULL), list("b", NULL, NULL)))
  expect_mapequal(as.list(envir), list(a = 1, b = 2, c = 4))
})


test_that("parameters must be atomic scalars", {
  err <- expect_error(
    check_parameters(list(a = NULL, b = 2), list(a = NULL, b = NULL)),
    "Invalid parameter value\\b")
  expect_equal(
    err$body,
    c("x" = "Values must be scalar, but were not for:",
      "*" = "a",
      "x" = "Values must be character, numeric or boolean, but were not for:",
      "*" = "a"))
  err <- expect_error(
    check_parameters(list(a = NULL, b = 2:10), list(a = NULL, b = NULL)),
    "Invalid parameter values\\b")
  expect_equal(
    err$body,
    c("x" = "Values must be scalar, but were not for:",
      "*" = "a",
      "*" = "b",
      "x" = "Values must be character, numeric or boolean, but were not for:",
      "*" = "a"))
  err <- expect_error(
    check_parameters(list(a = utils::data, b = 2), list(a = NULL, b = NULL)),
    "Invalid parameter value\\b")
  expect_equal(
    err$body,
    c("x" = "Values must be character, numeric or boolean, but were not for:",
      "*" = "a"))
  err <- expect_error(
    check_parameters(list(a = utils::data, b = 2 + 1i),
                     list(a = NULL, b = NULL)),
    "Invalid parameter values\\b")
  expect_equal(
    err$body,
    c("x" = "Values must be character, numeric or boolean, but were not for:",
      "*" = "a",
      "*" = "b"))
})


test_that("parse parameter metadata", {
  expect_null(static_orderly_parameters(list()))
  expect_equal(static_orderly_parameters(list(a = NULL)),
               strict_list(a = NULL, .name = "parameters"))
  expect_equal(static_orderly_parameters(list(a = 1)),
               strict_list(a = 1, .name = "parameters"))
})


test_that("defaults must be valid", {
  err <- expect_error(
    static_orderly_parameters(list(a = 1:2)),
    "Invalid parameter default")
  expect_equal(err$body, c("x" = "Values must be scalar, but were not for:",
                           "*" = "a"))
  err <- expect_error(
    static_orderly_parameters(list(a = utils::data)),
    "Invalid parameter default")
  expect_equal(
    err$body,
    c("x" = "Values must be character, numeric or boolean, but were not for:",
      "*" = "a"))
})
