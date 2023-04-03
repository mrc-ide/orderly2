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
})


test_that("prevent extra parameters", {
  expect_error(
    check_parameters(list(a = 1), NULL),
    "Extra parameters: 'a'")
  expect_error(
    check_parameters(list(a = 1, b = 2), NULL),
    "Extra parameters: 'a', 'b'")
  expect_error(
    check_parameters(list(a = 1, b = 2), list(a = NULL)),
    "Extra parameters: 'b'")
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
  env <- new.env()
  expect_null(check_parameters_interactive(env, NULL))
  expect_equal(ls(env), character())
})


test_that("set defaults into environment if missing", {
  env <- new.env()
  check_parameters_interactive(env, list(a = 1, b = 2))
  expect_setequal(names(env), c("a", "b"))
  expect_equal(env$a, 1)
  expect_equal(env$b, 2)
})


test_that("require non-default parameters are present in environment", {
  env <- list2env(list(b = 3, c = 4), parent = new.env())
  expect_error(
    check_parameters_interactive(env, list(a = NULL, b = NULL, c = NULL)),
    "Missing parameters: 'a'")
})


test_that("parameters must be atomic scalars", {
  expect_error(
    check_parameters(list(a = NULL, b = 2), list(a = NULL, b = NULL)),
    "Invalid parameters: 'a' - must be scalar")
  expect_error(
    check_parameters(list(a = NULL, b = 2:10), list(a = NULL, b = NULL)),
    "Invalid parameters: 'a', 'b' - must be scalar")
  expect_error(
    check_parameters(list(a = data, b = 2), list(a = NULL, b = NULL)),
    "Invalid parameters: 'a' - must be character, numeric or logical")
  expect_error(
    check_parameters(list(a = data, b = 2 + 1i), list(a = NULL, b = NULL)),
    "Invalid parameters: 'a', 'b' - must be character, numeric or logical")
})


test_that("parse parameter metadata", {
  expect_null(static_orderly_parameters(list()))
  expect_equal(static_orderly_parameters(list(a = NULL)),
               list(a = NULL))
  expect_equal(static_orderly_parameters(list(a = 1)),
               list(a = 1))
})


test_that("defaults must be valid", {
  expect_error(
    static_orderly_parameters(list(a = 1:2)),
    "Invalid parameter defaults: 'a' - must be scalar")
  expect_error(
    static_orderly_parameters(list(a = data)),
    "Invalid parameter defaults: 'a' - must be character, numeric or logical")
})
