test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("drop null can return corner case", {
  expect_null(drop_null(list(), NULL))
  expect_null(drop_null(list(NULL), NULL))
  expect_null(drop_null(list(NULL, NULL), NULL))
  expect_equal(drop_null(list(NULL, NULL), list()), list())
})


test_that("drop_null can filter list", {
  expect_equal(drop_null(list("x", NULL)), list("x"))
  expect_equal(drop_null(list("x", "y")), list("x", "y"))
  expect_equal(drop_null(list("x", NULL, "y")), list("x", "y"))
})


test_that("assert_scalar_atomic rejects non-atomic entries", {
  value <- list(1)
  expect_error(assert_scalar_atomic(value),
               "'value' must be atomic (string, numeric, logical)",
               fixed = TRUE)
  expect_silent(assert_scalar_atomic(1))
})


test_that("can resolve environment variables", {
  env <- c(V1 = "a", V2 = "b")
  expect_equal(
    resolve_env(list(A = "$V1", B = "$V2", C = "c"), env, "x"),
    list(A = "a", B = "b", C = "c"))
  expect_equal(
    resolve_env(list(list(A = "$V1", B = "$V2", C = "c")), env, "x"),
    list(list(A = "a", B = "b", C = "c")))
  expect_equal(
    resolve_env(list(list(A = list(a = "$V1"),
                          B = list(b = "$V2"),
                          C = list(c = "c"))), env, "x"),
    list(list(A = list(a = "a"), B = list(b = "b"), C = list(c = "c"))))
})


test_that("can return meaningful errors if lookup fails", {
  env <- c(V1 = "a", V2 = "b")
  expect_error(
    resolve_env(list(A = "$V3", B = "$V2", C = "c"), env, "x"),
    "Environment variable 'V3' is not set\n\t(used in x$A)",
    fixed = TRUE)
  expect_error(
    resolve_env(list(list(A = "$V3", B = "$V2", C = "c")), env, "x"),
    "Environment variable 'V3' is not set\n\t(used in x[[1]]$A)",
    fixed = TRUE)
  expect_error(
    resolve_env(list(list(A = list(a = "$V1"),
                          B = list(b = "$V3"),
                          C = list(c = "c"))), env, "x"),
    "Environment variable 'V3' is not set\n\t(used in x[[1]]$B$b)",
    fixed = TRUE)
})


test_that("can ignore errors and substitute in NA values", {
  env <- c(V1 = "a", V2 = "b")
  expect_equal(
    resolve_env(list(A = "$V3", B = "$V2", C = "c"), env, "x", FALSE),
    list(A = NA_character_, B = "b", C = "c"))
  expect_equal(
    resolve_env(list(list(A = "$V3", B = "$V2", C = "c")), env, "x", FALSE),
    list(list(A = NA_character_, B = "b", C = "c")))
  expect_equal(
    resolve_env(list(list(A = list(a = "$V1"),
                          B = list(b = "$V3"),
                          C = list(c = "c"))), env, "x", FALSE),
    list(list(A = list(a = "a"),
              B = list(b = NA_character_),
              C = list(c = "c"))))
})


test_that("yaml_read throws nicely", {
  expect_error(suppressWarnings(yaml_read("foo")), "while reading 'foo'")
})


test_that("sys_getenv", {
  withr::with_envvar(
    c("SOME_VAR" = NA_character_), {
      expect_error(
        sys_getenv("SOME_VAR", "loc"),
        "Environment variable 'SOME_VAR' is not set.*used in loc")
      expect_null(sys_getenv("SOME_VAR", "loc", FALSE))
      expect_identical(sys_getenv("SOME_VAR", "loc", FALSE, NA_character_),
                       NA_character_)
    })

  ## On windows if env variable is empty then windows will return NA from call
  ## to Sys.getenv
  if (is_windows()) {
    expected_err <- "Environment variable 'SOME_VAR' is not set.*used in loc"
  } else {
    expected_err <- "Environment variable 'SOME_VAR' is empty.*used in loc"
  }

  withr::with_envvar(
    c("SOME_VAR" = ""),
    expect_error(
      sys_getenv("SOME_VAR", "loc"),
      expected_err))
  withr::with_envvar(
    c("SOME_VAR" = "x"),
    expect_identical(sys_getenv("SOME_VAR", "loc"), "x"))
})


test_that("check fields", {
  expect_silent(check_fields(list(), "x", character(), character()))

  expect_silent(check_fields(list(a = 1), "x", "a", character()))
  expect_silent(check_fields(list(a = 1), "x", character(), "a"))

  expect_silent(check_fields(list(a = 1, b = 1), "x", "a", "b"))
  expect_silent(check_fields(list(a = 1, b = 1), "x", c("a", "b"), character()))
  expect_silent(check_fields(list(a = 1, b = 1), "x", character(), letters))

  expect_error(
    check_fields(list(a = 1, b = 2), "x", c("a", "b", "c"), character()),
    "Fields missing from x: c")
  expect_error(
    check_fields(list(a = 1, b = 2), "x", c("a", "b", "c", "d"), character()),
    "Fields missing from x: c, d")

  expect_error(
    check_fields(list(a = 1, b = 2), "x", "a", character()),
    "Unknown fields in x: b")
  expect_error(
    check_fields(list(a = 1, b = 2, c = 3, d = 4), "x", "a", "b"),
    "Unknown fields in x: c, d")
})


test_that("Descend failure", {
  path <- temp_file()
  dir.create(path)
  expect_null(find_file_descend(".orderly_foobar", tempdir(), path))
  expect_null(find_file_descend(".orderly_foobar", "/", path))
  expect_null(find_file_descend(".orderly_foobar", "/", "/"))
})


test_that("can get near matches", {
  x <- c("apples", "applez", "appell", "applly")
  expect_equal(
    near_match("apple", x),
    c("apples", "applez", "appell", "applly"))
  expect_equal(
    near_match("apple", x, 1),
    c("apples", "applez"))
  expect_equal(
    near_match("apple", x, 2, 3),
    c("apples", "applez", "appell"))
})
