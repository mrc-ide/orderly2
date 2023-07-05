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


test_that("canonical case: single file", {
  ## There are issues with either mocking or system calls for
  ## canonical case checking on solaris, but as it is case-sensitive
  ## the tests are not important.
  skip_on_solaris()
  root <- tempfile()
  dir.create(root)
  path <- "a"
  PATH <- toupper(path) # nolint
  full <- file.path(root, path)

  dir.create(dirname(full), FALSE, TRUE)
  file.create(full)

  withr::with_dir(root, {
    expect_true(file_has_canonical_case(path))
    expect_equal(file_canonical_case(path), path)
    expect_true(file_exists(path))
    expect_true(file_exists(path, check_case = TRUE))

    expect_false(file_has_canonical_case(PATH))
    expect_equal(file_canonical_case(PATH), path)
  })

  expect_true(file_exists(path, check_case = FALSE, workdir = root))
  expect_true(file_exists(path, check_case = TRUE, workdir = root))

  expect_false(file_exists(PATH, check_case = TRUE, workdir = root))

  if (is_linux()) {
    mockery::stub(file_exists, "file.exists", TRUE)
  }
  expect_true(file_exists(PATH, check_case = FALSE, workdir = root))
  v <- file_exists(PATH, check_case = TRUE, workdir = root,
                   force_case_check = TRUE)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, PATH))
})


test_that("canonical case: relative path", {
  skip_on_solaris() # See above
  root <- tempfile()
  dir.create(root)
  path <- file.path("a", "b", "c")
  PATH <- toupper(path) # nolint
  full <- file.path(root, path)

  dir.create(dirname(full), FALSE, TRUE)
  file.create(full)

  withr::with_dir(root, {
    expect_true(file_has_canonical_case(path))
    expect_equal(file_canonical_case(path), path)
    expect_true(file_exists(path))
    expect_true(file_exists(path, check_case = TRUE))

    expect_false(file_has_canonical_case(PATH))
    expect_equal(file_canonical_case(PATH), path)
  })

  expect_true(file_exists(path, check_case = FALSE, workdir = root))
  expect_true(file_exists(path, check_case = TRUE, workdir = root))

  expect_false(file_exists(PATH, check_case = TRUE, workdir = root))

  if (is_linux()) {
    mockery::stub(file_exists, "file.exists", TRUE)
  }

  expect_true(file_exists(PATH, check_case = FALSE, workdir = root))
  v <- file_exists(PATH, check_case = TRUE, workdir = root,
                   force_case_check = TRUE)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, PATH))
})


test_that("canonical case: absolute path", {
  skip_on_solaris() # See above
  path <- file.path(tempfile(), "a", "b", "c")
  dir.create(dirname(path), FALSE, TRUE)
  file.create(path)
  path <- normalizePath(path, "/")
  PATH <- toupper(path) # nolint
  if (is_windows()) {
    ## On windows, use upper case drive letters here:
    path <- paste0(toupper(substr(path, 1, 1)),
                   substr(path, 2, nchar(path)))
  }

  expect_true(file_has_canonical_case(path))
  expect_equal(file_canonical_case(path), path)
  expect_true(file_exists(path))
  expect_true(file_exists(path, check_case = TRUE))

  expect_false(file_has_canonical_case(PATH))
  expect_equal(file_canonical_case(PATH), path)

  expect_true(file_exists(path, check_case = FALSE))
  expect_true(file_exists(path, check_case = TRUE))

  expect_false(file_exists(PATH, check_case = TRUE))

  if (is_linux()) {
    mockery::stub(file_exists, "file.exists", TRUE)
  }
  expect_true(file_exists(PATH, check_case = FALSE))

  v <- file_exists(PATH, check_case = TRUE, force_case_check = TRUE)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, PATH))
})


test_that("canonical case: path splitting", {
  skip_on_solaris() # See above
  expect_equal(file_split_base("a/b/c"),
               list(path = c("a", "b", "c"), base = ".", absolute = FALSE))
  expect_equal(file_split_base("/a/b/c"),
               list(path = c("a", "b", "c"), base = "/", absolute = TRUE))
  expect_equal(file_split_base("c:/a/b/c"),
               list(path = c("a", "b", "c"), base = "c:/", absolute = TRUE))
  expect_equal(file_split_base("C:/a/b/c"),
               list(path = c("a", "b", "c"), base = "C:/", absolute = TRUE))
})


test_that("canonical case: on missing file", {
  skip_on_solaris() # See above
  expect_equal(file_canonical_case("test-util.R"), "test-util.R")
  expect_identical(file_canonical_case("another file"), NA_character_)
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
