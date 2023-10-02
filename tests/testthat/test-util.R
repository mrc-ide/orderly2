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


test_that("assert_simple_scalar_atomic rejects non-atomic entries", {
  value <- list(1)
  expect_error(assert_simple_scalar_atomic(value),
               "'value' must be atomic (string, numeric, logical)",
               fixed = TRUE)
  expect_silent(assert_simple_scalar_atomic(1))
})


test_that("can resolve environment variables", {
  variables <- c(V1 = "a", V2 = "b")
  expect_equal(
    resolve_envvar(list(A = "$V1", B = "$V2", C = "c"), variables, "x"),
    list(A = "a", B = "b", C = "c"))
  expect_equal(
    resolve_envvar(list(list(A = "$V1", B = "$V2", C = "c")), variables, "x"),
    list(list(A = "a", B = "b", C = "c")))
  expect_equal(
    resolve_envvar(list(list(A = list(a = "$V1"),
                             B = list(b = "$V2"),
                             C = list(c = "c"))), variables, "x"),
    list(list(A = list(a = "a"), B = list(b = "b"), C = list(c = "c"))))
})


test_that("can return meaningful errors if lookup fails", {
  variables <- c(V1 = "a", V2 = "b")
  expect_error(
    resolve_envvar(list(A = "$V3", B = "$V2", C = "c"), variables, "x"),
    "Environment variable 'V3' is not set\n\t(used in x$A)",
    fixed = TRUE)
  expect_error(
    resolve_envvar(list(list(A = "$V3", B = "$V2", C = "c")), variables, "x"),
    "Environment variable 'V3' is not set\n\t(used in x[[1]]$A)",
    fixed = TRUE)
  expect_error(
    resolve_envvar(list(list(A = list(a = "$V1"),
                             B = list(b = "$V3"),
                             C = list(c = "c"))), variables, "x"),
    "Environment variable 'V3' is not set\n\t(used in x[[1]]$B$b)",
    fixed = TRUE)
})


test_that("can ignore errors and substitute in NA values", {
  variables <- c(V1 = "a", V2 = "b")
  expect_equal(
    resolve_envvar(list(A = "$V3", B = "$V2", C = "c"), variables, "x", FALSE),
    list(A = NA_character_, B = "b", C = "c"))
  expect_equal(
    resolve_envvar(list(list(A = "$V3", B = "$V2", C = "c")),
                   variables, "x", FALSE),
    list(list(A = NA_character_, B = "b", C = "c")))
  expect_equal(
    resolve_envvar(list(list(A = list(a = "$V1"),
                             B = list(b = "$V3"),
                             C = list(c = "c"))), variables, "x", FALSE),
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

  ## On windows if environment variable is empty then windows will
  ## return NA from call to Sys.getenv
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


test_that("validate namespaced symbol strings", {
  expect_equal(check_symbol_from_str("a::b", "x"),
               list(namespace = "a", symbol = "b"))
  expect_error(check_symbol_from_str("b", "x"),
               "Expected fully qualified name for 'x'")
  expect_error(check_symbol_from_str("a:::b", "x"),
               "Expected fully qualified name for 'x'")
  expect_error(check_symbol_from_str("a::b::c", "x"),
               "Expected fully qualified name for 'x'")
})


test_that("can perform simple string interpolation", {
  envir <- list2env(list(a = 1, b = "banana", c = "carrot"),
                    parent = emptyenv())

  expect_equal(string_interpolate_simple("hello", envir), "hello")
  expect_equal(string_interpolate_simple("${hello", envir), "${hello")
  expect_equal(string_interpolate_simple("a ${b} c", envir),
               "a banana c")
  expect_equal(string_interpolate_simple("a ${b} ${c}", envir),
               "a banana carrot")
  expect_equal(string_interpolate_simple("a ${b} ${ b }", envir),
               "a banana banana")

  expect_equal(string_interpolate_simple(
    c("${a}/${b}", "${a}/${c}"), envir),
    c("1/banana", "1/carrot"))
  expect_equal(string_interpolate_simple(
    I(c("${a}/${b}", "${a}/${c}")), envir),
    I(c("${a}/${b}", "${a}/${c}")))
})


test_that("prevent problematic string interpolations", {
  envir <- list2env(
    list(a = NULL, b = "${a}", c = letters, d = "${d}", f = args),
    parent = emptyenv())

  err <- expect_error(
    string_interpolate_simple("a/${b}/c", envir),
    "Can't perform recursive string interpolation")
  expect_equal(
    err$body,
    c(x = "Tried to substitute '${b}' to '${a}'",
      i = "Was interpolating string 'a/${b}/c'",
      i = "Don't use '${...}' within the values you are substituting to"))

  err <- expect_error(
    string_interpolate_simple("a/${b}/${d}", envir),
    "Can't perform recursive string interpolation")
  expect_equal(
    err$body,
    c(x = "Tried to substitute '${b}' to '${a}'",
      x = "Tried to substitute '${d}' to '${d}'",
      i = "Was interpolating string 'a/${b}/${d}'",
      i = "Don't use '${...}' within the values you are substituting to"))

  err <- expect_error(
    string_interpolate_simple("a/${a}", envir),
    "Failed to convert string interpolation variable to string")
  expect_equal(
    err$body,
    c(x = "Failed when retrieving 'a' which has length 0",
      i = "Was interpolating string 'a/${a}'",
      i = "All values in ${...} must refer to strings"))

  err <- expect_error(
    string_interpolate_simple("a/${x}", envir),
    "Failed to find value for 'x'")
  expect_equal(err$body, c("i" = "Was interpolating string 'a/${x}'"))

  err <- expect_error(
    string_interpolate_simple("a/${f}", envir),
    "Failed to convert 'f' to character")
  msg <- tryCatch(as.character(args), error = identity)
  expect_equal(err$body,
               c(x = msg$message,
                 "i" = "Was interpolating string 'a/${f}'"))
})


test_that("pairs generates pairs", {
  expect_equal(pairs(c("a", "b")), list(c("a", "b")))
  expect_equal(pairs(c("a", "b", "c")),
               list(c("a", "b"), c("a", "c"), c("b", "c")))
  expect_equal(pairs(c("a", "b", "c", "d")),
               list(c("a", "b"), c("a", "c"), c("b", "c"),
                    c("a", "d"), c("b", "d"), c("c", "d")))
})


test_that("can print pretty bytes", {
  expect_equal(pretty_bytes(100), "100 B")
  expect_equal(pretty_bytes(5000), "5 kB")
  expect_equal(pretty_bytes(5123), "5.1 kB")
  expect_equal(pretty_bytes(5000000), "5 MB")
  expect_equal(pretty_bytes(5123456), "5.1 MB")
  expect_equal(pretty_bytes(5000000000), "5,000 MB")
  expect_equal(pretty_bytes(5123456789), "5,123.5 MB")
})


test_that("set_names copes with common pathologies", {
  expect_equal(set_names(character(), "x"),
               structure(character(), names = character()))
  expect_equal(set_names("a", "x"),
               c("x" = "a"))
  expect_equal(set_names(c("a", "b"), "x"),
               c("x" = "a", x = "b"))
  expect_equal(set_names(c("a", "b"), c("x", "y")),
               c("x" = "a", y = "b"))
  expect_null(set_names(NULL, "x"))
})


test_that("can collapse with special last case", {
  x <- c("x", "y", "z")
  expect_equal(collapse(x), "x, y, z")
  expect_equal(collapse(x, " or "), "x, y or z")
  expect_equal(collapse(x[1:2], " or "), "x or y")
  expect_equal(collapse(x[1], " or "), "x")
  expect_equal(collapse(x[0], " or "), "")
})
