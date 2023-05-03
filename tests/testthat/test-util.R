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
