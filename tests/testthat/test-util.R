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
