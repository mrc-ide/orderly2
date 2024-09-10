test_that("assert_simple_scalar_atomic", {
  expect_silent(assert_simple_scalar_atomic(TRUE))
  expect_silent(assert_simple_scalar_atomic(1))
  expect_silent(assert_simple_scalar_atomic("a"))
  expect_error(assert_simple_scalar_atomic(list(1)), "must be atomic")
})
