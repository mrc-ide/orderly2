test_that("can read file with no helpers", {
  expect_equal(orderly_read_r("examples/implicit/orderly.R"), list())
})


test_that("can read file with helpers", {
  dat <- orderly_read_r("examples/explicit/orderly.R")
  expect_setequal(names(dat), c("resources", "artefacts"))
  expect_equal(dat$resources, "data.csv")
  expect_equal(dat$artefacts,
               list(list(description = "A graph of things",
                         files = "mygraph.png")))
})


test_that("Can read string literals from expressions", {
  expect_equal(static_character_vector(quote("x")), "x")
  expect_equal(static_character_vector(quote(c("x"))), "x")
  expect_equal(static_character_vector(quote(c("x", "y"))), c("x", "y"))
  expect_equal(static_character_vector(quote(c("x", c("y", "z")))),
               c("x", "y", "z"))

  expect_null(static_character_vector(quote(a)))
  expect_null(static_character_vector(quote(c(a))))
  expect_null(static_character_vector(quote(c(a, "x"))))
  expect_null(static_character_vector(quote(c(a, b))))
  expect_null(static_character_vector(quote(c("x", c(a, b)))))
  expect_null(static_character_vector(quote(c("x", c("y", b)))))
  expect_null(static_character_vector(quote(c(a, c("x", "y")))))
})
