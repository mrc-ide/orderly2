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


test_that("Skip over computed resources", {
  dat <- orderly_read_r("examples/computed-resource/orderly.R")
  expect_null(dat$resources)
})


test_that("Can read string vector literals from expressions", {
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


test_that("Can read string from expressions", {
  expect_equal(static_string(quote("x")), "x")
  expect_equal(static_string(quote(c("x"))), "x")

  expect_null(static_string(quote(a)))
  expect_null(static_string(quote(c(a))))
})


test_that("read dependency", {
  args <- list(name = "a", query = "latest", use = c(x = "y"))
  expect_equal(static_orderly_dependency(args), args)

  expect_null(
    static_orderly_dependency(list(name = quote(a),
                                   query = "latest",
                                   use = c(x = "y"))))
  expect_null(
    static_orderly_dependency(list(name = "a",
                                   query = quote(latest),
                                   use = c(x = "y"))))
  expect_null(
    static_orderly_dependency(list(name = "a",
                                   query = "latest",
                                   use = quote(use))))
})
