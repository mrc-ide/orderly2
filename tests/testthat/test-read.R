test_that("can read file with no helpers", {
  expect_equal(orderly_read_r("examples/implicit/orderly.R"), list())
})


test_that("can read file with helpers", {
  dat <- orderly_read_r("examples/explicit/orderly.R")
  expect_setequal(names(dat), c("resource", "artefacts"))
  expect_equal(dat$resource, "data.csv")
  expect_equal(dat$artefacts,
               list(list(description = "A graph of things",
                         files = "mygraph.png")))
})
