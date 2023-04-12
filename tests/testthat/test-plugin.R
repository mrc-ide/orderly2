test_that("Can run simple example with plugin", {
  path <- test_prepare_orderly_example("plugin")

  env <- new.env()
  set.seed(1)
  id <- orderly_run("plugin", root = path, envir = env)

  set.seed(1)
  cmp <- rnorm(10)

  root <- orderly_root(path, locate = FALSE)
  meta <- root$outpack$metadata(id, full = TRUE)

  ## Our nice vectors have become lists here, due to the general pain
  ## of deserialising json, into R but at least it's all there.
  ## Probably the most general solution involves plugins being able to
  ## provide deserialisers that can apply any required simplification?
  expect_equal(
    meta$custom$orderly$plugins$example.random,
    list(list(as = "dat", mean = mean(cmp), variance = var(cmp))))
  expect_equal(readRDS(file.path(path, "archive", "plugin", id, "data.rds")),
               cmp)
})
