test_that("can run simple task", {
  path <- test_prepare_orderly_example("implicit")
  env <- new.env()
  orderly_run("implicit", root = path, envir = env)
})
