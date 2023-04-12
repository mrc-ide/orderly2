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


test_that("can run interactive example with plugin", {
  path <- test_prepare_orderly_example("plugin")

  env <- new.env()
  set.seed(1)
  path_src <- file.path(path, "src", "plugin")
  withr::with_dir(path_src,
                  sys.source("orderly.R", env))

  set.seed(1)
  cmp <- rnorm(10)

  expect_setequal(dir(path_src), c("data.rds", "orderly.R"))
  expect_equal(readRDS(file.path(path_src, "data.rds")), cmp)
})


test_that("loading plugin triggers package load", {
  skip_if_not_installed("mockery")
  clear_plugins()
  on.exit(clear_plugins())

  mock_load_namespace <- mockery::mock(register_example_plugin())
  mockery::stub(load_orderly_plugin, "loadNamespace", mock_load_namespace)

  plugin <- load_orderly_plugin("example.random")
  mockery::expect_called(mock_load_namespace, 1)
  expect_equal(mockery::mock_args(mock_load_namespace)[[1]],
               list("example.random"))
  expect_s3_class(plugin, "orderly_plugin")
  expect_identical(plugin, .plugins$example.random)
})


test_that("error if load fails to register plugin", {
  skip_if_not_installed("mockery")
  clear_plugins()
  on.exit(clear_plugins())

  mock_load_namespace <- mockery::mock()
  mockery::stub(load_orderly_plugin, "loadNamespace", mock_load_namespace)

  expect_error(load_orderly_plugin("example.random"),
               "Plugin 'example.random' not found")
  mockery::expect_called(mock_load_namespace, 1)
  expect_equal(mockery::mock_args(mock_load_namespace)[[1]],
               list("example.random"))
})


test_that("don't load package if plugin already loaded", {
  register_example_plugin()
  mock_load_namespace <- mockery::mock()
  mockery::stub(load_orderly_plugin, "loadNamespace", mock_load_namespace)
  plugin <- load_orderly_plugin("example.random")
  mockery::expect_called(mock_load_namespace, 0)
  expect_s3_class(plugin, "orderly_plugin")
  expect_identical(plugin, .plugins$example.random)
})
