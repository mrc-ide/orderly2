test_that("Can run simple example with plugin", {
  path <- test_prepare_orderly_example("plugin")

  envir <- new.env()
  set.seed(1)
  id <- orderly_run("plugin", root = path, envir = envir)

  set.seed(1)
  cmp <- rnorm(10)

  expect_identical(envir$dat, cmp)

  meta <- orderly_metadata(id, root = path)

  ## Our nice vectors have become lists here, due to the general pain
  ## of deserialising json, into R but at least it's all there.
  ## Probably the most general solution involves plugins being able to
  ## provide deserialisers that can apply any required simplification?
  expect_equal(
    meta$custom$example.random,
    list(list(as = "dat", mean = mean(cmp), variance = var(cmp))))
  expect_equal(readRDS(file.path(path, "archive", "plugin", id, "data.rds")),
               cmp)
})


test_that("can run interactive example with plugin", {
  path <- test_prepare_orderly_example("plugin")

  envir <- new.env()
  set.seed(1)
  path_src <- file.path(path, "src", "plugin")
  withr::with_dir(path_src,
                  sys.source("orderly.R", envir))

  set.seed(1)
  cmp <- rnorm(10)

  expect_identical(envir$dat, cmp)
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
  skip_if_not_installed("mockery")
  register_example_plugin()
  mock_load_namespace <- mockery::mock()
  mockery::stub(load_orderly_plugin, "loadNamespace", mock_load_namespace)
  plugin <- load_orderly_plugin("example.random")
  mockery::expect_called(mock_load_namespace, 0)
  expect_s3_class(plugin, "orderly_plugin")
  expect_identical(plugin, .plugins$example.random)
})


test_that("error if packet uses non-configured plugin", {
  path <- test_prepare_orderly_example("plugin")
  writeLines(empty_config_contents(), file.path(path, "orderly_config.yml"))

  envir <- new.env()
  expect_error(
    orderly_run("plugin", root = path, envir = envir),
    "Plugin 'example.random' not enabled in 'orderly_config.yml'",
    fixed = TRUE)
})


test_that("run cleanup on exit", {
  skip_if_not_installed("mockery")
  clear_plugins()
  on.exit(clear_plugins())

  path <- test_prepare_orderly_example("plugin")
  mock_cleanup <- mockery::mock()
  .plugins$example.random$cleanup <- mock_cleanup

  envir <- new.env()
  set.seed(1)
  id <- orderly_run("plugin", root = path, envir = envir)

  mockery::expect_called(mock_cleanup, 1)
  expect_equal(
    mockery::mock_args(.plugins$example.random$cleanup)[[1]], list())
})


test_that("validate that plugins make sense", {
  skip_if_not_installed("mockery")
  config <- function(...) "config"
  serialise <- function(...) "serialise"
  cleanup <- function(...) "cleanup"
  schema <- withr::local_tempfile(fileext = ".json")
  writeLines("{}", schema)

  mock_pkg_root <- mockery::mock(dirname(schema), cycle = TRUE)
  mockery::stub(orderly_plugin, "pkg_root", mock_pkg_root)

  p <- orderly_plugin("pkg", config, NULL, NULL, NULL)
  expect_identical(p$config, config)
  expect_identical(p$serialise, plugin_no_serialise)
  expect_identical(p$cleanup, plugin_no_cleanup)
  expect_null(p$schema)

  p <- orderly_plugin("pkg", config, serialise, cleanup, basename(schema))
  expect_identical(p$config, config)
  expect_identical(p$serialise, serialise)
  expect_identical(p$cleanup, cleanup)
  expect_equal(p$schema, file.path("pkg", basename(schema)))

  expect_error(
    orderly_plugin("pkg", config, NULL, NULL, basename(schema)),
    "If 'schema' is given, then 'serialise' must be non-NULL")

  unlink(schema)
  expect_error(
    orderly_plugin("pkg", config, serialise, cleanup, basename(schema)),
    sprintf("Expected schema file '%s' to exist in package 'pkg'",
            basename(schema)),
    fixed = TRUE)
})


test_that("default serialise errors if metadata found", {
  js_null <- to_json(NULL, NULL)
  expect_equal(plugin_no_serialise(NULL), js_null)
  expect_equal(plugin_no_serialise(list(a = NULL, b = NULL)), js_null)
  expect_error(
    plugin_no_serialise(list(a = 1, b = 2)),
    "Your plugin produced output to be serialise but has no serialise method")
})


test_that("deal with devmode roots", {
  skip_if_not_installed("mockery")
  mock_find_package <- mockery::mock("/path/to/pkg", cycle = TRUE)
  mock_is_dev_package <- mockery::mock(FALSE, TRUE)
  mockery::stub(pkg_root, "find.package", mock_find_package)
  mockery::stub(pkg_root, "is_dev_package", mock_is_dev_package)
  expect_equal(pkg_root("pkg"), "/path/to/pkg")
  expect_equal(pkg_root("pkg"), file.path("/path/to/pkg", "inst"))
})
