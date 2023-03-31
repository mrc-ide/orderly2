test_that("can run simple task with explicit inputs and outputs", {
  path <- test_prepare_orderly_example("explicit")
  env <- new.env()
  id <- orderly_run("explicit", root = path, envir = env)
  expect_type(id, "character")
  expect_length(id, 1)
  expect_match(id, "^[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}$")

  ## All outputs as expected
  path_res <- file.path(path, "archive", "explicit", id)
  expect_true(is_directory(path_res))
  expect_setequal(dir(path_res), c("orderly.R", "mygraph.png", "data.csv"))

  ## Nothing left in drafts
  expect_true(is_directory(file.path(path, "draft", "explicit")))
  expect_false(file.exists(file.path(path, "draft", "explicit", id)))

  ## Nothing extra in src
  expect_setequal(dir(file.path(path, "src", "explicit")),
                  c("orderly.R", "data.csv"))

  root <- orderly_root(path, FALSE)
  idx <- root$outpack$index()
  expect_equal(names(idx$metadata), id)
  meta <- root$outpack$metadata(id, full = TRUE)

  expect_equal(
    meta$custom$orderly$role,
    list(list(path = "data.csv", role = "resource")))
  expect_equal(
    meta$custom$orderly$artefacts,
    list(list(description = "A graph of things",
              paths = list("mygraph.png"))))
})


test_that("can run simple task with implicit inputs and outputs", {
  path <- test_prepare_orderly_example("implicit")
  env <- new.env()
  id <- orderly_run("implicit", root = path, envir = env)
  expect_type(id, "character")
  expect_length(id, 1)
  expect_match(id, "^[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}$")

  ## All outputs as expected
  path_res <- file.path(path, "archive", "implicit", id)
  expect_true(is_directory(path_res))
  expect_setequal(dir(path_res), c("orderly.R", "mygraph.png", "data.csv"))

  ## Nothing left in drafts
  expect_true(is_directory(file.path(path, "draft", "implicit")))
  expect_false(file.exists(file.path(path, "draft", "implicit", id)))

  ## Nothing extra in src
  expect_setequal(dir(file.path(path, "src", "implicit")),
                  c("orderly.R", "data.csv"))

  root <- orderly_root(path, FALSE)
  idx <- root$outpack$index()
  expect_equal(names(idx$metadata), id)
  meta <- root$outpack$metadata(id, full = TRUE)

  expect_equal(meta$custom$orderly$role, list())
  expect_equal(meta$custom$orderly$artefacts, list())
})


test_that("error if declared artefacts are not produced", {
  path <- test_prepare_orderly_example("explicit")
  env <- new.env()
  path_src <- file.path(path, "src", "explicit", "orderly.R")
  code <- readLines(path_src)
  writeLines(c(
    'orderly3::orderly_artefact("some data", "output.csv")',
    code),
    path_src)
  expect_error(
    orderly_run("explicit", root = path, envir = env),
    "Script did not produce expected artefacts: 'output.csv'")
})


test_that("Can run explicit case without orderly", {
  path <- test_prepare_orderly_example("explicit")
  env <- new.env()
  path_src <- file.path(path, "src", "explicit")
  withr::with_dir(path_src,
                  sys.source("orderly.R", env))
  expect_setequal(dir(path_src),
                  c("data.csv", "orderly.R", "mygraph.png"))
})


test_that("cope with computed values in static functions", {
  path <- test_prepare_orderly_example("computed-resource")
  env <- new.env()
  id <- orderly_run("computed-resource", root = path, envir = env)
  expect_setequal(
    dir(file.path(path, "archive", "computed-resource", id)),
    c("data.csv", "mygraph.png", "orderly.R"))
})


test_that("run a packet with parameters", {
  path <- test_prepare_orderly_example("parameters")
  env <- new.env()
  id <- orderly_run("parameters", parameters = list(a = 10, b = 20, c = 30),
                    envir = env, root = path)
  path_rds <- file.path(path, "archive", "parameters", id, "data.rds")
  expect_true(file.exists(path_rds))
  expect_equal(readRDS(path_rds), list(a = 10, b = 20, c = 30))
})


test_that("fall back on parameter defaults", {
  path <- test_prepare_orderly_example("parameters")
  env <- new.env()
  id <- orderly_run("parameters", parameters = list(a = 10, c = 30),
                    envir = env, root = path)

  path_rds <- file.path(path, "archive", "parameters", id, "data.rds")
  expect_true(file.exists(path_rds))
  expect_equal(readRDS(path_rds), list(a = 10, b = 2, c = 30))
})


test_that("can run orderly with parameters, without orderly", {
  path <- test_prepare_orderly_example("parameters")
  env <- list2env(list(a = 10, c = 30), parent = new.env())
  path_src <- file.path(path, "src", "parameters")
  withr::with_dir(path_src,
                  sys.source("orderly.R", env))

  path_rds <- file.path(path_src, "data.rds")
  expect_true(file.exists(path_rds))
  expect_equal(readRDS(path_rds), list(a = 10, b = 2, c = 30))
})


test_that("Can run simple case with dependency", {
  path <- test_prepare_orderly_example(c("explicit", "depends"))
  env1 <- new.env()
  id1 <- orderly_run("explicit", root = path, envir = env1)
  env2 <- new.env()
  id2 <- orderly_run("depends", root = path, envir = env2)

  path1 <- file.path(path, "archive", "explicit", id1)
  path2 <- file.path(path, "archive", "depends", id2)

  expect_true(file.exists(file.path(path2, "graph.png")))
  expect_equal(
    unname(tools::md5sum(file.path(path2, "graph.png"))),
    unname(tools::md5sum(file.path(path1, "mygraph.png"))))
})


test_that("Can run dependencies case without orderly", {
  skip("needs work")
  path <- test_prepare_orderly_example(c("explicit", "depends"))
  env1 <- new.env()
  id1 <- orderly_run("explicit", root = path, envir = env1)

  env2 <- new.env()
  path_src <- file.path(path, "src", "depends")
  withr::with_dir(path_src,
                  sys.source("orderly.R", env2))
  expect_setequal(dir(path_src), c("orderly.R", "graph.png"))
})
