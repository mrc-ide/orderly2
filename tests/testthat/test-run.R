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
  path <- test_prepare_orderly_example(c("explicit", "depends"))
  env1 <- new.env()
  id1 <- orderly_run("explicit", root = path, envir = env1)

  env2 <- new.env()
  path_src <- file.path(path, "src", "depends")
  withr::with_dir(path_src,
                  sys.source("orderly.R", env2))
  expect_setequal(dir(path_src), c("orderly.R", "graph.png"))
  expect_equal(
    unname(tools::md5sum(file.path(path_src, "graph.png"))),
    unname(tools::md5sum(file.path(path, "archive", "explicit", id1,
                                   "mygraph.png"))))
})


test_that("can run with global resources", {
  path <- test_prepare_orderly_example("global")
  env <- new.env()
  id <- orderly_run("global", root = path, envir = env)
  expect_setequal(
    dir(file.path(path, "archive", "global", id)),
    c("global_data.csv", "mygraph.png", "orderly.R"))
  root <- orderly_root(path, FALSE)
  meta <- root$outpack$metadata(id, full = TRUE)
  expect_length(meta$custom$orderly$global, 1)
  expect_mapequal(meta$custom$orderly$global[[1]],
                  list(here = "global_data.csv", there = "data.csv"))
  expect_equal(
    meta$custom$orderly$role,
    list(list(path = "global_data.csv", role = "global")))
})


test_that("can run manually with global resources", {
  path <- test_prepare_orderly_example("global")
  env <- new.env()
  path_src <- file.path(path, "src", "global")
  withr::with_dir(path_src,
                  sys.source("orderly.R", env))
  expect_setequal(
    dir(path_src),
    c("global_data.csv", "mygraph.png", "orderly.R"))
})


test_that("can validate global resource arguments", {
  expect_error(
    validate_global_resource(list()),
    "orderly_global_resource requires at least one argument")
  expect_error(
    validate_global_resource(list(input = c("a", "b"))),
    "Invalid global resource 'input': entries must be strings")
  expect_error(
    validate_global_resource(list(a = 1, b = TRUE, c = "str")),
    "Invalid global resource 'a', 'b': entries must be strings")
  expect_equal(
    validate_global_resource(list(a = "A", b = "B")),
    c(a = "A", b = "B"))
})


test_that("can't use global resources if not enabled", {
  path <- test_prepare_orderly_example("global")
  file.create(file.path(path, "orderly_config.yml")) # truncates file
  env <- new.env()
  path_src <- file.path(path, "src", "global")
  err <- expect_error(
    orderly_run("global", root = path, envir = env),
    "'global_resources' is not supported; please edit orderly_config.yml")
  expect_error(
    withr::with_dir(path_src, sys.source("orderly.R", env)),
    err$message, fixed = TRUE)
})


test_that("global resources can be directories", {
  path <- test_prepare_orderly_example("global-dir")
  write.csv(mtcars, file.path(path, "global/data/mtcars.csv"),
            row.names = FALSE)
  write.csv(iris, file.path(path, "global/data/iris.csv"),
            row.names = FALSE)

  env <- new.env()
  id <- orderly_run("global-dir", root = path, envir = env)

  expect_setequal(
    dir(file.path(path, "archive", "global-dir", id)),
    c("global_data", "output.rds", "orderly.R"))
  expect_setequal(
    dir(file.path(path, "archive", "global-dir", id, "global_data")),
    c("iris.csv", "mtcars.csv"))
  root <- orderly_root(path, FALSE)
  meta <- root$outpack$metadata(id, full = TRUE)
  expect_length(meta$custom$orderly$global, 2)
  expect_mapequal(
    meta$custom$orderly$global[[1]],
    list(here = "global_data/iris.csv", there = "data/iris.csv"))
  expect_mapequal(
    meta$custom$orderly$global[[2]],
    list(here = "global_data/mtcars.csv", there = "data/mtcars.csv"))
  expect_equal(
    meta$custom$orderly$role,
    list(list(path = "global_data/iris.csv", role = "global"),
         list(path = "global_data/mtcars.csv", role = "global")))
  d <- readRDS(file.path(path, "archive", "global-dir", id, "output.rds"))
  expect_equal(
    d,
    list(iris = read.csv(file.path(path, "global/data/iris.csv")),
         mtcars = read.csv(file.path(path, "global/data/mtcars.csv"))))
})


test_that("can use subqueries in orderly run", {
  path <- test_prepare_orderly_example(c("explicit", "depends", "depends2"))

  env_explicit1 <- new.env()
  id_explicit1 <- orderly_run("explicit", root = path, envir = env_explicit1)
  env_depends1 <- new.env()
  id_depends1 <- orderly_run("depends", root = path, envir = env_depends1)
  env_explicit2 <- new.env()
  id_explicit2 <- orderly_run("explicit", root = path, envir = env_explicit2)

  env_depends2 <- new.env()
  id_depends2 <- orderly_run("depends2", root = path, envir = env_depends2)

  path_depends2 <- file.path(path, "archive", "depends2", id_depends2)
  expect_true(file.exists(path_depends2))
  expect_true(file.exists(file.path(path_depends2, "depends_graph.png")))
  expect_true(file.exists(file.path(path_depends2, "explicit_graph.png")))
  expect_true(file.exists(file.path(path_depends2, "all_plots.zip")))
  expect_setequal(
    zip::zip_list(file.path(path_depends2, "all_plots.zip"))$filename,
    c("depends_graph.png", "explicit_graph.png"))

  ## "explicit" dependency was pulled in from first id i.e. the version of
  ## "explicit" used by "depends"
  path_explicit1 <- file.path(path, "archive", "explicit", id_explicit1)
  expect_equal(
    unname(tools::md5sum(file.path(path_depends2, "explicit_graph.png"))),
    unname(tools::md5sum(file.path(path_explicit1, "mygraph.png"))))
  path_explicit2 <- file.path(path, "archive", "explicit", id_explicit2)
  expect_true(
    unname(tools::md5sum(file.path(path_depends2, "explicit_graph.png"))) !=
      unname(tools::md5sum(file.path(path_explicit2, "mygraph.png"))))
})


test_that("can use subqueries in interactive run", {
  path <- test_prepare_orderly_example(c("explicit", "depends", "depends2"))

  env_explicit1 <- new.env()
  id_explicit1 <- orderly_run("explicit", root = path, envir = env_explicit1)
  env_depends1 <- new.env()
  id_depends1 <- orderly_run("depends", root = path, envir = env_depends1)
  env_explicit2 <- new.env()
  id_explicit2 <- orderly_run("explicit", root = path, envir = env_explicit2)

  env_depends2 <- new.env()
  path_src <- file.path(path, "src", "depends2")
  withr::with_dir(path_src, sys.source("orderly.R", env_depends2))

  path_depends2 <- file.path(path, "archive", "depends2", id_depends2)
  expect_true(file.exists(path_depends2))
  expect_true(file.exists(file.path(path_depends2, "depends_graph.png")))
  expect_true(file.exists(file.path(path_depends2, "explicit_graph.png")))
  expect_true(file.exists(file.path(path_depends2, "all_plots.zip")))
  expect_setequal(
    zip::zip_list(file.path(path_depends2, "all_plots.zip"))$filename,
    c("depends_graph.png", "explicit_graph.png"))

  ## "explicit" dependency was pulled in from first id i.e. the version of
  ## "explicit" used by "depends"
  path_explicit1 <- file.path(path, "archive", "explicit", id_explicit1)
  expect_equal(
    unname(tools::md5sum(file.path(path_depends2, "explicit_graph.png"))),
    unname(tools::md5sum(file.path(path_explicit1, "mygraph.png"))))
  path_explicit2 <- file.path(path, "archive", "explicit", id_explicit2)
  expect_true(
    unname(tools::md5sum(file.path(path_depends2, "explicit_graph.png"))) !=
      unname(tools::md5sum(file.path(path_explicit2, "mygraph.png"))))
})


test_that("Can query for dependencies", {
  path <- test_prepare_orderly_example(NULL)
  create_random_report(path, "a")
  create_random_report(path, "b", list(a = "latest"))

  env_a1 <- new.env()
  id_a1 <- orderly_run("a", root = path, envir = env_a1)
  env_b1 <- new.env()
  id_b1 <- orderly_run("b", root = path, envir = env_b1)

  path_a1 <- file.path(path, "archive", "a", id_a1)
  path_b1 <- file.path(path, "archive", "b", id_b1)

  expect_true(file.exists(file.path(path_a1, "data.rds")))
  expect_true(file.exists(file.path(path_b1, "input1.rds")))
  expect_true(file.exists(file.path(path_b1, "data.rds")))
  ## b1 depends on a1
  expect_equal(
    unname(tools::md5sum(file.path(path_b1, "input1.rds"))),
    unname(tools::md5sum(file.path(path_a1, "data.rds"))))


  ## Can search for reports usedby another
  create_random_report(path, "c", list(a = "usedby(latest(name == \\\"b\\\"))"))
  env_c1 <- new.env()
  id_c1 <- orderly_run("c", root = path, envir = env_c1)
  path_c1 <- file.path(path, "archive", "c", id_c1)

  ## c1 depends on a1
  expect_equal(
    unname(tools::md5sum(file.path(path_c1, "input1.rds"))),
    unname(tools::md5sum(file.path(path_a1, "data.rds"))))

  ## Can search for reports which use another
  create_random_report(path, "d", list(b = "uses(latest(name == \\\"a\\\"))"))
  env_d1 <- new.env()
  id_d1 <- orderly_run("d", root = path, envir = env_d1)
  path_d1 <- file.path(path, "archive", "d", id_d1)

  ## d1 depends on b1
  expect_equal(
    unname(tools::md5sum(file.path(path_d1, "input1.rds"))),
    unname(tools::md5sum(file.path(path_b1, "data.rds"))))
})


test_that("useful error returned when dependency query not satisfied", {
  path <- test_prepare_orderly_example(NULL)
  create_random_report(path, "a")
  create_random_report(path, "b", list(a = "latest"))

  env_a1 <- new.env()
  id_a1 <- orderly_run("a", root = path, envir = env_a1)
  env_b1 <- new.env()
  id_b1 <- orderly_run("b", root = path, envir = env_b1)
  env_a2 <- new.env()
  id_a2 <- orderly_run("a", root = path, envir = env_a2)

  ## No report satisfies query as no report built from id_a2
  create_random_report(path, "c", list(b = "uses(latest(name == \\\"a\\\"))"))
  env_c1 <- new.env()
  expect_error(orderly_run("c", root = path, envir = env_c1),
               "Found no packets")
})
