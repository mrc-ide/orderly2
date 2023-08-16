test_that("can run simple task with explicit inputs and outputs", {
  path <- test_prepare_orderly_example("explicit")
  envir <- new.env()
  id <- orderly_run_quietly("explicit", root = path, envir = envir)
  expect_type(id, "character")
  expect_length(id, 1)
  expect_match(id, "^[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}$")

  ## All outputs as expected
  path_res <- file.path(path, "archive", "explicit", id)
  expect_true(is_directory(path_res))
  expect_setequal(dir(path_res),
                  c("orderly.R", "mygraph.png", "data.csv"))

  ## Nothing left in drafts
  expect_true(is_directory(file.path(path, "draft", "explicit")))
  expect_false(file.exists(file.path(path, "draft", "explicit", id)))

  ## Nothing extra in src
  expect_setequal(dir(file.path(path, "src", "explicit")),
                  c("orderly.R", "data.csv"))

  meta <- orderly_metadata(id, root = path)

  expect_equal(
    meta$custom$orderly$role,
    data_frame(path = c("orderly.R", "data.csv"),
               role = c("orderly", "resource")))
  expect_equal(
    meta$custom$orderly$artefacts,
    data_frame(description = "A graph of things",
               paths = I(list(list("mygraph.png")))))
})


test_that("can run simple task with implicit inputs and outputs", {
  path <- test_prepare_orderly_example("implicit")
  envir <- new.env()
  id <- orderly_run_quietly("implicit", root = path, envir = envir)
  expect_type(id, "character")
  expect_length(id, 1)
  expect_match(id, "^[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}$")

  ## All outputs as expected
  path_res <- file.path(path, "archive", "implicit", id)
  expect_true(is_directory(path_res))
  expect_setequal(dir(path_res),
                  c("orderly.R", "mygraph.png", "data.csv"))

  ## Nothing left in drafts
  expect_true(is_directory(file.path(path, "draft", "implicit")))
  expect_false(file.exists(file.path(path, "draft", "implicit", id)))

  ## Nothing extra in src
  expect_setequal(dir(file.path(path, "src", "implicit")),
                  c("orderly.R", "data.csv"))

  meta <- orderly_metadata(id, root = path)

  expect_equal(meta$custom$orderly$role,
               data_frame(path = "orderly.R",
                          role = "orderly"))
  expect_equal(meta$custom$orderly$artefacts,
               data_frame(description = character(),
                          paths = I(list())))
})


test_that("error if declared artefacts are not produced", {
  path <- test_prepare_orderly_example("explicit")
  envir <- new.env()
  path_src <- file.path(path, "src", "explicit", "orderly.R")
  code <- readLines(path_src)
  writeLines(c(
    'orderly2::orderly_artefact("some data", "output.csv")',
    code),
    path_src)
  expect_error(
    orderly_run_quietly("explicit", root = path, envir = envir),
    "Script did not produce expected artefacts: 'output.csv'")
})


test_that("Can run explicit case without orderly", {
  path <- test_prepare_orderly_example("explicit")
  envir <- new.env()
  path_src <- file.path(path, "src", "explicit")
  withr::with_dir(path_src,
                  sys.source("orderly.R", envir))
  expect_setequal(dir(path_src),
                  c("data.csv", "orderly.R", "mygraph.png"))
})


test_that("cope with computed values in static functions", {
  path <- test_prepare_orderly_example("computed-resource")
  envir <- new.env()
  id <- orderly_run_quietly("computed-resource", root = path, envir = envir)
  expect_setequal(
    dir(file.path(path, "archive", "computed-resource", id)),
    c("data.csv", "mygraph.png", "orderly.R"))
})


test_that("run a packet with parameters", {
  path <- test_prepare_orderly_example("parameters")
  envir <- new.env()
  id <- orderly_run_quietly("parameters", parameters = list(a = 10, b = 20, c = 30),
                            envir = envir, root = path)
  path_rds <- file.path(path, "archive", "parameters", id, "data.rds")
  expect_true(file.exists(path_rds))
  expect_equal(readRDS(path_rds), list(a = 10, b = 20, c = 30))
})


test_that("fall back on parameter defaults", {
  path <- test_prepare_orderly_example("parameters")
  envir <- new.env()
  id <- orderly_run_quietly("parameters", parameters = list(a = 10, c = 30),
                            envir = envir, root = path)

  path_rds <- file.path(path, "archive", "parameters", id, "data.rds")
  expect_true(file.exists(path_rds))
  expect_equal(readRDS(path_rds), list(a = 10, b = 2, c = 30))
})


test_that("can run orderly with parameters, without orderly", {
  path <- test_prepare_orderly_example("parameters")
  envir <- list2env(list(a = 10, c = 30), parent = new.env())
  path_src <- file.path(path, "src", "parameters")
  withr::with_dir(path_src,
                  sys.source("orderly.R", envir))

  path_rds <- file.path(path_src, "data.rds")
  expect_true(file.exists(path_rds))
  expect_equal(readRDS(path_rds), list(a = 10, b = 2, c = 30))
})


test_that("can run orderly with parameters, without orderly, globally", {
  path <- test_prepare_orderly_example(c("parameters", "depends-query"))
  id <- orderly_run_quietly("parameters", parameters = list(a = 10, b = 20, c = 30),
                            envir = new.env(), root = path)
  path_src <- file.path(path, "src", "depends-query")
  envir <- list2env(list(a = 10, b = 20, c = 30), parent = globalenv())
  withr::with_dir(path_src,
                  sys.source("orderly.R", envir))
  path_rds <- file.path(path_src, "result.rds")
  expect_true(file.exists(path_rds))
  expect_equal(readRDS(path_rds), list(a = 20, b = 40, c = 60))
})


test_that("Can run simple case with dependency", {
  path <- test_prepare_orderly_example(c("data", "depends"))
  envir1 <- new.env()
  id1 <- orderly_run_quietly("data", root = path, envir = envir1)
  envir2 <- new.env()
  id2 <- orderly_run_quietly("depends", root = path, envir = envir2)

  path1 <- file.path(path, "archive", "data", id1)
  path2 <- file.path(path, "archive", "depends", id2)

  expect_true(file.exists(file.path(path2, "input.rds")))
  expect_equal(
    unname(tools::md5sum(file.path(path2, "input.rds"))),
    unname(tools::md5sum(file.path(path1, "data.rds"))))
})

test_that("Can run dependencies case without orderly", {
  path <- test_prepare_orderly_example(c("data", "depends"))
  envir1 <- new.env()
  id1 <- orderly_run_quietly("data", root = path, envir = envir1)
  envir2 <- new.env()
  path_src <- file.path(path, "src", "depends")
  withr::with_dir(path_src,
                  sys.source("orderly.R", envir2))
  expect_setequal(dir(path_src), c("orderly.R", "input.rds", "graph.png"))
  expect_equal(
    unname(tools::md5sum(file.path(path_src, "input.rds"))),
    unname(tools::md5sum(file.path(path, "archive", "data", id1, "data.rds"))))
})


test_that("Can run case with dependency where both reports are parameterised", {
  path <- test_prepare_orderly_example(c("parameters", "depends-params"))
  envir1 <- new.env()
  id1 <- orderly_run_quietly("parameters", root = path, envir = envir1,
                             parameters = list(a = 10, b = 20, c = 30))
  envir2 <- new.env()
  id2 <- orderly_run_quietly("depends-params", root = path, envir = envir2,
                             parameters = list(a = 1))

  path1 <- file.path(path, "archive", "parameters", id1)
  path2 <- file.path(path, "archive", "depends-params", id2)

  expect_true(file.exists(file.path(path2, "input.rds")))
  expect_equal(
    unname(tools::md5sum(file.path(path2, "input.rds"))),
    unname(tools::md5sum(file.path(path1, "data.rds"))))
})


test_that("can run with shared resources", {
  path <- test_prepare_orderly_example("shared")
  envir <- new.env()
  id <- orderly_run_quietly("shared", root = path, envir = envir)
  expect_setequal(
    dir(file.path(path, "archive", "shared", id)),
    c("shared_data.csv", "mygraph.png", "orderly.R"))
  meta <- orderly_metadata(id, root = path)
  expect_equal(nrow(meta$custom$orderly$shared), 1)
  expect_equal(meta$custom$orderly$shared,
               data_frame(here = "shared_data.csv", there = "data.csv"))
  expect_equal(
    meta$custom$orderly$role,
    data_frame(path = c("orderly.R", "shared_data.csv"),
               role = c("orderly", "shared")))
})


test_that("can run manually with shared resources", {
  path <- test_prepare_orderly_example("shared")
  envir <- new.env()
  path_src <- file.path(path, "src", "shared")
  withr::with_dir(path_src,
                  sys.source("orderly.R", envir))
  expect_setequal(
    dir(path_src),
    c("shared_data.csv", "mygraph.png", "orderly.R"))
})


test_that("can validate shared resource arguments", {
  expect_error(
    validate_shared_resource(list()),
    "orderly_shared_resource requires at least one argument")
  expect_error(
    validate_shared_resource(list(input = c("a", "b"))),
    "Invalid shared resource 'input': entries must be strings")
  expect_error(
    validate_shared_resource(list(a = 1, b = TRUE, c = "str")),
    "Invalid shared resource 'a', 'b': entries must be strings")
  expect_equal(
    validate_shared_resource(list(a = "A", b = "B")),
    c(a = "A", b = "B"))
})


test_that("can't use shared resources if not enabled", {
  path <- test_prepare_orderly_example("shared")
  unlink(file.path(path, "shared"), recursive = TRUE)
  envir <- new.env()
  path_src <- file.path(path, "src", "shared")
  err <- expect_error(
    orderly_run_quietly("shared", root = path, envir = envir),
    "The shared resources directory 'shared' does not exist at orderly's root")
  expect_error(
    withr::with_dir(path_src, sys.source("orderly.R", envir)),
    "The shared resources directory 'shared' does not exist at orderly's root")
})


test_that("shared resources can be directories", {
  path <- test_prepare_orderly_example("shared-dir")
  write.csv(mtcars, file.path(path, "shared/data/mtcars.csv"),
            row.names = FALSE)
  write.csv(iris, file.path(path, "shared/data/iris.csv"),
            row.names = FALSE)

  envir <- new.env()
  id <- orderly_run_quietly("shared-dir", root = path, envir = envir)

  expect_setequal(
    dir(file.path(path, "archive", "shared-dir", id)),
    c("shared_data", "output.rds", "orderly.R"))
  expect_setequal(
    dir(file.path(path, "archive", "shared-dir", id, "shared_data")),
    c("iris.csv", "mtcars.csv"))
  meta <- orderly_metadata(id, root = path)
  expect_equal(nrow(meta$custom$orderly$shared), 2)
  expect_equal(
    meta$custom$orderly$shared,
    data_frame(here = c("shared_data/iris.csv", "shared_data/mtcars.csv"),
               there = c("data/iris.csv", "data/mtcars.csv")))
  expect_equal(
    meta$custom$orderly$role,
    data_frame(
      path = c("orderly.R", "shared_data/iris.csv",
               "shared_data/mtcars.csv"),
      role = c("orderly", "shared", "shared")))
  d <- readRDS(file.path(path, "archive", "shared-dir", id, "output.rds"))
  expect_equal(
    d,
    list(iris = read.csv(file.path(path, "shared/data/iris.csv")),
         mtcars = read.csv(file.path(path, "shared/data/mtcars.csv"))))
})


test_that("can add description metadata", {
  path <- test_prepare_orderly_example("description")
  envir <- new.env()
  id <- orderly_run_quietly("description", root = path, envir = envir)

  meta <- orderly_metadata(id, root = path)
  expect_equal(
    meta$custom$orderly$description,
    list(display = "Packet with description",
         long = "A longer description. Perhaps multiple sentences",
         custom = list(author = "Alice", requester = "Bob")))
})


test_that("can't use description twice in one packet", {
  path <- test_prepare_orderly_example("description")
  envir <- new.env()
  path_orderly <- file.path(path, "src", "description", "orderly.R")
  code <- readLines(path_orderly)
  writeLines(c(code, "orderly2::orderly_description()"), path_orderly)
  expect_error(
    orderly_run_quietly("description", root = path, envir = envir),
    "Only one call to 'orderly2::orderly_description' is allowed",
    fixed = TRUE)
})


test_that("can't use description twice by being sneaky", {
  path <- test_prepare_orderly_example("description")
  envir <- new.env()
  path_orderly <- file.path(path, "src", "description", "orderly.R")
  code <- readLines(path_orderly)
  writeLines(c(code, "for (i in 1:2) orderly2::orderly_description()"),
             path_orderly)
  expect_error(
    orderly_run_quietly("description", root = path, envir = envir),
    "Only one call to 'orderly2::orderly_description' is allowed",
    fixed = TRUE)
})


test_that("with strict mode, only declared files are copied, running fails", {
  path <- test_prepare_orderly_example("implicit")
  path_src <- file.path(path, "src", "implicit", "orderly.R")
  code <- readLines(path_src)
  writeLines(c("orderly2::orderly_strict_mode()", code), path_src)
  err <- suppressWarnings(tryCatch(read.csv("data.csv"), error = identity))
  expect_error(
    suppressWarnings(orderly_run_quietly("implicit", root = path)),
    err$message,
    fixed = TRUE)
})


test_that("with strict mode, indicate unknown files as potential artefacts", {
  path <- test_prepare_orderly_example("implicit")
  path_src <- file.path(path, "src", "implicit", "orderly.R")
  code <- readLines(path_src)
  writeLines(c("orderly2::orderly_strict_mode()",
               'orderly2::orderly_resource("data.csv")',
               code),
             path_src)
  res <- testthat::evaluate_promise(
    id <- orderly_run("implicit", root = path))
  expect_match(res$messages,
               "orderly produced unexpected files:\n  - mygraph.png",
               all = FALSE)
  expect_setequal(
    dir(file.path(path, "archive", "implicit", id)),
    c("orderly.R", "mygraph.png", "data.csv"))
})


test_that("without strict mode, detect modified files", {
  path <- test_prepare_orderly_example("implicit")
  file.create(file.path(path, "src", "implicit", "mygraph.png"))
  res <- testthat::evaluate_promise(
    id <- orderly_run("implicit", root = path))
  expect_match(
    res$messages,
    "inputs modified; these are probably artefacts:\n  - mygraph.png",
    fixed = TRUE, all = FALSE)
  expect_setequal(
    dir(file.path(path, "archive", "implicit", id)),
    c("orderly.R", "mygraph.png", "data.csv"))
})


test_that("disallow multiple calls to strict mode", {
  path <- test_prepare_orderly_example("implicit")
  path_src <- file.path(path, "src", "implicit", "orderly.R")
  code <- readLines(path_src)
  writeLines(c("if (TRUE) {",
               "  orderly2::orderly_strict_mode()",
               "}",
               code),
             path_src)
  expect_error(
    orderly_run_quietly("implicit", root = path),
    "orderly function 'orderly_strict_mode' can only be used at the top level")

  writeLines(c("orderly2::orderly_strict_mode()",
               "orderly2::orderly_strict_mode()",
               code),
             path_src)
  expect_error(
    orderly_run_quietly("implicit", root = path),
    "Only one call to 'orderly2::orderly_strict_mode' is allowed")
})


test_that("can copy resource from directory, implicitly", {
  path <- test_prepare_orderly_example("resource-in-directory")
  envir <- new.env()
  id <- orderly_run_quietly("resource-in-directory", root = path, envir = envir)

  meta <- orderly_metadata(id, root = path)
  expect_equal(meta$custom$orderly$role,
               data_frame(path = "orderly.R",
                          role = "orderly"))
  expect_setequal(
    meta$files$path,
    c("data.rds", "data/a.csv", "data/b.csv", "orderly.R"))
  expect_true(file.exists(
    file.path(path, "archive", "resource-in-directory", id, "data.rds")))
})


test_that("fail to copy resource from directory, implicitly, strictly", {
  path <- test_prepare_orderly_example("resource-in-directory")
  envir <- new.env()
  path_src <- file.path(path, "src", "resource-in-directory", "orderly.R")
  prepend_lines(path_src, "orderly2::orderly_strict_mode()")
  err <- suppressWarnings(tryCatch(read.csv("data/a.csv"), error = identity))
  expect_error(suppressWarnings(
    orderly_run_quietly("resource-in-directory", root = path, envir = envir)),
    err$message,
    fixed = TRUE)
})


test_that("can copy resource from directory, included by file", {
  path <- test_prepare_orderly_example("resource-in-directory")
  envir <- new.env()
  path_src <- file.path(path, "src", "resource-in-directory", "orderly.R")
  prepend_lines(path_src,
                c('orderly2::orderly_resource("data/a.csv")',
                  'orderly2::orderly_resource("data/b.csv")'))
  id <- orderly_run_quietly("resource-in-directory", root = path, envir = envir)
  meta <- orderly_metadata(id, root = path)
  expect_equal(
    meta$custom$orderly$role,
    data_frame(path = c("orderly.R", "data/a.csv", "data/b.csv"),
               role = c("orderly", "resource", "resource")))
  expect_true(file.exists(
    file.path(path, "archive", "resource-in-directory", id, "data.rds")))
})


test_that("can copy resource from directory, included by file, strict mode", {
  path <- test_prepare_orderly_example("resource-in-directory")
  envir <- new.env()
  path_src <- file.path(path, "src", "resource-in-directory", "orderly.R")
  prepend_lines(path_src,
                c("orderly2::orderly_strict_mode()",
                  'orderly2::orderly_resource("data/a.csv")',
                  'orderly2::orderly_resource("data/b.csv")'))
  id <- orderly_run_quietly("resource-in-directory", root = path, envir = envir)
  meta <- orderly_metadata(id, root = path)
  expect_equal(
    meta$custom$orderly$role,
    data_frame(path = c("orderly.R", "data/a.csv", "data/b.csv"),
               role = c("orderly", "resource", "resource")))
  expect_true(file.exists(
    file.path(path, "archive", "resource-in-directory", id, "data.rds")))
})


test_that("can copy resource from directory, included by directory", {
  path <- test_prepare_orderly_example("resource-in-directory")
  envir <- new.env()
  path_src <- file.path(path, "src", "resource-in-directory", "orderly.R")
  prepend_lines(path_src, 'orderly2::orderly_resource("data")')
  id <- orderly_run_quietly("resource-in-directory", root = path, envir = envir)

  meta <- orderly_metadata(id, root = path)
  expect_equal(
    meta$custom$orderly$role,
    data_frame(path = c("orderly.R", "data/a.csv", "data/b.csv"),
               role = c("orderly", "resource", "resource")))
  expect_true(file.exists(
    file.path(path, "archive", "resource-in-directory", id, "data.rds")))
})


test_that("can copy resource from directory, included by directory, strictly", {
  path <- test_prepare_orderly_example("resource-in-directory")
  envir <- new.env()
  path_src <- file.path(path, "src", "resource-in-directory", "orderly.R")
  prepend_lines(path_src,
                c("orderly2::orderly_strict_mode()",
                  'orderly2::orderly_resource("data")'))
  id <- orderly_run_quietly("resource-in-directory", root = path, envir = envir)

  meta <- orderly_metadata(id, root = path)
  expect_equal(
    meta$custom$orderly$role,
    data_frame(path = c("orderly.R", "data/a.csv", "data/b.csv"),
               role = c("orderly", "resource", "resource")))
  expect_true(file.exists(
    file.path(path, "archive", "resource-in-directory", id, "data.rds")))
})


test_that("don't copy artefacts over when not needed", {
  artefacts <- list(list(files = "a.rds"), list(files = c("b.rds", "c.rds")))
  resources <- c("a.csv", "b.csv", "c.rds")
  src <- withr::local_tempdir()
  file.create(file.path(src, resources))
  file.create(file.path(src, "d.csv"))

  dst1 <- withr::local_tempdir()
  info <- copy_resources_implicit(src, dst1, resources, artefacts)
  expect_setequal(info$path, c(resources, "d.csv"))
  expect_setequal(dir(dst1), c(resources, "d.csv"))

  dst2 <- withr::local_tempdir()
  info <- copy_resources_implicit(src, dst2, resources[-3], artefacts)
  expect_setequal(info$path, c(resources[-3], "d.csv"))
  expect_setequal(dir(dst2), c(resources[-3], "d.csv"))

  dst3 <- withr::local_tempdir()
  info <- copy_resources_implicit(src, dst3, character(), artefacts)
  expect_setequal(info$path, c(resources[-3], "d.csv"))
  expect_setequal(dir(dst3), c(resources[-3], "d.csv"))
})


test_that("can pull resources programmatically", {
  path <- test_prepare_orderly_example("programmatic-resource")
  id1 <- orderly_run_quietly("programmatic-resource", list(use = "a"),
                             root = path)
  id2 <- orderly_run_quietly("programmatic-resource", list(use = "b"),
                             root = path)
  meta1 <- orderly_metadata(id1, root = path)
  meta2 <- orderly_metadata(id2, root = path)

  expect_equal(meta1$custom$orderly$role,
               data_frame(path = c("orderly.R", "a.csv"),
                          role = c("orderly", "resource")))
  expect_equal(meta2$custom$orderly$role,
               data_frame(path = c("orderly.R", "b.csv"),
                          role = c("orderly", "resource")))
  expect_setequal(meta1$files$path,
                  c("a.csv", "b.csv", "data.rds", "orderly.R"))
  expect_setequal(meta2$files$path,
                  c("a.csv", "b.csv", "data.rds", "orderly.R"))
})


test_that("can pull resources programmatically, strictly", {
  path <- test_prepare_orderly_example("programmatic-resource")
  path_src <- file.path(path, "src", "programmatic-resource", "orderly.R")
  prepend_lines(path_src, "orderly2::orderly_strict_mode()")
  id1 <- orderly_run_quietly("programmatic-resource", list(use = "a"),
                             root = path)
  id2 <- orderly_run_quietly("programmatic-resource", list(use = "b"),
                             root = path)
  meta1 <- meta <- orderly_metadata(id1, root = path)
  meta2 <- meta <- orderly_metadata(id2, root = path)

  expect_equal(meta1$custom$orderly$role,
               data_frame(path = c("orderly.R", "a.csv"),
                          role = c("orderly",  "resource")))
  expect_equal(meta2$custom$orderly$role,
               data_frame(path = c("orderly.R", "b.csv"),
                          role = c("orderly",  "resource")))
  expect_setequal(meta1$files$path,
                  c("a.csv", "data.rds", "orderly.R"))
  expect_setequal(meta2$files$path,
                  c("b.csv", "data.rds", "orderly.R"))
})


test_that("can fetch information about the context", {
  path <- test_prepare_orderly_example(c("data", "depends"))

  envir1 <- new.env()
  id1 <- orderly_run_quietly("data", root = path, envir = envir1)
  path_src <- file.path(path, "src", "depends", "orderly.R")
  code <- readLines(path_src)
  writeLines(c(code, 'saveRDS(orderly2::orderly_run_info(), "info.rds")'),
             path_src)

  envir2 <- new.env()
  id2 <- orderly_run_quietly("depends", root = path, envir = envir2)

  path2 <- file.path(path, "archive", "depends", id2)
  d <- readRDS(file.path(path2, "info.rds"))

  root_real <- as.character(fs::path_real(path))
  depends <- data_frame(index = 1, name = "data",
                        query = 'latest(name == "data")',
                        id = id1, there = "data.rds", here = "input.rds")
  expect_equal(d, list(name = "depends", id = id2, root = root_real,
                       depends = depends))
})


test_that("can fetch information interactively", {
  path <- test_prepare_orderly_example(c("data", "depends"))
  envir1 <- new.env()
  id1 <- orderly_run_quietly("data", root = path, envir = envir1)
  path_src <- file.path(path, "src", "depends", "orderly.R")
  code <- readLines(path_src)
  writeLines(c(code, 'saveRDS(orderly2::orderly_run_info(), "info.rds")'),
             path_src)

  envir2 <- new.env()
  path_src <- file.path(path, "src", "depends")
  withr::with_dir(path_src,
                  sys.source("orderly.R", envir2))

  path2 <- file.path(path, "src", "depends")
  d <- readRDS(file.path(path2, "info.rds"))

  root_real <- as.character(fs::path_real(path))
  depends <- data_frame(index = integer(), name = character(),
                        query = character(), id = character(),
                        there = character(), here = character())
  expect_equal(d, list(name = "depends", id = NA_character_, root = root_real,
                       depends = depends))
})


test_that("cope with failed run", {
  path <- test_prepare_orderly_example("explicit")
  envir <- new.env()

  append_lines(file.path(path, "src", "explicit", "orderly.R"),
               'readRDS("somepath.rds")')
  err <- expect_error(suppressWarnings(
    orderly_run_quietly("explicit", root = path, envir = envir, echo = FALSE)))

  p <- file.path(path, "draft", "explicit")
  id <- tail(dir(p), 1)

  ## Currently, nothing here is written to disk:
  ## > cmp <- evaluate_promise(
  ## >   tryCatch(readRDS("somepath.rds"), error = identity))
  ## > expect_equal(detail$warning, cmp$warnings)
  ## > expect_equal(detail$error, cmp$result$message)
  ## > expect_gt(length(detail$trace), 5) # nontrivial trace
  ## > expect_match(detail$trace,
  ## >              'readRDS\\("somepath.rds"\\).*orderly\\.R', all = FALSE)
})


test_that("Can select location when querying dependencies for a report", {
  path <- character()
  ids <- character()
  for (nm in c("us", "prod", "dev")) {
    path[[nm]] <- test_prepare_orderly_example(c("data", "depends"))
    if (nm != "us") {
      ids[[nm]] <- orderly_run_quietly("data", root = path[[nm]])
      orderly_location_add(nm, "path", list(path = path[[nm]]),
                           root = path[["us"]])
      orderly_location_pull_metadata(nm, root = path[["us"]])
      for (i in ids[[nm]]) {
        suppressMessages(orderly_location_pull_packet(i, root = path[["us"]]))
      }
    }
  }
  ## Run extra local copy - this is the most recent.
  ids[["us"]] <- orderly_run_quietly("data", root = path[["us"]])

  ## Without locations we prefer the local one:
  id1 <- orderly_run_quietly("depends", root = path[["us"]])
  expect_equal(orderly_metadata(id1, path[["us"]])$depends$packet,
               ids[["us"]])

  ## Filter to only allow prod:
  id2 <- orderly_run_quietly("depends",
                             search_options = list(location = "prod"),
                             root = path[["us"]])
  expect_equal(orderly_metadata(id2, path[["us"]])$depends$packet,
               ids[["prod"]])

  ## Allow any location:
  id3 <- orderly_run_quietly("depends",
                             search_options = list(location = c("prod", "dev")),
                             root = path[["us"]])
  expect_equal(orderly_metadata(id3, path[["us"]])$depends$packet,
               ids[["dev"]])
})


test_that("can select location when querying dependencies interactively", {
  withr::defer(reset_interactive())

  envir1 <- new.env()

  path <- character()
  ids <- character()
  for (nm in c("us", "prod", "dev")) {
    path[[nm]] <- test_prepare_orderly_example(c("data", "depends"))
    if (nm != "us") {
      ids[[nm]] <- orderly_run_quietly("data", envir = envir1, root = path[[nm]])
      orderly_location_add(nm, "path", list(path = path[[nm]]),
                           root = path[["us"]])
      orderly_location_pull_metadata(nm, root = path[["us"]])
      for (i in ids[[nm]]) {
        suppressMessages(orderly_location_pull_packet(i, root = path[["us"]]))
      }
    }
  }
  ## Run extra local copy - this is the most recent.
  ids[["us"]] <- orderly_run_quietly("data", envir = envir1, root = path[["us"]])

  orderly_interactive_set_search_options(list(location = "prod"))
  expect_equal(.interactive$search_options, list(location = "prod"))

  envir2 <- new.env()
  path_src <- file.path(path[["us"]], "src", "depends")
  withr::with_dir(path_src,
                  sys.source("orderly.R", envir2))

  ## Correct file was pulled in:
  expect_equal(
    readRDS(file.path(path_src, "input.rds")),
    readRDS(file.path(path[["prod"]], "archive", "data", ids[["prod"]],
                      "data.rds")))
})


test_that("can use a resource from a directory", {
  path <- test_prepare_orderly_example("directories")
  envir <- new.env()
  id <- orderly_run_quietly("directories", root = path, envir = envir)
  meta <- orderly_metadata(id, root = path)
  expect_equal(
    meta$custom$orderly$role,
    data_frame(path = c("orderly.R", "data/a.csv", "data/b.csv"),
               role = c("orderly",  "resource", "resource")))
  expect_equal(
    meta$custom$orderly$artefacts,
    data_frame(description = "output files",
               paths = I(list(list("output/a.rds", "output/b.rds")))))
})


test_that("can use a resource from a directory", {
  path <- test_prepare_orderly_example("directories")
  envir <- new.env()
  id <- orderly_run_quietly("directories", root = path, envir = envir)
  meta <- orderly_metadata(id, root = path)
  expect_equal(
    meta$custom$orderly$role,
    data_frame(path = c("orderly.R", "data/a.csv", "data/b.csv"),
               role = c("orderly", "resource", "resource")))
  expect_equal(
    meta$custom$orderly$artefacts,
    data_frame(description = "output files",
               paths = I(list(list("output/a.rds", "output/b.rds")))))
  expect_setequal(meta$files$path,
                  c("data/a.csv", "data/b.csv", "orderly.R",
                    "output/a.rds", "output/b.rds"))
})


test_that("can depend on a directory artefact", {
  path <- test_prepare_orderly_example("directories")
  envir1 <- new.env()
  id1 <- orderly_run_quietly("directories", root = path, envir = envir1)

  path_src <- file.path(path, "src", "use")
  fs::dir_create(path_src)
  writeLines(c(
    'orderly2::orderly_dependency("directories", "latest()", c(d = "output/"))',
    'orderly2::orderly_artefact("data", "d.rds")',
    'd <- c(readRDS("d/a.rds", "d/b.rds"))',
    'saveRDS(d, "d.rds")'),
    file.path(path_src, "orderly.R"))
  envir2 <- new.env()
  id2 <- orderly_run_quietly("use", root = path, envir = envir2)
  meta <- orderly_metadata(id2, root = path)
  expect_equal(meta$depends$packet, id1)
  expect_equal(meta$depends$files[[1]],
               data_frame(here = c("d/a.rds", "d/b.rds"),
                          there = c("output/a.rds", "output/b.rds")))
})


test_that("can compute dependencies", {
  path <- test_prepare_orderly_example("parameters")
  envir1 <- new.env()
  id1 <- orderly_run_quietly("parameters", list(a = 1, b = 2, c = 3),
                             root = path, envir = envir1)
  id2 <- orderly_run_quietly("parameters", list(a = 3, b = 2, c = 1),
                             root = path, envir = envir1)

  path_src <- file.path(path, "src", "use")
  fs::dir_create(path_src)
  code <- c(
    "orderly2::orderly_dependency(",
    '  "parameters",',
    '  "latest(parameter:a == environment:x)",',
    '  c(d.rds = "data.rds"))',
    'orderly2::orderly_artefact("data", "d.rds")')

  writeLines(code, file.path(path_src, "orderly.R"))
  envir2 <- new.env()
  expect_error(
    orderly_run_quietly("use", root = path, envir = envir2),
    "Did not find 'x' within given environment")

  envir2$x <- 1
  id <- orderly_run_quietly("use", root = path, envir = envir2)
  expect_equal(orderly_metadata(id, root = path)$depends[[1]], id1)

  envir2$x <- 2
  expect_error(
    orderly_run_quietly("use", root = path, envir = envir2),
    "Failed to find packet for query")

  envir2$x <- 3
  id <- orderly_run_quietly("use", root = path, envir = envir2)
  meta <- orderly_metadata(id, root = path)
  expect_equal(meta$depends$packet, id2)
  expect_equal(meta$depends$query,
               'latest(parameter:a == 3 && name == "parameters")')

  writeLines(c("x <- 1", code), file.path(path_src, "orderly.R"))
  id <- orderly_run_quietly("use", root = path, envir = envir2)
  expect_equal(orderly_metadata(id, root = path)$depends$packet, id1)

  rm(list = "x", envir = envir2)
  id <- orderly_run_quietly("use", root = path, envir = envir2)
  expect_equal(orderly_metadata(id, root = path)$depends$packet, id1)
})


test_that("nice error if running nonexistant report", {
  path <- test_prepare_orderly_example(c("implicit", "explicit"))
  envir <- new.env()
  err <- expect_error(
    orderly_run_quietly("xplicit", root = path, envir = envir),
    "Did not find orderly report 'xplicit'")
  expect_equal(err$body[1:2],
               c(x = "The path 'src/xplicit' does not exist",
                 i = "Did you mean 'explicit', 'implicit'"))
  ## Sloppy check here to avoid tedious macOS temporary path location
  ## being in two places at once.
  expect_match(err$body[[3]], "Looked relative to orderly root at")
})


test_that("validation of orderly directories", {
  path <- test_prepare_orderly_example(character())
  root <- root_open(path, FALSE, TRUE)
  nms <- sprintf("example_%s", letters[1:8])
  fs::dir_create(file.path(path, "src", nms))
  file.create(file.path(path, "src", nms, "orderly.R"))
  hint_root <- sprintf("Looked relative to orderly root at '%s'", root$path)

  err <- expect_error(
    validate_orderly_directory("foo", root),
    "Did not find orderly report 'foo'")
  expect_equal(err$body,
               c(x = "The path 'src/foo' does not exist",
                 i = hint_root))

  file.create(file.path(path, "src", "foo"))
  err <- expect_error(
    validate_orderly_directory("foo", root),
    "Did not find orderly report 'foo'")
  expect_equal(err$body,
               c(x = "The path 'src/foo' exists but is not a directory",
                 i = hint_root))

  fs::dir_create(file.path(path, "src", "bar"))
  err <- expect_error(
    validate_orderly_directory("bar", root),
    "Did not find orderly report 'bar'")
  expect_equal(
    err$body,
    c(x = "The path 'src/bar' exists but does not contain 'orderly.R'",
      i = hint_root))

  hint_close <- sprintf("Did you mean %s",
                        paste(squote(nms[1:5]), collapse = ", "))
  err <- expect_error(
    validate_orderly_directory("example_z", root),
    "Did not find orderly report 'example_z'")
  expect_equal(err$body,
               c(x = "The path 'src/example_z' does not exist",
                 i = hint_close,
                 i = hint_root))

  file.create(file.path(path, "src", "example_z"))
  err <- expect_error(
    validate_orderly_directory("example_z", root),
    "Did not find orderly report 'example_z'")
  expect_equal(err$body,
               c(x = "The path 'src/example_z' exists but is not a directory",
                 i = hint_close,
                 i = hint_root))

  fs::dir_create(file.path(path, "src", "example_x"))
  err <- expect_error(
    validate_orderly_directory("example_x", root),
    "Did not find orderly report 'example_x'")
  expect_equal(
    err$body,
    c(x = "The path 'src/example_x' exists but does not contain 'orderly.R'",
      i = hint_close,
      i = hint_root))
})


test_that("can rename dependencies programmatically", {
  path <- test_prepare_orderly_example("data")
  envir1 <- new.env()
  id1 <- orderly_run_quietly("data", root = path, envir = envir1)

  path_src <- file.path(path, "src", "use")
  fs::dir_create(path_src)
  writeLines(c(
    'orderly2::orderly_artefact("data", "d.rds")',
    'p <- "x"',
    "orderly2::orderly_dependency(",
    '  "data", "latest()",',
    '  c("${p}/data.rds" = "data.rds"))',
    'd <- readRDS(file.path(p, "data.rds"))',
    'saveRDS(d, "d.rds")'),
    file.path(path_src, "orderly.R"))
  envir2 <- new.env()
  id2 <- orderly_run_quietly("use", root = path, envir = envir2)
  meta <- orderly_metadata(id2, root = path)
  expect_equal(meta$depends$packet, id1)
  expect_equal(meta$depends$files[[1]],
               data_frame(here = "x/data.rds",
                          there = "data.rds"))
})


test_that("can detect device imbalance", {
  path <- test_prepare_orderly_example("explicit")

  path_src <- file.path(path, "src", "explicit", "orderly.R")
  code <- readLines(path_src)
  writeLines(code[!grepl("^dev.off", code)], path_src)

  stack <- dev.list()

  envir <- new.env()
  err <- expect_error(
    orderly_run_quietly("explicit", root = path, envir = envir),
    "Script failed to balance a global resource stack")
  expect_equal(err$body,
               c(x = "Script left 1 device open"))
  expect_equal(stack, dev.list())
})


test_that("can use quote for queries queries", {
  path <- test_prepare_orderly_example(c("data", "depends"))
  id1 <- c(orderly_run_quietly("data", envir = new.env(), root = path),
           orderly_run_quietly("data", envir = new.env(), root = path))
  path_src <- file.path(path, "src", "depends", "orderly.R")
  src <- readLines(path_src)
  writeLines(sub('"latest"', "quote(latest())", src, fixed = TRUE), path_src)
  id2 <- orderly_run_quietly("depends", root = path)
  expect_equal(orderly_metadata(id2, root = path)$depends$packet, id1[[2]])
})


test_that("can describe misses for dependencies", {
  path <- test_prepare_orderly_example(c("data", "depends"))
  envir <- new.env()
  err <- expect_error(
    orderly_run_quietly("depends", root = path, envir = envir),
    "Failed to run report")
  expect_equal(
    unname(err$parent$message),
    "Failed to find packet for query 'latest(name == \"data\")'")
  expect_equal(
    err$parent$body,
    c(i = "See 'rlang::last_error()$explanation' for details"))
  expect_s3_class(err$parent$explanation, "orderly_query_explain")
  expect_identical(err$explanation, err$parent$explanation)
})
