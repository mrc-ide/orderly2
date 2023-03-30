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
