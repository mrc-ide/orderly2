test_that("can run simple case in separate directory", {
  info <- test_prepare_orderly_example_separate("explicit")
  id <- withr::with_envvar(
    c(ORDERLY_REPORT_SRC = info$src),
    orderly_run_quietly("explicit", envir = new.env(), root = info$outpack)
  )
  expect_type(id, "character")
  expect_true(file.exists(file.path(info$src, "draft")))
  expect_false(file.exists(file.path(info$src, "archive")))
  expect_false(file.exists(file.path(info$outpack, "draft")))
  expect_true(file.exists(file.path(info$outpack, "archive")))
  expect_true(file.exists(file.path(info$outpack, "archive", "explicit", id)))
})


test_that("can run shared resources case in separate directory", {
  ## This is worth a separate check as it's important that the shared
  ## resources are relative to the *source* tree and not the outpack
  ## root.
  info <- test_prepare_orderly_example_separate("shared")
  id <- withr::with_envvar(
    c(ORDERLY_REPORT_SRC = info$src),
    orderly_run_quietly("shared", envir = new.env(), root = info$outpack)
  )
  expect_setequal(
    dir(file.path(info$outpack, "archive", "shared", id)),
    c("shared_data.csv", "mygraph.png", "shared.R"))
})


test_that("can use dependencies in separate directory", {
  ## Ensures that we hit the outpack root for pulling deps in
  info <- test_prepare_orderly_example_separate(c("data", "depends"))
  id1 <- withr::with_envvar(
    c(ORDERLY_REPORT_SRC = info$src),
    orderly_run_quietly("data", envir = new.env(), root = info$outpack)
  )
  id2 <- withr::with_envvar(
    c(ORDERLY_REPORT_SRC = info$src),
    orderly_run_quietly("depends", envir = new.env(), root = info$outpack)
  )
  path1 <- file.path(info$outpack, "archive", "data", id1)
  path2 <- file.path(info$outpack, "archive", "depends", id2)

  expect_true(file.exists(file.path(path2, "input.rds")))
  expect_equal(
    unname(tools::md5sum(file.path(path2, "input.rds"))),
    unname(tools::md5sum(file.path(path1, "data.rds"))))
})


test_that("can get git information in separate directory", {
  info <- test_prepare_orderly_example_separate("explicit")
  info$git <- helper_add_git(info$src)
  id <- withr::with_envvar(
    c(ORDERLY_REPORT_SRC = info$src),
    orderly_run_quietly("explicit", envir = new.env(), root = info$outpack)
  )
  meta <- orderly_metadata(id, root = info$outpack)
  expect_mapequal(meta$git, info$git[c("sha", "branch", "url")])
})


test_that("can't run interactively in separate directory", {
  ## Picking on depends here because it really requires the outpack
  ## root
  info <- test_prepare_orderly_example_separate(c("data", "depends"))
  id1 <- withr::with_envvar(
    c(ORDERLY_REPORT_SRC = info$src),
    orderly_run_quietly("data", envir = new.env(), root = info$outpack)
  )
  path_src <- file.path(info$src, "src", "depends")
  expect_error(
    withr::with_dir(path_src,
                    sys.source("depends.R", new.env())),
    "orderly directory '.+' not initialised")
})
