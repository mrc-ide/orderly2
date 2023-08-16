test_that("can cleanup explicit things quite well", {
  path <- test_prepare_orderly_example("explicit")
  envir <- new.env()
  path_src <- file.path(path, "src", "explicit")
  withr::with_dir(path_src,
                  sys.source("orderly.R", envir))
  status <- withr::with_dir(path_src, orderly_cleanup_status())

  expect_s3_class(status, "orderly_cleanup_status")
  expect_setequal(names(status),
                  c("name", "root", "path", "role", "status", "delete"))
  expect_equal(status$name, "explicit")
  expect_equal(status$root, root_open(path, FALSE, FALSE)$path)
  expect_equal(normalise_path(status$path),
               normalise_path(file.path(status$root, "src", status$name)))
  paths <- c("data.csv", "mygraph.png", "orderly.R")
  expect_equal(status$role,
               cbind(orderly = set_names(c(FALSE, FALSE, TRUE), paths),
                     resource = c(TRUE, FALSE, FALSE),
                     shared_resource = FALSE,
                     dependency = FALSE,
                     artefact = c(FALSE, TRUE, FALSE)))
  expect_equal(status$status,
               cbind(source = set_names(c(TRUE, FALSE, TRUE), paths),
                     derived = c(FALSE, TRUE, FALSE),
                     ignored = NA))
  expect_equal(status$delete, "mygraph.png")

  res <- testthat::evaluate_promise(
    withr::with_dir(path_src, orderly_cleanup(dry_run = TRUE)))
  expect_match(res$messages, "I would delete 1 file from 'explicit':",
               all = FALSE)
  expect_setequal(dir(path_src), c("data.csv", "orderly.R", "mygraph.png"))
  expect_equal(res$result, status)

  res <- testthat::evaluate_promise(
    withr::with_dir(path_src, orderly_cleanup()))
  expect_match(res$messages, "Deleting 1 file from 'explicit':", all = FALSE)
  expect_setequal(dir(path_src), c("data.csv", "orderly.R"))
  expect_equal(res$result, status)
})


test_that("can clean up unknown files if gitignored", {
  path <- test_prepare_orderly_example("explicit")
  envir <- new.env()
  path_src <- file.path(path, "src", "explicit")
  helper_add_git(path)

  fs::dir_create(file.path(path_src, c("a", "b/c", "b/d")))
  files <- c("a/x", "b/c/x", "b/c/y")
  file.create(file.path(path_src, files))

  status <- orderly_cleanup_status("explicit", root = path)
  expect_equal(
    status$status[c("a/x", "b/c/x", "b/c/y"), ],
    cbind(source = set_names(rep(FALSE, 3), files),
          derived = FALSE,
          ignored = FALSE))
  expect_equal(status$delete, character())

  writeLines(c("b/"), file.path(path_src, ".gitignore"))
  status <- orderly_cleanup_status("explicit", root = path)
  expect_equal(
    status$status[c("a/x", "b/c/x", "b/c/y"), ],
    cbind(source = set_names(rep(FALSE, 3), files),
          derived = FALSE,
          ignored = c(FALSE, TRUE, TRUE)))
  expect_equal(status$delete, c("b/c/x", "b/c/y"))

  res <- testthat::evaluate_promise(
    orderly_cleanup("explicit", root = path))
  expect_match(res$messages,
               "Deleting 2 files from 'explicit'", all = FALSE)
  expect_match(res$messages,
               "Also deleted 1 empty directory", all = FALSE)
  expect_equal(res$result, status)
  expect_setequal(
    dir(path_src, recursive = TRUE, include.dirs = TRUE),
    c("a", "a/x", "data.csv", "orderly.R"))
})


test_that("can clean up shared resources", {
  path <- test_prepare_orderly_example("shared")
  path_src <- file.path(path, "src", "shared")
  file.create(file.path(path_src, "shared_data.csv"))
  status <- orderly_cleanup_status("shared", root = path)

  files <- c("orderly.R", "shared_data.csv")
  expect_setequal(rownames(status$role), files)
  expect_equal(
    status$role,
    cbind(orderly = set_names(c(TRUE, FALSE), files),
          resource = FALSE,
          shared_resource = c(FALSE, TRUE),
          dependency = FALSE,
          artefact = FALSE))
  expect_equal(
    status$status,
    cbind(source = set_names(c(TRUE, FALSE), files),
          derived = c(FALSE, TRUE),
          ignored = NA))
  expect_equal(status$delete, "shared_data.csv")
})


test_that("can clean up dependencies", {
  path <- test_prepare_orderly_example(c("data", "depends"))
  path_src <- file.path(path, "src", "depends")
  file.create(file.path(path_src, c("input.rds", "other.rds")))
  status <- orderly_cleanup_status("depends", root = path)

  files <- c("input.rds", "orderly.R", "other.rds")
  expect_setequal(rownames(status$role), files)
  expect_equal(
    status$role,
    cbind(orderly = set_names(c(FALSE, TRUE, FALSE), files),
          resource = FALSE,
          shared_resource = FALSE,
          dependency = c(TRUE, FALSE, FALSE),
          artefact = FALSE))
  expect_equal(
    status$status,
    cbind(source = set_names(c(FALSE, TRUE, FALSE), files),
          derived = c(TRUE, FALSE, FALSE),
          ignored = NA))
  expect_equal(status$delete, "input.rds")
})


test_that("can clean up directories", {
  path <- test_prepare_orderly_example("directories")
  envir <- new.env()
  path_src <- file.path(path, "src", "directories")
  withr::with_dir(path_src,
                  sys.source("orderly.R", envir))
  status <- orderly_cleanup_status("directories", root = path)

  files <- c("data/a.csv", "data/b.csv", "orderly.R",
             "output/a.rds", "output/b.rds")
  expect_equal(rownames(status$role), files)
  expect_equal(
    status$role,
    cbind(orderly = set_names(files == "orderly.R", files),
          resource = c(TRUE, TRUE, FALSE, FALSE, FALSE),
          shared_resource = FALSE,
          dependency = FALSE,
          artefact = c(FALSE, FALSE, FALSE, TRUE, TRUE)))
  expect_equal(
    status$status,
    cbind(source = set_names(c(TRUE, TRUE, TRUE, FALSE, FALSE), files),
          derived = c(FALSE, FALSE, FALSE, TRUE, TRUE),
          ignored = NA))
  expect_equal(status$delete, c("output/a.rds", "output/b.rds"))

  res <- testthat::evaluate_promise(orderly_cleanup("directories", root = path))
  expect_equal(res$result, status)
  expect_setequal(
    dir(path_src, recursive = TRUE, include.dirs = TRUE),
    c("data", "data/a.csv", "data/b.csv", "orderly.R"))
})


test_that("Don't call cleanup on an active packet", {
  path <- test_prepare_orderly_example("data")
  path_src <- file.path(path, "src", "data")
  append_lines(file.path(path_src, "orderly.R"),
               "orderly2::orderly_cleanup_status()")
  expect_error(
    orderly_run_quietly("data", root = path),
    "Don't call 'orderly2::orderly_cleanup_status()' from a running packet",
    fixed = TRUE)
})


test_that("Don't be weird about not passing name", {
  path <- test_prepare_orderly_example("data")
  path_src <- file.path(path, "src", "data")
  expect_error(
    withr::with_dir(path_src, orderly_cleanup_status(root = path)),
    "If 'root' is given explicitly, 'name' is required")
})


test_that("Cope with cleaning up when there's nothing to do", {
  path <- test_prepare_orderly_example("data")
  path_src <- file.path(path, "src", "data")
  status <- withr::with_dir(path_src, orderly_cleanup_status())
  expect_equal(status$delete, character())
  res <- testthat::evaluate_promise(
    withr::with_dir(path_src, orderly_cleanup()))
  expect_match(res$messages, "Nothing to clean")
  expect_equal(res$result, status)
})
