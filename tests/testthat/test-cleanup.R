test_that("can cleanup explicit things quite well", {
  path <- test_prepare_orderly_example("explicit")
  envir <- new.env()
  path_src <- file.path(path, "src", "explicit")
  withr::with_dir(path_src,
                  sys.source("explicit.R", envir))
  status <- withr::with_dir(path_src, orderly_cleanup_status())

  expect_s3_class(status, "orderly_cleanup_status")
  expect_setequal(
    names(status),
    c("name", "root", "path", "role", "status", "delete", "unknown"))
  expect_equal(status$name, "explicit")
  expect_equal(normalise_path(status$root),
               normalise_path(root_open(path, require_orderly = FALSE)$path))
  expect_equal(normalise_path(status$path),
               normalise_path(file.path(status$root, "src", status$name)))
  paths <- c("data.csv", "explicit.R", "mygraph.png")
  expect_equal(status$role,
               cbind(orderly = set_names(c(FALSE, TRUE, FALSE), paths),
                     resource = c(TRUE, FALSE, FALSE),
                     shared_resource = FALSE,
                     dependency = FALSE,
                     artefact = c(FALSE, FALSE, TRUE)))
  expect_equal(status$status,
               cbind(source = set_names(c(TRUE, TRUE, FALSE), paths),
                     derived = c(FALSE, FALSE, TRUE),
                     ignored = NA))
  expect_equal(status$delete, "mygraph.png")
  expect_equal(status$unknown, character())

  res <- testthat::evaluate_promise(print(status))
  expect_equal(res$result, status)
  expect_length(res$messages, 3)
  expect_match(res$messages[[1]], "explicit is not clean")
  expect_match(res$messages[[2]], "1 file can be deleted by running")
  expect_match(res$messages[[3]], "mygraph.png")

  res <- testthat::evaluate_promise(
    withr::with_dir(path_src, orderly_cleanup(dry_run = TRUE)))
  expect_match(res$messages, "I would delete 1 file from 'explicit':",
               all = FALSE)
  expect_setequal(dir(path_src), c("data.csv", "explicit.R", "mygraph.png"))
  expect_equal(res$result, status)

  res <- testthat::evaluate_promise(
    withr::with_dir(path_src, orderly_cleanup()))
  expect_match(res$messages, "Deleting 1 file from 'explicit':", all = FALSE)
  expect_setequal(dir(path_src), c("data.csv", "explicit.R"))
  expect_equal(res$result, status)
})


test_that("inform when running implicitly", {
  path <- test_prepare_orderly_example("implicit")
  helper_add_git(path)
  envir <- new.env()
  path_src <- file.path(path, "src", "implicit")
  withr::with_dir(path_src,
                  sys.source("implicit.R", envir))

  status <- withr::with_dir(path_src, orderly_cleanup_status())
  expect_equal(status$delete, character())
  expect_setequal(status$unknown, c("data.csv", "mygraph.png"))

  res <- testthat::evaluate_promise(print(status))
  expect_equal(res$result, status)
  expect_length(res$messages, 6)
  expect_match(res$messages[[1]], "implicit is not clean")
  expect_match(res$messages[[2]], "2 files have unknown status")
  expect_match(res$messages[[5]], "Mark these as resources")
  expect_match(res$messages[[6]], "Mark these as artefacts, dependencies")

  writeLines("mygraph.png", file.path(path_src, ".gitignore"))

  status <- withr::with_dir(path_src, orderly_cleanup_status())
  expect_equal(status$delete, "mygraph.png")
  expect_setequal(status$unknown, "data.csv")

  res <- testthat::evaluate_promise(print(status))
  expect_equal(res$result, status)
  expect_length(res$messages, 7)
  expect_match(res$messages[[1]], "implicit is not clean")
  expect_match(res$messages[[2]], "1 file can be deleted by running")
  expect_match(res$messages[[3]], "mygraph.png")
  expect_match(res$messages[[4]], "1 file has unknown status")
  expect_match(res$messages[[5]], "data.csv")
  expect_match(res$messages[[6]], "Mark these as resources")
  expect_match(res$messages[[7]], "Mark these as artefacts, dependencies")
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
    c("a", "a/x", "data.csv", "explicit.R"))
})


test_that("can clean up shared resources", {
  path <- test_prepare_orderly_example("shared")
  path_src <- file.path(path, "src", "shared")
  file.create(file.path(path_src, "shared_data.csv"))
  status <- orderly_cleanup_status("shared", root = path)

  files <- c("shared.R", "shared_data.csv")
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

test_that("can clean up shared resources with shorthand syntax", {
  path <- test_prepare_orderly_example("shared-shorthand")
  path_src <- file.path(path, "src", "shared-shorthand")
  file.create(file.path(path_src, "data.csv"))
  status <- orderly_cleanup_status("shared-shorthand", root = path)

  files <- c("data.csv", "shared-shorthand.R")
  expect_setequal(rownames(status$role), files)
  expect_equal(
    status$role,
    cbind(orderly = set_names(c(FALSE, TRUE), files),
          resource = FALSE,
          shared_resource = c(TRUE, FALSE),
          dependency = FALSE,
          artefact = FALSE))
  expect_equal(
    status$status,
    cbind(source = set_names(c(FALSE, TRUE), files),
          derived = c(TRUE, FALSE),
          ignored = NA))
  expect_equal(status$delete, "data.csv")
})


test_that("can clean up dependencies", {
  path <- test_prepare_orderly_example(c("data", "depends"))
  path_src <- file.path(path, "src", "depends")
  file.create(file.path(path_src, c("input.rds", "other.rds")))
  status <- orderly_cleanup_status("depends", root = path)

  files <- c("depends.R", "input.rds", "other.rds")
  expect_setequal(rownames(status$role), files)
  expect_equal(
    status$role,
    cbind(orderly = set_names(c(TRUE, FALSE, FALSE), files),
          resource = FALSE,
          shared_resource = FALSE,
          dependency = c(FALSE, TRUE, FALSE),
          artefact = FALSE))
  expect_equal(
    status$status,
    cbind(source = set_names(c(TRUE, FALSE, FALSE), files),
          derived = c(FALSE, TRUE, FALSE),
          ignored = NA))
  expect_equal(status$delete, "input.rds")
})


test_that("can clean up directories", {
  path <- test_prepare_orderly_example("directories")
  envir <- new.env()
  path_src <- file.path(path, "src", "directories")
  withr::with_dir(path_src,
                  sys.source("directories.R", envir))
  status <- orderly_cleanup_status("directories", root = path)

  files <- c("data/a.csv", "data/b.csv", "directories.R",
             "output/a.rds", "output/b.rds")
  expect_equal(rownames(status$role), files)
  expect_equal(
    status$role,
    cbind(orderly = set_names(files == "directories.R", files),
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
    c("data", "data/a.csv", "data/b.csv", "directories.R"))
})


test_that("Don't call cleanup on an active packet", {
  path <- test_prepare_orderly_example("data")
  path_src <- file.path(path, "src", "data")
  append_lines(file.path(path_src, "data.R"),
               "orderly2::orderly_cleanup_status()")
  expect_error(
    orderly_run_quietly("data", root = path, envir = new.env()),
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

  res <- testthat::evaluate_promise(print(status))
  expect_length(res$messages, 1)
  expect_match(res$messages,
               "data is clean, nothing to delete, nothing unknown")
  expect_equal(res$result, status)
})
