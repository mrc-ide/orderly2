test_that("can cleanup explicit things quite well", {
  path <- test_prepare_orderly_example("explicit")
  env <- new.env()
  path_src <- file.path(path, "src", "explicit")
  withr::with_dir(path_src,
                  sys.source("orderly.R", env))
  status <- withr::with_dir(path_src, orderly_cleanup_status())

  expect_s3_class(status, "orderly_cleanup_status")
  expect_setequal(names(status),
                  c("name", "root", "path", "role", "status", "delete"))
  expect_equal(status$name, "explicit")
  expect_equal(status$root, orderly_root(path, FALSE)$path)
  expect_equal(status$path, file.path(status$root, "src", status$name))
  paths <- c("data.csv", "mygraph.png", "orderly.R")
  expect_equal(status$role,
               cbind(orderly = set_names(c(FALSE, FALSE, TRUE), paths),
                     resource = c(TRUE, FALSE, FALSE),
                     global = FALSE,
                     dependency = FALSE,
                     artefact = c(FALSE, TRUE, FALSE)))
  expect_equal(status$status,
               cbind(source = set_names(c(TRUE, FALSE, TRUE), paths),
                     derived = c(FALSE, TRUE, FALSE),
                     ignored = NA))
  expect_equal(status$delete, "mygraph.png")

  status <- withr::with_dir(path_src, orderly_cleanup())
  expect_equal(dir(path_src), c("data.csv", "orderly.R"))
})


test_that("can clean up unknown files if gitignored", {
  path <- test_prepare_orderly_example("explicit")
  env <- new.env()
  path_src <- file.path(path, "src", "explicit")
  helper_add_git(path)

  fs::dir_create(file.path(path_src, c("a", "b/c", "b/d")))
  files <- c("a/x", "b/c/x", "b/c/y")
  file.create(file.path(path_src, files))

  status <- orderly_cleanup_status("explicit", path)
  expect_equal(
    status$status[c("a/x", "b/c/x", "b/c/y"), ],
    cbind(source = set_names(rep(FALSE, 3), files),
          derived = FALSE,
          ignored = FALSE))
  expect_equal(status$delete, character())

  writeLines(c("b/"), file.path(path_src, ".gitignore"))
  status <- orderly_cleanup_status("explicit", path)
  expect_equal(
    status$status[c("a/x", "b/c/x", "b/c/y"), ],
    cbind(source = set_names(rep(FALSE, 3), files),
          derived = FALSE,
          ignored = c(FALSE, TRUE, TRUE)))
  expect_equal(status$delete, c("b/c/x", "b/c/y"))

  status2 <- orderly_cleanup("explicit", path)
  expect_equal(status2, status)
  expect_setequal(
    dir(path_src, recursive = TRUE, include.dirs = TRUE),
    c("a", "a/x", "data.csv", "orderly.R"))
})
