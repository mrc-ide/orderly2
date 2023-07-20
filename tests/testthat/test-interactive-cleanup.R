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
