test_that("Initialisation requires empty directory", {
  tmp <- tempfile()
  fs::dir_create(tmp)
  on.exit(fs::dir_delete(tmp))
  file.create(file.path(tmp, "file"))
  expect_error(orderly_init_quietly(tmp),
               "'root' exists but is not empty, or an outpack archive")
})


test_that("Can initialise a new orderly root", {
  tmp <- tempfile()
  on.exit(fs::dir_delete(tmp))
  res <- orderly_init_quietly(tmp)
  expect_true(file.exists(tmp))
  expect_identical(normalise_path(res), normalise_path(tmp))
  root <- root_open(tmp, require_orderly = TRUE)
  expect_s3_class(root, "outpack_root")
  expect_equal(root$config$orderly,
               list(minimum_orderly_version = numeric_version("1.99.0")))
})


test_that("initialisation leaves things unchanged", {
  path <- test_prepare_orderly_example("plugin")
  cmp <- orderly_config(path)
  res <- orderly_init_quietly(path)
  expect_equal(orderly_config(path), cmp)
})


test_that("can turn an outpack root into an orderly one", {
  tmp <- withr::local_tempdir()
  outpack_init_no_orderly(tmp)

  orderly_init_quietly(tmp)
  root2 <- root_open(tmp, require_orderly = FALSE)
  expect_equal(root2$config$orderly,
               list(minimum_orderly_version = numeric_version("1.99.0")))
  expect_s3_class(root2, "outpack_root")
})


test_that("can initialise a repo with orderly but no .outpack directory", {
  path <- test_prepare_orderly_example("data")
  parent <- dirname(path)
  base <- basename(path)
  fs::dir_delete(file.path(path, ".outpack"))
  err <- expect_error(
    withr::with_dir(parent,
      orderly_run_quietly("data", root = base, envir = new.env())),
    sprintf("orderly directory '%s' not initialise", base))
  expect_equal(
    err$body,
    c(x = "Did not find an '.outpack' directory within path",
      i = sprintf('Please run orderly2::orderly_init("%s") to initialise',
                  base),
      i = "See ?orderly_init for more arguments to this function"))

  withr::with_dir(parent, orderly_init_quietly(base))
  root <- root_open(path, require_orderly = TRUE)
  expect_true(is_directory(file.path(path, ".outpack")))

  id <- withr::with_dir(parent,
    orderly_run_quietly("data", root = base, envir = new.env()))
  expect_type(id, "character")
})


test_that("Must include some packet storage", {
  path <- temp_file()
  expect_error(
    orderly_init_quietly(path, path_archive = NULL, use_file_store = FALSE),
    "If 'path_archive' is NULL, then 'use_file_store' must be TRUE")
  expect_false(file.exists(path))
})


test_that("Can control root config on initialisation", {
  root <- create_temporary_root(path_archive = NULL, use_file_store = TRUE,
                                require_complete_tree = TRUE)
  expect_mapequal(root$config$core,
                  list(path_archive = NULL,
                       use_file_store = TRUE,
                       require_complete_tree = TRUE,
                       hash_algorithm = "sha256"))
  expect_true(file.exists(file.path(root$path, ".outpack", "files")))
})


test_that("can log creation of outpack repo", {
  path <- temp_file()
  expect_message(
    root <- orderly_init(path, path_archive = "archive", use_file_store = TRUE),
    "Created orderly root at",
    fixed = TRUE)
})


test_that("Initialisation can't be done into a file", {
  tmp <- withr::local_tempfile()
  file.create(tmp)
  expect_error(orderly_init_quietly(tmp),
               "'root' exists but is not a directory")
})


test_that("can't reinitialise an outpack root with different arguments", {
  tmp <- withr::local_tempfile()
  root <- outpack_init_no_orderly(tmp)

  err <- expect_error(
    orderly_init_quietly(tmp, use_file_store = TRUE),
    "Trying to change configuration when re-initialising")
  expect_equal(
    err$body,
    c(x = "use_file_store: was 'FALSE' but was given 'TRUE'",
      i = "Use 'orderly2::orderly_config_set()' to change configuration"))

  err <- expect_error(
    orderly_init_quietly(tmp, use_file_store = TRUE, path_archive = "other"),
    "Trying to change configuration when re-initialising")
  expect_equal(
    err$body,
    c(x = "path_archive: was 'archive' but was given 'other'",
      x = "use_file_store: was 'FALSE' but was given 'TRUE'",
      i = "Use 'orderly2::orderly_config_set()' to change configuration"))
})


test_that("can reinitialise with specific arguments that match config", {
  tmp <- withr::local_tempfile()
  root <- outpack_init_no_orderly(tmp)
  res <- orderly_init_quietly(tmp, use_file_store = FALSE,
                              path_archive = "archive",
                              require_complete_tree = FALSE)
  expect_equal(normalise_path(tmp), normalise_path(res))

  tmp <- withr::local_tempfile()
  root <- outpack_init_no_orderly(tmp, path_archive = NULL,
                                  use_file_store = TRUE)
  res <- orderly_init_quietly(tmp, use_file_store = TRUE, path_archive = NULL,
                              require_complete_tree = FALSE)
  expect_equal(normalise_path(tmp), normalise_path(res))
})


test_that("inform about weirdly nested roots: orderly in outpack", {
  tmp <- withr::local_tempfile()
  root <- outpack_init_no_orderly(tmp)
  p <- file.path(tmp, "a", "b")
  fs::dir_create(p)
  file.create(file.path(p, "orderly_config.yml"))
  err <- expect_error(
    withr::with_dir(p, root_open(NULL, require_orderly = TRUE)),
    "Found incorrectly nested orderly and outpack directories")

  path_msg <- normalise_path(root$path)
  expect_equal(
    err$body,
    c(i = sprintf("outpack was found at '%s'", path_msg),
      i = sprintf("orderly was found at '%s/a/b'", path_msg),
      x = "orderly is nested within outpack at 'a/b'",
      i = "How did you even do this? Please let us know!"))
})


test_that("inform about weirdly nested roots: orderly in outpack", {
  tmp <- withr::local_tempfile()
  root <- orderly_init_quietly(tmp)
  p <- file.path(tmp, "a", "b")
  root2 <- outpack_init_no_orderly(p)
  err <- expect_error(
    withr::with_dir(p, root_open(NULL, require_orderly = TRUE)),
    "Found incorrectly nested orderly and outpack directories")
  expect_equal(
    err$body,
    c(i = sprintf("orderly was found at '%s'", root),
      i = sprintf("outpack was found at '%s/a/b'", root),
      x = "outpack is nested within orderly at 'a/b'",
      i = "How did you even do this? Please let us know!"))
})


test_that("create root in wd by default", {
  path <- withr::local_tempdir()
  root <- withr::with_dir(path, suppressMessages(orderly_init()))
  expect_true(file.exists(file.path(path, ".outpack")))
  expect_true(file.exists(file.path(path, "orderly_config.yml")))
})


test_that("allow rstudio files to exist for init", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "foo.Rproj"))
  dir.create(file.path(tmp, ".Rproj.user"))
  dir.create(file.path(tmp, ".git"))
  file.create(file.path(tmp, ".Rhistory"))
  file.create(file.path(tmp, ".gitignore"))

  expect_no_error(orderly_init_quietly(tmp))
  expect_true(file.exists(file.path(tmp, ".outpack")))
  expect_true(file.exists(file.path(tmp, "orderly_config.yml")))
})


test_that("force initialisation of non-empty directory", {
  tmp <- tempfile()
  fs::dir_create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  file.create(file.path(tmp, "file"))
  expect_no_error(orderly_init_quietly(tmp, force = TRUE))
  expect_true(file.exists(file.path(tmp, ".outpack")))
  expect_true(file.exists(file.path(tmp, "orderly_config.yml")))
})
