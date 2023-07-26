test_that("Validate inputs to config set", {
  root <- create_temporary_root()

  expect_error(
    orderly_config_set(TRUE, root = root),
    "'options' must be named")
  expect_error(
    orderly_config_set(a = 1, options = list(b = 2), root = root),
    "If 'options' is given, no dot arguments are allowed")
})


test_that("Setting no options does nothing", {
  root <- create_temporary_root()
  config <- root$config
  orderly_config_set(root = root)
  expect_identical(orderly_config(root$path), config)
})


test_that("Disallow unknown configuration options", {
  root <- create_temporary_root()
  expect_error(
    orderly_config_set(whatever = TRUE, root = root),
    "Can't set configuration option: 'whatever'")
})


test_that("Can update core.require_complete_tree in empty archive", {
  root <- create_temporary_root()
  expect_false(root$config$core$require_complete_tree)

  orderly_config_set(core.require_complete_tree = TRUE, root = root)

  expect_true(root$config$core$require_complete_tree)
  expect_true(orderly_config(root$path)$core$require_complete_tree)
})


test_that("Can remove file_store if path_archive exists", {
  root <- create_temporary_root(use_file_store = TRUE)
  expect_true(root$config$core$use_file_store)

  file_store <- root$files$path
  expect_true(fs::dir_exists(file_store))

  expect_message(orderly_config_set(core.use_file_store = TRUE, root = root),
                 "'core.use_file_store' was unchanged")
  expect_true(root$config$core$use_file_store)
  expect_true(fs::dir_exists(file_store))

  orderly_config_set(core.use_file_store = FALSE, root = root)

  expect_false(root$config$core$use_file_store)
  expect_false(orderly_config(root$path)$core$use_file_store)
  expect_false(fs::dir_exists(file_store))
})


test_that("Cannot remove file_store if no path_archive", {
  root <- create_temporary_root(use_file_store = TRUE, path_archive = NULL)
  file_store <- root$files$path
  expect_true(fs::dir_exists(file_store))
  expect_error(orderly_config_set(core.use_file_store = FALSE, root = root),
               "If 'path_archive' is NULL, then 'use_file_store' must be TRUE")

  expect_true(root$config$core$use_file_store)
  expect_true(orderly_config(root$path)$core$use_file_store)
  expect_true(fs::dir_exists(file_store))
})


test_that("Can add file_store", {
  root <- list()
  root$src <- create_temporary_root()
  root$dst <- create_temporary_root()

  expect_false(root$dst$config$core$use_file_store)

  id <- create_random_packet_chain(root$src, 3)

  outpack_location_add("src", "path", list(path = root$src$path),
                       root = root$dst$path)
  outpack_location_pull_metadata(root = root$dst$path)
  outpack_location_pull_packet(id[["c"]], root = root$dst$path)

  expect_equal(root$dst$index()$unpacked, id[["c"]])
  orderly_config_set(core.use_file_store = TRUE, root = root$dst)

  expect_true(root$dst$config$core$use_file_store)
  expect_true(orderly_config(root$dst$path)$core$use_file_store)
  expect_true(fs::dir_exists(root$dst$files$path))

  hash_pulled <- root$dst$metadata(id[["c"]])$files$hash
  expect_equal(length(hash_pulled), 4)

  dest <- temp_file()
  dir.create(dest)
  root$dst$files$get(hash_pulled[[1]], dest, TRUE)
  root$dst$files$get(hash_pulled[[2]], dest, TRUE)
  root$dst$files$get(hash_pulled[[3]], dest, TRUE)
  root$dst$files$get(hash_pulled[[4]], dest, TRUE)

  hash_not_pulled <- root$dst$metadata(id[["a"]])$files$hash
  expect_error(root$dst$files$get(hash_not_pulled[[1]], dest, TRUE),
               "not found in store")
})


test_that("File store is not added if a hash cannot be resolved", {
  root <- create_temporary_root()
  expect_false(root$config$core$use_file_store)

  id <- create_random_packet(root, name = "test")

  meta <- root$metadata(id)

  expect_equal(root$index()$unpacked, id)
  fs::file_delete(file.path(root$path, "archive", "test", id, "data.rds"))
  regex <- paste("Error adding file store:(.*)",
                 "the following files were missing or corrupted: 'data.rds'")

  expect_error(suppressMessages(
    orderly_config_set(core.use_file_store = TRUE, root = root)),
    regex)
  expect_false(root$config$core$use_file_store)
  expect_null(root$config$files)
})


test_that("Files will be searched for by hash when adding file store", {
  root <- create_temporary_root()
  expect_false(root$config$core$use_file_store)

  id <- create_deterministic_packet(root, name = "data")
  id_new <- create_deterministic_packet(root, name = "data-new")

  expect_equal(root$index()$unpacked, c(id, id_new))
  fs::file_delete(file.path(root$path, "archive", "data", id, "data.rds"))

  suppressMessages(orderly_config_set(core.use_file_store = TRUE, root = root))

  expect_true(root$config$core$use_file_store)

  dest <- temp_file()
  dir.create(dest)
  root$files$get(root$metadata(id)$files$hash, dest, TRUE)
})


test_that("Can remove uninitialised archive if using file store", {
  root <- create_temporary_root(path_archive = "archive",
                                use_file_store = TRUE)

  expect_equal(root$config$core$path_archive, "archive")
  orderly_config_set(core.path_archive = NULL, root = root)
  expect_null(root$config$core$path_archive)
})


test_that("Can remove initialised archive if using file store", {
  root <- create_temporary_root(path_archive = "archive",
                                use_file_store = TRUE)

  expect_equal(root$config$core$path_archive, "archive")
  expect_message(orderly_config_set(core.path_archive = "archive", root = root),
                 "'core.path_archive' was unchanged")

  create_random_packet(root)
  path_archive <- file.path(root$path, "archive")
  expect_true(fs::dir_exists(path_archive))

  orderly_config_set(core.path_archive = NULL, root = root)
  expect_null(root$config$core$path_archive)
  expect_false(fs::dir_exists(path_archive))

  expect_message(orderly_config_set(core.path_archive = NULL, root = root),
                 "'core.path_archive' was unchanged")
})


test_that("Can rename uninitialised archive", {
  root <- create_temporary_root(path_archive = "archive")

  expect_equal(root$config$core$path_archive, "archive")

  orderly_config_set(core.path_archive = "new", root = root)

  expect_equal(root$config$core$path_archive, "new")
})


test_that("Can rename initialised archive", {
  root <- create_temporary_root(path_archive = "archive")

  create_random_packet(root)

  expect_equal(root$config$core$path_archive, "archive")
  path_archive <- file.path(root$path, "archive")
  expect_true(fs::dir_exists(path_archive))

  orderly_config_set(core.path_archive = "new", root = root)

  expect_equal(root$config$core$path_archive, "new")
  path_archive_new <- file.path(root$path, "new")
  expect_false(fs::dir_exists(path_archive))
  expect_true(fs::dir_exists(path_archive_new))
})


test_that("Cannot remove archive if not using file store", {
  root <- create_temporary_root(path_archive = "archive")

  expect_error(orderly_config_set(core.path_archive = NULL, root = root),
               "If 'path_archive' is NULL, then 'use_file_store' must be TRUE")
})


test_that("Can add archive", {
  root <- create_temporary_root(path_archive = NULL, use_file_store = TRUE)
  path <- root$path

  id <- create_random_packet_chain(root, 3)
  orderly_config_set(core.path_archive = "archive", root = root)

  expect_equal(root$config$core$path_archive, "archive")
  expect_true(fs::dir_exists(file.path(path, "archive")))
  expect_true(fs::dir_exists(file.path(path, "archive", "a", id[["a"]])))
  expect_true(fs::dir_exists(file.path(path, "archive", "b", id[["b"]])))
  expect_true(fs::dir_exists(file.path(path, "archive", "c", id[["c"]])))

  expect_true(fs::file_exists(file.path(path, "archive", "c", id[["c"]],
                                        "script.R")))
  expect_true(fs::file_exists(file.path(path, "archive", "c", id[["c"]],
                                        "input.rds")))
  expect_true(fs::file_exists(file.path(path, "archive", "c", id[["c"]],
                                        "data.rds")))

  # remove file store, recreate it from the archive, then remove archive
  # and check the remaining file store is valid
  orderly_config_set(core.use_file_store = FALSE, root = root)
  orderly_config_set(core.use_file_store = TRUE, root = root)
  orderly_config_set(core.path_archive = NULL, root = root)

  hash <- root$metadata(id[["c"]])$files$hash
  expect_equal(length(hash), 4)

  dest <- temp_file()
  dir.create(dest)
  root$files$get(hash[[1]], dest, TRUE)
  root$files$get(hash[[2]], dest, TRUE)
  root$files$get(hash[[3]], dest, TRUE)
  root$files$get(hash[[4]], dest, TRUE)
})


test_that("Archive is not added if file store is corrupt", {
  root <- create_temporary_root(path_archive = NULL, use_file_store = TRUE)

  id <- create_random_packet_chain(root, 3)
  hash <- root$metadata(id[["c"]])$files$hash
  fs::file_delete(root$files$filename(hash[[1]]))
  expect_error(orderly_config_set(core.path_archive = "archive", root = root),
               "Error adding 'path_archive': Hash not found in store:")

  expect_null(root$config$core$path_archive)
  expect_false(fs::dir_exists(file.path(root$path, "archive")))
})


test_that("Validates path_archive", {
  root <- create_temporary_root(path_archive = NULL, use_file_store = TRUE)
  expect_error(orderly_config_set(core.path_archive = "/archive", root = root),
               "'path_archive' must be relative path")
  expect_null(root$config$core$path_archive)

  dir.create(file.path(root$path, "archive"))
  expect_error(orderly_config_set(core.path_archive = "archive", root = root),
               "Directory already exists")
  expect_null(root$config$core$path_archive)

  orderly_config_set(core.path_archive = "new-archive", root = root)
  expect_error(orderly_config_set(core.path_archive = "/archive", root = root),
               "'path_archive' must be relative path")
  expect_error(orderly_config_set(core.path_archive = "archive", root = root),
               "Directory already exists")
  expect_equal(root$config$core$path_archive, "new-archive")
})


test_that("Enabling recursive pulls forces pulling missing packets", {
  root <- list()
  root$src <- create_temporary_root()
  root$dst <- create_temporary_root()

  expect_false(root$dst$config$core$require_complete_tree)

  id <- create_random_packet_chain(root$src, 3)
  outpack_location_add("src", "path", list(path = root$src$path),
                       root = root$dst$path)
  outpack_location_pull_metadata(root = root$dst$path)
  outpack_location_pull_packet(id[["c"]], root = root$dst$path)
  expect_equal(root$dst$index()$unpacked, id[["c"]])

  orderly_config_set(core.require_complete_tree = TRUE, root = root$dst$path)

  expect_setequal(root$dst$index()$unpacked, id)
  expect_true(orderly_config(root$dst$path)$core$require_complete_tree)
})


test_that("Unchanged require_complete_tree prints message", {
  root <- create_temporary_root()
  expect_false(root$config$core$require_complete_tree)
  expect_message(
    orderly_config_set(core.require_complete_tree = FALSE, root = root),
    "'core.require_complete_tree' was unchanged")
  expect_silent(
    orderly_config_set(core.require_complete_tree = TRUE, root = root))
  expect_message(
    orderly_config_set(core.require_complete_tree = TRUE, root = root),
    "'core.require_complete_tree' was unchanged")
})


test_that("can set logging threshold", {
  root <- create_temporary_root()
  expect_equal(root$config$logging,
               list(console = FALSE, threshold = "info"))

  orderly_config_set(logging.threshold = "debug", root = root)
  expect_equal(root$config$logging$threshold, "debug")
  expect_equal(orderly_config(root$path)$logging$threshold, "debug")
})


test_that("reject invalid logging thresholds", {
  root <- create_temporary_root()
  expect_error(
    orderly_config_set(logging.threshold = "unknown", root = root),
    "logging.threshold must be one of 'info', 'debug', 'trace'",
    fixed = TRUE)
})


test_that("can control console logging", {
  root <- create_temporary_root()
  orderly_config_set(logging.console = TRUE, root = root)
  expect_true(root$config$logging$console)

  root2 <- root_open(root$path, FALSE, FALSE)
  expect_true(root2$config$logging$console)

  orderly_config_set(logging.console = FALSE, root = root)
  expect_false(root$config$logging$console)

  root3 <- root_open(root$path, FALSE, FALSE)
  expect_false(root3$config$logging$console)
})


test_that("loading config without logging adds defaults", {
  root <- create_temporary_root()
  config_remove_logging(root$path)
  root2 <- root_open(root$path, FALSE, FALSE)
  expect_equal(root2$config$logging, list(console = TRUE, threshold = "info"))
})
