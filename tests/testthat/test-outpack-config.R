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

  suppressMessages(
    orderly_config_set(core.require_complete_tree = TRUE, root = root))

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

  orderly_location_add_path("src", path = root$src$path, root = root$dst$path)
  orderly_location_fetch_metadata(root = root$dst$path)
  suppressMessages(
    orderly_location_pull(id[["c"]], root = root$dst$path))

  expect_equal(root$dst$index$data()$unpacked, id[["c"]])
  orderly_config_set(core.use_file_store = TRUE, root = root$dst)

  expect_true(root$dst$config$core$use_file_store)
  expect_true(orderly_config(root$dst$path)$core$use_file_store)
  expect_true(fs::dir_exists(root$dst$files$path))

  hash_pulled <- outpack_metadata_core(id[["c"]], root$dst)$files$hash
  expect_equal(length(hash_pulled), 3)

  dest <- temp_file()
  root$dst$files$get(hash_pulled[[1]], file.path(dest, "a"), TRUE)
  root$dst$files$get(hash_pulled[[2]], file.path(dest, "b"), TRUE)
  root$dst$files$get(hash_pulled[[3]], file.path(dest, "c"), TRUE)

  hash_not_pulled <- outpack_metadata_core(id[["a"]], root$dst)$files$hash
  expect_error(
    root$dst$files$get(hash_not_pulled[[1]], file.path(dest, "d"), TRUE),
    "not found in store")
})


test_that("File store is not added if a hash cannot be resolved", {
  root <- create_temporary_root()
  expect_false(root$config$core$use_file_store)

  id <- create_random_packet(root, name = "test")

  meta <- outpack_metadata_core(id, root)

  expect_equal(root$index$unpacked(), id)
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

  expect_equal(root$index$unpacked(), c(id, id_new))
  fs::file_delete(file.path(root$path, "archive", "data", id, "data.rds"))

  suppressMessages(orderly_config_set(core.use_file_store = TRUE, root = root))

  expect_true(root$config$core$use_file_store)

  dest <- temp_file()
  root$files$get(outpack_metadata_core(id, root)$files$hash, dest, TRUE)
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

  hash <- outpack_metadata_core(id[["c"]], root)$files$hash
  expect_equal(length(hash), 3)

  dest <- temp_file()
  root$files$get(hash[[1]], file.path(dest, "a"), TRUE)
  root$files$get(hash[[2]], file.path(dest, "b"), TRUE)
  root$files$get(hash[[3]], file.path(dest, "c"), TRUE)
})


test_that("Archive is not added if file store is corrupt", {
  root <- create_temporary_root(path_archive = NULL, use_file_store = TRUE)

  id <- create_random_packet_chain(root, 3)
  hash <- outpack_metadata_core(id[["c"]], root)$files$hash
  fs::file_delete(root$files$filename(hash[[1]]))
  expect_error(orderly_config_set(core.path_archive = "archive", root = root),
               "Error adding 'path_archive': Hash not found in store:")

  expect_null(root$config$core$path_archive)
  expect_false(fs::dir_exists(file.path(root$path, "archive")))
})


test_that("Validates path_archive", {
  root <- create_temporary_root(path_archive = NULL, use_file_store = TRUE)
  expect_error(orderly_config_set(core.path_archive = "/archive", root = root),
               "'path_archive' must be a relative path")
  expect_null(root$config$core$path_archive)

  dir.create(file.path(root$path, "archive"))
  expect_error(orderly_config_set(core.path_archive = "archive", root = root),
               "Directory already exists")
  expect_null(root$config$core$path_archive)

  orderly_config_set(core.path_archive = "new-archive", root = root)
  expect_error(orderly_config_set(core.path_archive = "/archive", root = root),
               "'path_archive' must be a relative path")
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
  orderly_location_add_path("src", path = root$src$path, root = root$dst$path)
  orderly_location_fetch_metadata(root = root$dst$path)
  suppressMessages(
    orderly_location_pull(id[["c"]], root = root$dst$path))
  expect_equal(root$dst$index$unpacked(), id[["c"]])

  suppressMessages(
    orderly_config_set(core.require_complete_tree = TRUE, root = root$dst$path))

  expect_setequal(root$dst$index$unpacked(), id)
  expect_true(orderly_config(root$dst$path)$core$require_complete_tree)
})


test_that("Unchanged require_complete_tree prints message", {
  root <- create_temporary_root()
  expect_false(root$config$core$require_complete_tree)
  expect_message(
    orderly_config_set(core.require_complete_tree = FALSE, root = root),
    "'core.require_complete_tree' was unchanged")
  suppressMessages(
    orderly_config_set(core.require_complete_tree = TRUE, root = root))
  expect_message(
    orderly_config_set(core.require_complete_tree = TRUE, root = root),
    "'core.require_complete_tree' was unchanged")
})
