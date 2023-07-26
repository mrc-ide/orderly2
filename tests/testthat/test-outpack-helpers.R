test_that("can copy files from outpack", {
  root <- create_temporary_root(use_file_store = TRUE)
  id <- create_random_packet(root)
  dst <- temp_file()
  orderly_copy_files(id, c("incoming.rds" = "data.rds"), dst, root = root)
  expect_equal(dir(dst), "incoming.rds")
  expect_identical(
    readRDS(file.path(dst, "incoming.rds")),
    readRDS(file.path(root$path, "archive", "data", id, "data.rds")))
})


test_that("can copy files from location, using store", {
  here <- create_temporary_root(use_file_store = TRUE)
  there <- create_temporary_root(use_file_store = TRUE)
  outpack_location_add("there", "path", list(path = there$path), root = here)
  id <- create_random_packet(there)

  tmp <- withr::local_tempdir()
  expect_error(
    orderly_copy_files(id, c("data.rds" = "data.rds"), tmp, root = here),
    "id '.+' not found in index")
  outpack_location_pull_metadata(root = here)

  expect_error(
    orderly_copy_files(id, c("data.rds" = "data.rds"), tmp, root = here),
    "Unable to copy files, as they are not available locally")

  orderly_copy_files(id, c("data.rds" = "data.rds"), tmp,
                     allow_remote = TRUE, root = here)
  expect_equal(dir(tmp), "data.rds")

  meta <- there$metadata(id)
  hash <- meta$files$hash[meta$files$path == "data.rds"]
  expect_equal(hash_file(file.path(tmp, "data.rds"), "sha256"), hash)
  expect_equal(here$files$list(), hash)
})


test_that("can copy files from location, using archive", {
  here <- create_temporary_root(use_file_store = FALSE)
  there <- create_temporary_root(use_file_store = TRUE)
  outpack_location_add("there", "path", list(path = there$path), root = here)
  id <- create_random_packet(there)

  tmp <- withr::local_tempdir()
  outpack_location_pull_metadata(root = here)
  expect_error(
    orderly_copy_files(id, c("data.rds" = "data.rds"), tmp, root = here),
    "Unable to copy files, as they are not available locally")

  orderly_copy_files(id, c("data.rds" = "data.rds"), tmp,
                     allow_remote = TRUE, root = here)
  expect_equal(dir(tmp), "data.rds")

  meta <- there$metadata(id)
  hash <- meta$files$hash[meta$files$path == "data.rds"]
  expect_equal(hash_file(file.path(tmp, "data.rds"), "sha256"), hash)
})


test_that("can interpolate filenames in copy", {
  root <- create_temporary_root(use_file_store = TRUE)
  id <- create_random_packet(root)
  dst <- temp_file()
  ## Some bindings to force lookup:
  path <- "a"
  file <- "b"
  orderly_copy_files(id, c("${path}/${file}.rds" = "data.rds"),
                     dst, root = root)
  expect_equal(dir(dst), "a")
  expect_equal(dir(file.path(dst, "a")), "b.rds")
  expect_identical(
    readRDS(file.path(dst, "a", "b.rds")),
    readRDS(file.path(root$path, "archive", "data", id, "data.rds")))
})
