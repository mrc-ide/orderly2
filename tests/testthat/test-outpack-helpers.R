test_that("can copy files from outpack", {
  root <- create_temporary_root(use_file_store = TRUE)
  id <- create_random_packet(root)
  dst <- temp_file()
  res <-  orderly_copy_files(
    id, files = c("incoming.rds" = "data.rds"), dest = dst, root = root)
  expect_equal(dir(dst), "incoming.rds")
  expect_identical(
    readRDS(file.path(dst, "incoming.rds")),
    readRDS(file.path(root$path, "archive", "data", id, "data.rds")))
})


test_that("can copy files from location, using store", {
  here <- create_temporary_root(use_file_store = TRUE)
  there <- create_temporary_root(use_file_store = TRUE)
  orderly_location_add("there", "path", list(path = there$path), root = here)
  id <- create_random_packet(there)

  tmp <- withr::local_tempdir()
  expect_error(
    orderly_copy_files(id, files = c("data.rds" = "data.rds"), dest = tmp,
                       root = here),
    "id '.+' not found in index")
  orderly_location_pull_metadata(root = here)

  expect_error(
    orderly_copy_files(id, files = c("data.rds" = "data.rds"), dest = tmp,
                       root = here),
    "Unable to copy files, as they are not available locally")

  suppressMessages(
    orderly_copy_files(id, files = c("data.rds" = "data.rds"), dest = tmp,
                       options = list(allow_remote = TRUE), root = here))
  expect_equal(dir(tmp), "data.rds")

  meta <- orderly_metadata(id, root = there)
  hash <- meta$files$hash[meta$files$path == "data.rds"]
  expect_equal(hash_file(file.path(tmp, "data.rds"), "sha256"), hash)
  expect_equal(here$files$list(), hash)
})


test_that("can copy files from location, using archive", {
  here <- create_temporary_root(use_file_store = FALSE)
  there <- create_temporary_root(use_file_store = TRUE)
  orderly_location_add("there", "path", list(path = there$path), root = here)
  id <- create_random_packet(there)

  tmp <- withr::local_tempdir()
  orderly_location_pull_metadata(root = here)
  expect_error(
    orderly_copy_files(id, files = c("data.rds" = "data.rds"), dest = tmp,
                       root = here),
    "Unable to copy files, as they are not available locally")

  suppressMessages(
    orderly_copy_files(id, files = c("data.rds" = "data.rds"), dest = tmp,
                       options = list(allow_remote = TRUE), root = here))
  expect_equal(dir(tmp), "data.rds")

  meta <- orderly_metadata(id, there)
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
  suppressMessages(
    orderly_copy_files(id, files = c("${path}/${file}.rds" = "data.rds"),
                       dest = dst, root = root))
  expect_equal(dir(dst), "a")
  expect_equal(dir(file.path(dst, "a")), "b.rds")
  expect_identical(
    readRDS(file.path(dst, "a", "b.rds")),
    readRDS(file.path(root$path, "archive", "data", id, "data.rds")))
})


test_that("require a single id for search", {
  dst <- withr::local_tempdir()
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- replicate(3, outpack_id())
  expect_error(
    orderly_copy_files(ids, files = c(here = "there"), dest = dst, root = root),
    "Expected a length 1 value for first argument if id (not 3)",
    fixed = TRUE)
  expect_error(
    orderly_copy_files(character(), files = c(here = "there"), dest = dst,
                       root = root),
    "Expected a length 1 value for first argument if id (not 0)",
    fixed = TRUE)
})


test_that("require a single id for search", {
  dst <- withr::local_tempdir()
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- vcapply(1:3, function(i) create_random_packet(root))
  err <- expect_error(
    orderly_copy_files(name = "data", files = c(here = "there"),
                       dest = dst, root = root),
    "Query returned 3 results, expected a single result")
  expect_equal(err$body, c(i = "Did you forget latest()?"))

  err <- expect_error(
    orderly_copy_files(name = "missing", files = c(here = "there"),
                       dest = dst, root = root),
    "Query returned 0 results")
  expect_equal(err$body,
               c(i = "See 'rlang::last_error()$explanation' for details"))
  expect_equal(err$explanation,
               orderly_query_explain(NULL, name = "missing", root = root))
})
