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
  expect_true(fs::file_access(file.path(dst, "incoming.rds"), "write"))
})


test_that("can copy files from location, using store", {
  here <- create_temporary_root(use_file_store = TRUE)
  there <- create_temporary_root(use_file_store = TRUE)
  orderly_location_add_path("there", path = there$path, root = here)
  id <- create_random_packet(there)

  tmp <- withr::local_tempdir()
  expect_error(
    orderly_copy_files(id, files = c("data.rds" = "data.rds"), dest = tmp,
                       root = here),
    "Packet '.+' not found in outpack index")
  orderly_location_fetch_metadata(root = here)

  expect_error(
    orderly_copy_files(id, files = c("data.rds" = "data.rds"), dest = tmp,
                       root = here),
    "Unable to copy files, as they are not available locally")

  suppressMessages(
    orderly_copy_files(id, files = c("data.rds" = "data.rds"), dest = tmp,
                       allow_remote = TRUE, root = here))
  expect_equal(dir(tmp), "data.rds")

  meta <- orderly_metadata(id, root = there)
  hash <- meta$files$hash[meta$files$path == "data.rds"]
  expect_equal(hash_file(file.path(tmp, "data.rds"), "sha256"), hash)
  expect_equal(here$files$list(), hash)
})


test_that("can copy files from location, using archive", {
  here <- create_temporary_root(use_file_store = FALSE)
  there <- create_temporary_root(use_file_store = TRUE)
  orderly_location_add_path("there", path = there$path, root = here)
  id <- create_random_packet(there)

  tmp <- withr::local_tempdir()
  orderly_location_fetch_metadata(root = here)
  expect_error(
    orderly_copy_files(id, files = c("data.rds" = "data.rds"), dest = tmp,
                       root = here),
    "Unable to copy files, as they are not available locally")

  suppressMessages(
    orderly_copy_files(id, files = c("data.rds" = "data.rds"), dest = tmp,
                       allow_remote = TRUE, root = here))
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
    "Expected a length 1 value for 'expr' if id (not 3)",
    fixed = TRUE)
  expect_error(
    orderly_copy_files(character(), files = c(here = "there"), dest = dst,
                       root = root),
    "Expected a length 1 value for 'expr' if id (not 0)",
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


test_that("good error message if file not found in packet", {
  dst <- withr::local_tempdir()
  root <- create_temporary_root(use_file_store = TRUE)

  src <- fs::dir_create(file.path(dst, "src"))
  file.create(file.path(src, c("a.txt", "b.txt", "c.txt")))
  p <- outpack_packet_start_quietly(src, "data", root = root)
  outpack_packet_end_quietly(p)

  id <- p$id

  expect_null(validate_packet_has_file(root, id, "a.txt"))

  err <- expect_error(
    validate_packet_has_file(root, id, "a.TXT"),
    "Packet '.+' does not contain the requested path\\s*'a.TXT'")
  expect_equal(
    err$body,
    c(i = "For 'a.TXT' did you mean 'a.txt'",
      i = "Remember that all orderly paths are case sensitive"))

  err <- expect_error(
    validate_packet_has_file(root, id, "d.txt"),
    "Packet '.+' does not contain the requested path\\s*'d.txt'")
  expect_equal(
    err$body,
    c(i = "For 'd.txt' did you mean 'a.txt', 'b.txt' or 'c.txt'"))

  err <- expect_error(
    validate_packet_has_file(root, id, c("a.txt", "a.TXT", "d.txt")),
    "Packet '.+' does not contain the requested paths\\s*'a.TXT' and 'd.txt'")
  expect_equal(
    err$body,
    c(i = "For 'a.TXT' did you mean 'a.txt'",
      i = "For 'd.txt' did you mean 'a.txt', 'b.txt' or 'c.txt'",
      i = "Remember that all orderly paths are case sensitive"))
})


test_that("Can overwrite when copying files from packet", {
  root <- create_temporary_root(use_file_store = TRUE)

  id1 <- create_random_packet(root)
  id2 <- create_random_packet(root)

  # Just a bit of a sanity check. The two packets are random, so we'd expect
  # them to have different contents.
  expect_false(identical(
    readRDS(file.path(root$path, "archive", "data", id1, "data.rds")),
    readRDS(file.path(root$path, "archive", "data", id2, "data.rds"))))

  dst <- temp_file()

  suppressMessages(
    orderly_copy_files(id1, files = "data.rds", dest = dst, root = root))

  expect_identical(
    readRDS(file.path(dst, "data.rds")),
    readRDS(file.path(root$path, "archive", "data", id1, "data.rds")))

  suppressMessages(
    orderly_copy_files(id2, files = "data.rds", dest = dst, root = root))

  expect_identical(
    readRDS(file.path(dst, "data.rds")),
    readRDS(file.path(root$path, "archive", "data", id2, "data.rds")))
})
