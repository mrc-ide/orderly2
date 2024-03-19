export_info <- function(path) {
  listing <- zip::zip_list(path)$filename

  metadata <- grep("^metadata/.*[^/]$", listing, value = TRUE)
  metadata <- sub("^metadata/", "", metadata)

  files <- grep("^files/.*[^/]$", listing, value = TRUE)
  files <- sub("^files/", "", files)

  list(metadata = metadata, files = files)
}

test_that("Exporting a packet includes its transitive dependencies", {
  root <- create_temporary_root()
  ids <- create_random_packet_chain(root, 3)
  other <- create_random_packet(root)

  path <- withr::local_tempfile()
  orderly_export_zip(path, ids[[3]], root = root)

  info <- export_info(path)
  expect_setequal(info$metadata, ids)

  # The root packet has one file, and each downstream one has 2 (one source file
  # and one data). The downstreams actually have three, but one of them is a
  # copy of the parent packet's data, hence is deduplicated and doesn't count.
  expect_equal(length(info$files), 5)
})

test_that("Can export multiple packets", {
  root <- create_temporary_root()
  first <- create_random_packet(root)
  second <- create_random_packet(root)
  ids <- c(first, second)

  path <- withr::local_tempfile()
  orderly_export_zip(path, ids, root = root)

  info <- export_info(path)
  expect_setequal(info$metadata, ids)
  expect_equal(length(info$files), 2)
})

test_that("Can export from a file store", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 3)

  path <- withr::local_tempfile()
  orderly_export_zip(path, ids[[3]], root = root)

  info <- export_info(path)
  expect_setequal(info$metadata, ids)
  expect_equal(length(info$files), 5)
})

test_that("Packet files are de-duplicated when exported", {
  root <- create_temporary_root()
  ids <- c(create_deterministic_packet(root), create_deterministic_packet(root))

  path <- withr::local_tempfile()
  orderly_export_zip(path, ids, root = root)

  info <- export_info(path)
  expect_setequal(info$metadata, ids)
  expect_equal(length(info$files), 1)
})

test_that("Importing an invalid zip fails", {
  dir <- withr::local_tempfile()
  fs::dir_create(dir)
  fs::file_create(file.path(dir, "hello.txt"))

  zipfile <- withr::local_tempfile()
  zip::zip(zipfile, files = c("hello.txt"), root = dir)

  root <- create_temporary_root()
  expect_error(
    orderly_import_zip(zipfile, root = root),
    "Zip file does not contain an 'outpack.json' file at its root")
})

test_that("Can import a zip file", {
  upstream <- create_temporary_root()
  downstream <- create_temporary_root()

  id <- create_random_packet(upstream)

  path <- withr::local_tempfile()
  orderly_export_zip(path, id, root = upstream)

  imported <- orderly_import_zip(path, root = downstream)
  expect_equal(imported, id)

  index <- downstream$index$data()
  expect_setequal(names(index$metadata), id)
  expect_mapequal(index$metadata, upstream$index$data()$metadata)
  expect_setequal(index$unpacked, id)

  files <- upstream$index$metadata(id)$files
  file_paths <- file.path(downstream$path, downstream$config$core$path_archive,
                          upstream$index$metadata(id)$name, id, files$path)

  for (i in seq_along(file_paths)) {
    expect_no_error(hash_validate_file(file_paths[[i]], files$hash[[i]]))
  }
})

test_that("Can import a zip file to a file store", {
  upstream <- create_temporary_root()
  downstream <- create_temporary_root(use_file_store = TRUE)

  ids <- create_random_packet_chain(upstream, 3)

  path <- withr::local_tempfile()
  orderly_export_zip(path, ids[[3]], root = upstream)
  orderly_import_zip(path, root = downstream)

  index <- downstream$index$data()
  expect_setequal(names(index$metadata), ids)
  expect_mapequal(index$metadata, upstream$index$data()$metadata)
  expect_setequal(index$unpacked, ids)

  for (id in ids) {
    files <- upstream$index$metadata(id)$files
    expect_true(all(downstream$files$exists(files$hash)))
  }
})

test_that("Importing a zip file is idempotent", {
  upstream <- create_temporary_root()
  downstream <- create_temporary_root()

  id <- create_random_packet(upstream)

  path <- withr::local_tempfile()
  orderly_export_zip(path, id, root = upstream)
  imported_once <- orderly_import_zip(path, root = downstream)
  imported_twice <- orderly_import_zip(path, root = downstream)

  expect_equal(imported_once, id)
  expect_equal(imported_twice, id)

  index <- downstream$index$data()
  expect_setequal(names(index$metadata), id)
  expect_mapequal(index$metadata, upstream$index$data()$metadata)
  expect_setequal(index$unpacked, id)
})

test_that("New packets are imported", {
  upstream <- create_temporary_root()

  first_id <- create_random_packet(upstream)
  first_zip <- withr::local_tempfile()
  orderly_export_zip(first_zip, first_id, root = upstream)

  second_id <- create_random_packet(upstream)
  second_zip <- withr::local_tempfile()
  orderly_export_zip(second_zip, c(first_id, second_id), root = upstream)

  downstream <- create_temporary_root()

  orderly_import_zip(first_zip, root = downstream)
  index <- downstream$index$data()
  expect_setequal(names(index$metadata), first_id)
  expect_setequal(index$unpacked, first_id)

  orderly_import_zip(second_zip, root = downstream)
  index <- downstream$index$data()
  expect_setequal(names(index$metadata), c(first_id, second_id))
  expect_mapequal(index$metadata, upstream$index$data()$metadata)
  expect_setequal(index$unpacked, c(first_id, second_id))
})

test_that("Can import packet with existing metadata", {
  upstream <- create_temporary_root(use_file_store = TRUE)
  id <- create_random_packet(upstream)

  # We want to bring in the packets metadata into the downstream repository,
  # but not copy any of the actual files (yet). We do this by adding a path
  # location and pulling the metadata from it.
  downstream <- create_temporary_root()
  orderly_location_add("upstream", "path", list(path = upstream$path),
                       root = downstream)
  orderly_location_pull_metadata(root = downstream)

  index <- downstream$index$data()
  expect_setequal(names(index$metadata), id)
  expect_equal(length(index$unpacked), 0)

  path <- withr::local_tempfile()
  orderly_export_zip(path, id, root = upstream)
  orderly_import_zip(path, root = downstream)

  index <- downstream$index$data()
  expect_setequal(names(index$metadata), id)
  expect_setequal(index$unpacked, id)
})

test_that("Importing a zip file with mismatching metadata fails", {
  upstream <- create_temporary_root()
  downstream <- create_temporary_root()

  id <- outpack_id()
  create_random_packet(upstream, id = id)
  create_random_packet(downstream, id = id)

  path <- withr::local_tempfile()
  orderly_export_zip(path, id, root = upstream)

  expect_error(
    orderly_import_zip(path, root = downstream),
    "Imported file has conflicting metadata")
})
