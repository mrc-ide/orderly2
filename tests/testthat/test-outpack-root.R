test_that("can create new root", {
  root <- create_temporary_root()
  path <- root$path
  expect_s3_class(root, "outpack_root")

  expect_true(file.exists(file.path(path, ".outpack", "metadata")))
  expect_true(file.exists(file.path(path, ".outpack", "location")))
  expect_mapequal(root$config$core,
                  list(path_archive = "archive",
                       use_file_store = FALSE,
                       require_complete_tree = FALSE,
                       hash_algorithm = "sha256"))
  expect_false(file.exists(file.path(path, ".outpack", "files")))
  expect_equal(orderly_location_list(root = root), "local")
})


test_that("Can locate an outpack root", {
  root <- create_temporary_root()
  path <- root$path
  p <- file.path(path, "a", "b", "c")
  fs::dir_create(p)
  expect_equal(
    root_open(p, locate = TRUE, require_orderly = FALSE)$path,
    root_open(path, locate = FALSE, require_orderly = FALSE)$path)
  expect_equal(
    withr::with_dir(
      p,
      root_open(".", locate = TRUE, require_orderly = FALSE)$path),
    root_open(path, locate = FALSE, require_orderly = FALSE)$path)
  expect_identical(
    root_open(root, locate = FALSE, require_orderly = FALSE), root)
})


test_that("root_open errors if it reaches toplevel", {
  path <- temp_file()
  fs::dir_create(path)
  expect_error(
    root_open(path, locate = TRUE, require_orderly = FALSE),
    "Did not find existing orderly (or outpack) root in",
    fixed = TRUE)
})


test_that("root_open does not recurse if locate = FALSE", {
  root <- create_temporary_root()
  path <- root$path
  expect_identical(
    root_open(root, locate = FALSE, require_orderly = FALSE),
    root)
  expect_equal(
    root_open(path, locate = FALSE, require_orderly = FALSE)$path,
    path)

  p <- file.path(path, "a", "b", "c")
  fs::dir_create(p)
  expect_error(
    root_open(p, locate = FALSE, require_orderly = FALSE),
    "Did not find existing orderly (or outpack) root in",
    fixed = TRUE)
})


test_that("root configuration matches schema", {
  skip_if_not_installed("jsonvalidate")
  root <- create_temporary_root()
  path_config <- file.path(root$path, ".outpack", "config.json")
  expect_true(load_schema("outpack/config.json")$validate(path_config))
})


test_that("Can't get nonexistant metadata", {
  root <- create_temporary_root(path_archive = NULL, use_file_store = TRUE)
  id <- outpack_id()
  expect_error(
    outpack_metadata_core(id, root),
    sprintf("Packet '%s' not found in outpack index", id))
})


test_that("empty root has nothing unpacked", {
  root <- create_temporary_root()
  expect_equal(root$index$unpacked(), character())
})


## As used in orderly:
test_that("can find appropriate root if in working directory with path NULL", {
  root <- create_temporary_root()
  res <- withr::with_dir(
    root$path,
    root_open(NULL, locate = TRUE, require_orderly = FALSE))
  expect_equal(res$path, root$path)
})


test_that("Can work out what packets are missing", {
  root <- list()
  for (name in c("a", "b")) {
    root[[name]] <- create_temporary_root()
  }
  ids <- create_random_packet_chain(root$a, 3)
  orderly_location_add("a", "path", list(path = root$a$path), root = root$b)
  orderly_location_pull_metadata(root = root$b)

  ## Nothing missing in this case:
  expect_equal(root_list_unknown_packets(ids, root$a),
               character())
  expect_equal(root_list_unknown_packets(ids[[1]], root$a),
               character())
  expect_equal(root_list_unknown_packets(character(), root$a),
               character())

  ids_fake <- replicate(4, outpack_id())
  expect_equal(root_list_unknown_packets(ids_fake, root$a),
               ids_fake)
  expect_equal(root_list_unknown_packets(rev(ids_fake), root$a),
               rev(ids_fake))

  ids_all <- sample(c(unname(ids), ids_fake))
  expect_equal(root_list_unknown_packets(ids_all, root$a),
               setdiff(ids_all, ids))
  expect_equal(root_list_unknown_packets(ids_all, root$b),
               ids_all)
})


test_that("Can work out what files are missing", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 3)

  files <- root$files$list()
  files_msg <- hash_data(files, "sha256")

  expect_equal(root_list_unknown_files(files, root), character())
  expect_equal(root_list_unknown_files(files[[1]], root), character())
  expect_equal(root_list_unknown_files(character(), root), character())

  expect_equal(root_list_unknown_files(files_msg, root), files_msg)
  expect_equal(root_list_unknown_files(rev(files_msg), root), rev(files_msg))

  files_all <- sample(c(files, files_msg))
  expect_equal(root_list_unknown_files(files_all, root),
               setdiff(files_all, files))
})


test_that("Can work out what files are missing without file store", {
  root <- create_temporary_root(use_file_store = FALSE)
  ids <- create_random_packet_chain(root, 3)

  files <- unique(
    unlist(lapply(root$index$data()$metadata, function(x) x$files$hash),
           FALSE, FALSE))
  files_msg <- hash_data(files, "sha256")

  expect_equal(root_list_unknown_files(files, root), character())
  expect_equal(root_list_unknown_files(files[[1]], root), character())
  expect_equal(root_list_unknown_files(character(), root), character())

  expect_equal(root_list_unknown_files(files_msg, root), files_msg)
  expect_equal(root_list_unknown_files(rev(files_msg), root), rev(files_msg))

  files_all <- sample(c(files, files_msg))
  expect_equal(root_list_unknown_files(files_all, root),
               setdiff(files_all, files))
})


test_that("All files missing in empty archive", {
  root <- create_temporary_root(use_file_store = FALSE)
  files_msg <- hash_data(1:10, "sha256")
  expect_equal(root_list_unknown_files(files_msg, root), files_msg)
})
