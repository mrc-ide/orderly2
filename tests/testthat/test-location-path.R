test_that("can construct a orderly_location_path object", {
  root <- create_temporary_root()
  loc <- orderly_location_path$new(root$path)
  expect_s3_class(loc, "orderly_location_path")
  dat <- loc$list()
  expect_equal(nrow(dat), 0)
  expect_s3_class(dat, "data.frame")
  expect_equal(names(dat), c("packet", "time", "hash"))
})


test_that("orderly_location_path requires existing directory", {
  path <- temp_file()
  expect_error(
    orderly_location_path$new(path),
    "Directory does not exist:")
})


test_that("orderly_location_path requires exact root", {
  root <- create_temporary_root()
  subdir <- file.path(root$path, "subdir")
  dir.create(subdir)
  expect_error(
    orderly_location_path$new(subdir),
    "Did not find existing orderly (or outpack) root in",
    fixed = TRUE)

  expect_silent(orderly_location_path$new(root$path))
})


test_that("orderly_location_path returns list of packet ids", {
  root <- create_temporary_root()
  path <- root$path
  loc <- orderly_location_path$new(path)

  ids <- vcapply(1:3, function(i) create_random_packet(root$path))

  dat <- loc$list()
  expect_s3_class(dat, "data.frame")
  expect_equal(dat$packet, ids)
  expect_s3_class(dat$time, "POSIXt")
  str <- vcapply(file.path(path, ".outpack", "metadata", ids), read_string)
  expect_equal(
    dat$hash,
    vcapply(str, hash_data, "sha256", USE.NAMES = FALSE))
})


test_that("orderly_location_path can return metadata", {
  root <- create_temporary_root()
  path <- root$path
  loc <- orderly_location_path$new(path)

  ids <- vcapply(1:3, function(i) create_random_packet(path))
  str <- setNames(
    vcapply(file.path(path, ".outpack", "metadata", ids), read_string),
    ids)

  expect_equal(loc$metadata(ids[[2]]), str[2])
  expect_equal(loc$metadata(ids), str)
  expect_equal(loc$metadata(rep(ids[[1]], 2)), str[c(1, 1)])
})


test_that("requesting nonexistant metadata is an error", {
  root <- create_temporary_root()
  path <- root$path

  loc <- orderly_location_path$new(path)
  ids <- vcapply(1:3, function(i) create_random_packet(path))

  errs <- c("20220317-125935-ee5fd50e", "20220317-130038-48ffb8ba")
  expect_error(loc$metadata(errs[[1]]),
               "Some packet ids not found: '20220317-125935-ee5fd50e'")
  ## Truncating this error, will comma-separate all ids
  expect_error(
    loc$metadata(errs),
    "Some packet ids not found: '20220317-125935-ee5fd50e', '20220317")

  expect_error(loc$metadata(c(ids[[1]], errs[[1]], ids[[2]])),
               "Some packet ids not found: '20220317-125935-ee5fd50e'")
})


test_that("can locate files from the store", {
  root <- create_temporary_root(use_file_store = TRUE)
  path <- root$path

  loc <- orderly_location_path$new(path)
  ids <- vcapply(1:3, function(i) create_random_packet(path))

  files <- outpack_metadata_core(ids[[1]], root)$files

  h <- files$hash[files$path == "data.rds"]
  dest <- temp_file()
  res <- loc$fetch_file(h, dest)
  expect_identical(res, dest)
  expect_identical(hash_file(res), h)
})


test_that("sensible error if file not found in store", {
  root <- create_temporary_root(use_file_store = TRUE)
  path <- root$path

  loc <- orderly_location_path$new(path)
  h <- "md5:c7be9a2c3cd8f71210d9097e128da316"
  dest <- temp_file()
  expect_error(
    loc$fetch_file(h, dest),
    "Hash 'md5:c7be9a2c3cd8f71210d9097e128da316' not found at location")
  expect_false(file.exists(dest))
})


test_that("Can find file from archive", {
  root <- create_temporary_root(use_file_store = TRUE)
  path <- root$path

  loc <- orderly_location_path$new(path)
  ids <- vcapply(1:3, function(i) create_random_packet(path))
  idx <- root$index$data()

  files <- idx$metadata[[1]]$files
  h <- files$hash[files$path == "data.rds"]
  dest <- temp_file()
  res <- loc$fetch_file(h, dest)
  expect_identical(res, dest)
  expect_identical(hash_file(dest), h)
})


test_that("sensible error if file not found in archive", {
  root <- create_temporary_root(use_file_store = FALSE)
  path <- root$path

  loc <- orderly_location_path$new(path)
  h <- "md5:c7be9a2c3cd8f71210d9097e128da316"
  dest <- temp_file()
  expect_error(
    loc$fetch_file(h, dest),
    "Hash 'md5:c7be9a2c3cd8f71210d9097e128da316' not found at location")
  expect_false(file.exists(dest))
})


test_that("can detect differences between locations when destination empty", {
  client <- create_temporary_root()
  ids <- create_random_packet_chain(client, 4)

  server <- create_temporary_root(use_file_store = TRUE, path_archive = NULL)
  orderly_location_add_path("server", path = server$path, root = client)

  files <- lapply(ids, function(id) client$index$metadata(id)$files$hash)

  ## Simplest case; leaf node not known to the server.
  plan1 <- location_build_push_plan(ids[[1]], "server", client)
  expect_setequal(names(plan1), c("packet_id", "files"))
  expect_equal(plan1$packet_id, ids[[1]])
  expect_setequal(plan1$files, files[[1]])

  ## Whole tree:
  plan2 <- location_build_push_plan(ids[[4]], "server", client)
  expect_setequal(names(plan2), c("packet_id", "files"))
  expect_setequal(plan2$packet_id, ids)
  expect_setequal(plan2$files, unique(unlist(files, FALSE, FALSE)))

  ## Same if we use any of our ids explicitly:
  expect_equal(
    location_build_push_plan(ids, "server", client),
    location_build_push_plan(ids[[4]], "server", client))
  expect_equal(
    location_build_push_plan(ids[c(1, 4)], "server", client),
    location_build_push_plan(ids[[4]], "server", client))
})


test_that("Import complete tree via push into server", {
  client <- create_temporary_root()
  ids <- create_random_packet_chain(client, 4)

  server <- create_temporary_root(use_file_store = TRUE, path_archive = NULL)
  orderly_location_add_path("server", path = server$path, root = client)

  plan <- orderly_location_push(ids[[4]], "server", client)

  idx_c <- client$index$data()
  idx_s <- server$index$data()

  expect_equal(idx_s$metadata, idx_c$metadata)
  expect_equal(idx_s$unpacked, idx_c$unpacked)
  expect_equal(idx_s$location$packet, idx_c$location$packet)
  expect_equal(idx_s$location$hash, idx_c$location$hash)

  expect_setequal(plan$packet_id, ids)
  files_used <- lapply(ids, function(id) client$index$metadata(id)$files$hash)
  expect_setequal(plan$files, unique(unlist(files_used, FALSE, FALSE)))
})


test_that("Import packets into root with archive as well as store", {
  client <- create_temporary_root()
  ids <- create_random_packet_chain(client, 4)

  server <- create_temporary_root(use_file_store = TRUE,
                                  path_archive = "archive")
  orderly_location_add_path("server", path = server$path, root = client)

  plan <- orderly_location_push(ids[[4]], "server", client)

  expect_equal(
    sort(withr::with_dir(server$path, fs::dir_ls("archive", recurse = TRUE))),
    sort(withr::with_dir(client$path, fs::dir_ls("archive", recurse = TRUE))))
})


test_that("Prevent pushing things that would corrupt the store", {
  ## This can't actually happen without some deletion on the server I
  ## believe, which is going to require some race condition. But bugs
  ## could result in an incorrect plan being generated and these are
  ## the errors that would prevent the import going astray.
  client <- create_temporary_root()
  ids <- create_random_packet_chain(client, 4)

  server <- create_temporary_root(use_file_store = TRUE, path_archive = NULL)
  orderly_location_add_path("server", path = server$path, root = client)

  id <- ids[[3]]
  str <- read_string(file.path(client$path, ".outpack", "metadata", id))
  hash <- hash_data(str, "sha256")

  expect_error(
    location_path_import_metadata(str, chartr("bcdef", "cdefa", hash), server),
    sprintf("Hash of metadata for '%s' does not match", id))
  expect_error(
    location_path_import_metadata(str, hash, server),
    sprintf("Can't import metadata for '%s', as files missing", id))

  ## Manually import the files:
  for (h in client$index$metadata(id)$files$hash) {
    location_path_import_file(find_file_by_hash(client, h), h, server)
  }
  expect_error(
    location_path_import_metadata(str, hash, server),
    sprintf("Can't import metadata for '%s', as dependencies missing", id))
})


test_that("Can only push into a root with a file store", {
  ## This could possibly be relaxed, but it's hard to stash files
  ## somewhere without the store. Really in this condition the
  ## "server" should be pulling.
  client <- create_temporary_root()
  ids <- create_random_packet_chain(client, 2)
  server <- create_temporary_root()
  orderly_location_add_path("server", path = server$path, root = client)
  expect_error(
    orderly_location_push(ids[[2]], "server", client),
    "Can't push files into this server, as it does not have a file store")
})


test_that("pushing twice does nothing", {
  client <- create_temporary_root()
  ids <- create_random_packet_chain(client, 4)
  server <- create_temporary_root(use_file_store = TRUE, path_archive = NULL)
  orderly_location_add_path("server", path = server$path, root = client)
  plan1 <- orderly_location_push(ids[[4]], "server", client)
  plan2 <- orderly_location_push(ids[[4]], "server", client)
  expect_equal(plan2, list(packet_id = character(), files = character()))
})


test_that("push overlapping tree", {
  client <- create_temporary_root()
  server <- create_temporary_root(use_file_store = TRUE, path_archive = NULL)
  orderly_location_add_path("server", path = server$path, root = client)

  id_base <- create_random_packet(server)
  orderly_location_pull_metadata(root = client)
  suppressMessages(orderly_location_pull_packet(id_base, root = client))

  ids <- create_random_packet_chain(client, 3, id_base)
  plan <- orderly_location_push(ids[[3]], "server", client)

  expect_setequal(plan$packet_id, ids)
  expect_setequal(names(server$index$data()$metadata), c(id_base, ids))
})


test_that("Push single packet", {
  client <- create_temporary_root()
  id <- create_random_packet(client)

  server <- create_temporary_root(use_file_store = TRUE, path_archive = NULL)
  orderly_location_add_path("server", path = server$path, root = client)

  plan <- orderly_location_push(id, "server", client)

  idx_c <- client$index$data()
  idx_s <- server$index$data()

  expect_equal(idx_s$metadata, idx_c$metadata)
  expect_equal(idx_s$unpacked, idx_c$unpacked)
  expect_equal(idx_s$location$packet, idx_c$location$packet)
  expect_equal(idx_s$location$hash, idx_c$location$hash)

  expect_equal(plan$packet_id, id)
  files_used <- lapply(id, function(id) client$index$metadata(id)$files$hash)
  expect_setequal(plan$files, unique(unlist(files_used, FALSE, FALSE)))
})


test_that("Can read metadata files with a trailing newline", {
  # Past versions of orderly2 wrote metadata files with a trailing newline
  # character, despite the fact that the newline was not included when hashing.
  #
  # This has been fixed by not writing the newline anymore, but for
  # compatibility we need to ensure we can still read those metadata files and
  # get a correct hash.

  root <- create_temporary_root()
  id <- create_random_packet(root)
  path <- file.path(root$path, ".outpack", "metadata", id)

  # Calling writeLines adds the trailing newline and mimicks the old orderly2
  # behaviour. The size will be one or two bytes bigger than the actual data,
  # depending on whether the newline is `\n` or `\r\n`.
  old_size <- file.info(path)$size
  writeLines(read_string(path), path)
  expect_gte(file.info(path)$size, old_size + 1)

  # Reading the metadata from a location at that path correctly strips the
  # newline and hashes correctly.
  loc <- orderly_location_path$new(root$path)
  packets <- loc$list()
  data <- loc$metadata(id)
  expect_equal(nchar(data), old_size, ignore_attr = TRUE)

  expected_hash <- packets[packets$packet == id]$hash
  expect_no_error(hash_validate_data(data, expected_hash))
})


test_that("Fail to push sensibly if files have been changed", {
  client <- create_temporary_root()
  ids <- create_random_packet_chain(client, 4)

  server <- create_temporary_root(use_file_store = TRUE, path_archive = NULL)
  orderly_location_add_path("server", path = server$path, root = client)

  ## Corrupt one file:
  path <- file.path(client$path, "archive", "b", ids[["b"]], "script.R")
  forcibly_truncate_file(path)

  expect_error(
    suppressMessages(orderly_location_push(ids[[4]], "server", client)),
    "Did not find suitable file, can't push this packet")
})


test_that("allow relative paths in path locations", {
  tmp <- withr::local_tempdir()
  a <- suppressMessages(orderly_init(file.path(tmp, "a")))
  b <- suppressMessages(orderly_init(file.path(tmp, "b")))
  ids <- vcapply(1:3, function(i) create_random_packet(b))
  withr::with_dir(a, orderly_location_add_path("b", path = "../b"))
  orderly_location_pull_metadata(root = a)
  expect_equal(orderly_search(root = a, location = "b"), ids)
})


test_that("allow weird absolute paths in path locations", {
  tmp <- withr::local_tempdir()
  nms <- letters[1:3]
  root <- suppressMessages(
    set_names(lapply(nms, function(x) orderly_init(file.path(tmp, x))), nms))

  withr::with_dir(
    tmp, orderly_location_add_path("b", path = "../b", root = "a"))
  expect_equal(
    orderly_location_list(verbose = TRUE, root = root$a)$args[[2]]$path,
    "../b")

  fs::dir_create(file.path(root$a, "some/deep/path"))
  withr::with_dir(
    file.path(root$a, "some/deep/path"),
    orderly_location_add_path("c", path = "../c"))
  expect_equal(
    orderly_location_list(verbose = TRUE, root = root$a)$args[[3]]$path,
    "../c")
})


test_that("provide hint when wrong relative path given", {
  tmp <- withr::local_tempdir()
  tmp <- normalizePath(tmp)
  nms <- letters[1:3]
  root <- suppressMessages(
    set_names(lapply(nms, function(x) orderly_init(file.path(tmp, x))), nms))

  err <- expect_error(
    withr::with_dir(
      tmp, orderly_location_add_path("b", path = "b", root = "a")),
    "'path' must be given relative to the orderly root")
  expect_equal(err$body[[2]],
               "Consider passing '../b' instead")
})
