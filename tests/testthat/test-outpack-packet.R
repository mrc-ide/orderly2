test_that("Can run a basic packet", {
  path_src <- temp_file()
  fs::dir_create(path_src)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path_src, "data.csv"),
            row.names = FALSE)

  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path <- root$path

  p <- outpack_packet_start(path_src, "example", root = root)
  expect_s3_class(p, "outpack_packet")
  expect_null(p$complete)

  outpack_packet_run(p, "script.R")
  expect_true(file.exists(file.path(path_src, "myplot.png")))

  outpack_packet_end(p)
  expect_true(p$complete)

  index <- root$index()
  expect_length(index$metadata, 1)
  id <- p$id

  path_metadata <- file.path(path, ".outpack", "metadata", id)
  expect_true(file.exists(path_metadata))
  outpack_schema("metadata")$validate(path_metadata)

  path_location <- file.path(path, ".outpack", "location", "local", id)
  expect_true(file.exists(path_location))
  outpack_schema("location")$validate(path_location)

  meta <- outpack_metadata_load(path_metadata)

  ## The index metadata is a subset of the full set:
  expect_mapequal(index$metadata[[id]],
                  meta[c("name", "id", "parameters", "files", "depends")])

  expect_setequal(
    names(meta),
    c("schema_version", "name", "id", "time", "parameters", "files",
      "depends", "custom", "git"))

  expect_equal(meta$schema_version, outpack_schema_version())
  expect_equal(meta$name, "example")
  expect_equal(meta$id, id)
  expect_null(meta$parameters)
  expect_equal(meta$depends, data_frame(packet = character(),
                                        query = character(),
                                        files = I(list())))
  expect_setequal(meta$files$path,
                  c("data.csv", "myplot.png", "script.R", "log.json"))
  expect_equal(meta$files$size,
               file.size(file.path(path_src, meta$files$path)))
  expect_equal(meta$files$hash,
               hash_files(file.path(path_src, meta$files$path)))
  expect_null(meta$custom)
  expect_null(meta$git)

  ## Copy of the files in human readable archive:
  expect_true(all(file.exists(
    file.path(path, "archive", "example", id, meta$files$path))))
  expect_equal(
    hash_files(file.path(path, "archive", "example", id, meta$files$path)),
    meta$files$hash)

  ## Copy of the files in the file store:
  expect_setequal(root$files$list(), meta$files$hash)

  expect_equal(index$unpacked, id)

  ## Easily retrieve metadata from root:
  expect_equal(root$metadata(id), index$metadata[[id]])
})


test_that("Can handle dependencies", {
  ## A simple example where we run something.
  path_src1 <- temp_file()
  fs::dir_create(path_src1)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src1, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path_src1, "data.csv"),
            row.names = FALSE)

  path_src2 <- temp_file()
  fs::dir_create(path_src2)
  writeLines(c(
    "d <- read.csv('incoming.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src2, "script.R"))

  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path <- root$path

  p1 <- outpack_packet_start(path_src1, "a", root = root)
  id1 <- p1$id
  outpack_packet_run(p1, "script.R")
  outpack_packet_end(p1)

  p2 <- outpack_packet_start(path_src2, "b", root = root)
  id2 <- p2$id
  outpack_packet_use_dependency(p2, id1, c("incoming.csv" = "data.csv"))
  outpack_packet_run(p2, "script.R")
  outpack_packet_end(p2)

  meta <- orderly_metadata(id2, root = path)
  path_metadata <- file.path(path, ".outpack", "metadata", id2)
  expect_true(file.exists(path_metadata))
  outpack_schema("metadata")$validate(path_metadata)

  expect_equal(
    meta$depends,
    data_frame(
      packet = id1,
      query = sprintf('single(id == "%s")', id1),
      files = I(list(data_frame(here = "incoming.csv", there = "data.csv")))))
})


test_that("Can't add a packet twice", {
  ## A simple example where we run something.
  path_src <- temp_file()
  fs::dir_create(path_src)

  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path <- root$path

  p <- outpack_packet_start(path_src, "example", root = root)
  outpack_packet_end(p)

  id <- p$id
  json <- read_string(file.path(path, ".outpack", "metadata", id))
  class(json) <- "json"
  expect_error(
    outpack_insert_packet(path_src, json, root),
    "'.+' has already been added for 'local'")
})


test_that("Can't use nonexistant id as dependency", {
  path_src <- temp_file()
  fs::dir_create(path_src)

  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path <- root$path

  p1 <- outpack_packet_start(path_src, "example", root = root)
  outpack_packet_end(p1)

  p2 <- outpack_packet_start(path_src, "example", root = root)
  expect_error(
    outpack_packet_use_dependency(p2, p1$id, c("a" = "b")),
    sprintf("Packet '%s' does not contain path 'b'", p1$id))
  outpack_packet_cancel(p2)
})


test_that("Can't use file that does not exist from dependency", {
  path_src1 <- temp_file()
  fs::dir_create(path_src1)

  path_src2 <- temp_file()
  fs::dir_create(path_src2)

  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path <- root$path

  p1 <- outpack_packet_start(path_src1, "a", root = root)
  outpack_packet_end(p1)

  p2 <- outpack_packet_start(path_src2, "b", root = root)
  expect_error(
    outpack_packet_use_dependency(p2, p1$id, c("incoming.csv" = "data.csv")),
    "Packet '.+' does not contain path 'data.csv'")
})


test_that("Can use dependency from outpack without file store", {
  path_src1 <- temp_file()
  fs::dir_create(path_src1)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src1, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path_src1, "data.csv"),
            row.names = FALSE)

  path_src2 <- temp_file()
  fs::dir_create(path_src2)
  writeLines(c(
    "d <- read.csv('incoming.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src2, "script.R"))

  root <- create_temporary_root(path_archive = "archive",
                                use_file_store = FALSE)
  path <- root$path

  p1 <- outpack_packet_start(path_src1, "a", root = root)
  id1 <- p1$id
  outpack_packet_run(p1, "script.R")
  outpack_packet_end(p1)

  p2 <- outpack_packet_start(path_src2, "b", root = root)
  id2 <- p2$id
  outpack_packet_use_dependency(p2, id1, c("incoming.csv" = "data.csv"))
  outpack_packet_run(p2, "script.R")
  outpack_packet_end(p2)

  meta <- orderly_metadata(id2, root = path)
  expect_equal(
    meta$depends,
    data_frame(
      packet = id1,
      query = sprintf('single(id == "%s")', id1),
      files = I(list(data_frame(here = "incoming.csv", there = "data.csv")))))
})


test_that("validate dependencies from archive", {
  path_src1 <- temp_file()
  fs::dir_create(path_src1)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src1, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path_src1, "data.csv"),
            row.names = FALSE)

  path_src2 <- temp_file()
  fs::dir_create(path_src2)
  writeLines(c(
    "d <- read.csv('incoming.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src2, "script.R"))

  root <- create_temporary_root(path_archive = "archive",
                                use_file_store = FALSE)
  path <- root$path

  p1 <- outpack_packet_start(path_src1, "a", root = root)
  id1 <- p1$id
  outpack_packet_run(p1, "script.R")
  outpack_packet_end(p1)

  ## Change the value here:
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(root$path, "archive", "a", id1, "data.csv"),
            row.names = FALSE)

  p2 <- outpack_packet_start(path_src2, "b", root = root)
  id2 <- p2$id
  expect_error(
    outpack_packet_use_dependency(p2, id1, c("incoming.csv" = "data.csv")),
    "Hash of '.+' does not match")
})


test_that("Can add additional data", {
  tmp <- temp_file()

  root <- create_temporary_root()

  src <- fs::dir_create(file.path(tmp, "src"))
  saveRDS(runif(10), file.path(src, "data.rds"))
  p <- outpack_packet_start(src, "example", root = root)
  custom <- '{"a": 1, "b": 2}'
  outpack_packet_add_custom(p, "potato", custom)
  outpack_packet_end(p)

  meta <- orderly_metadata(p$id, root = root)
  expect_equal(meta$custom, list(potato = list(a = 1, b = 2)))
})


test_that("Can add multiple copies of extra data", {
  tmp <- temp_file()

  root <- create_temporary_root()

  src <- fs::dir_create(file.path(tmp, "src"))
  saveRDS(runif(10), file.path(src, "data.rds"))
  p <- outpack_packet_start(src, "example", root = root)
  outpack_packet_add_custom(p, "app1", '{"a": 1, "b": 2}')
  outpack_packet_add_custom(p, "app2", '{"c": [1, 2, 3]}')
  outpack_packet_end(p)

  path_metadata <- file.path(root$path, ".outpack", "metadata", p$id)
  meta <- outpack_metadata_load(path_metadata)
  expect_equal(meta$custom,
               list(app1 = list(a = 1, b = 2),
                    app2 = list(c = list(1, 2, 3))))
})


test_that("Can't add custom data for same app twice", {
  tmp <- temp_file()

  root <- create_temporary_root()

  src <- fs::dir_create(file.path(tmp, "src"))
  saveRDS(runif(10), file.path(src, "data.rds"))
  p <- outpack_packet_start(src, "example", root = root)
  outpack_packet_add_custom(p, "app1", '{"a": 1, "b": 2}')
  outpack_packet_add_custom(p, "app2", '{"a": 1, "b": 2}')
  expect_error(
    outpack_packet_add_custom(p, "app1", '{"c": [1, 2, 3]}'),
    "metadata for 'app1' has already been added for this packet")
  expect_error(
    outpack_packet_add_custom(p, "app2", '{"c": [1, 2, 3]}'),
    "metadata for 'app2' has already been added for this packet")
})


test_that("Can validate custom metadata against schema", {
  schema <- '{
    "type": "object",
    "properties": {"a": { "type": "string" }, "b": { "type": "number" }}}'

  tmp <- temp_file()

  root <- create_temporary_root()

  src <- fs::dir_create(file.path(tmp, "src"))
  saveRDS(runif(10), file.path(src, "data.rds"))
  p <- outpack_packet_start(src, "example", root = root)
  expect_error(
    outpack_packet_add_custom(p, "app1", '{"a": 1, "b": 2}', schema),
    "Validating custom metadata failed:")
  ## No error
  outpack_packet_add_custom(p, "app1", '{"a": "str", "b": 2}', schema)
  outpack_packet_end(p)

  path_metadata <- file.path(root$path, ".outpack", "metadata", p$id)
  meta <- outpack_metadata_load(path_metadata)
  expect_equal(meta$custom,
               list(app1 = list(a = "str", b = 2)))
})


test_that("Can report nicely about json syntax errors", {
  tmp <- temp_file()

  root <- create_temporary_root()

  src <- fs::dir_create(file.path(tmp, "src"))
  saveRDS(runif(10), file.path(src, "data.rds"))
  p <- outpack_packet_start(src, "example", root = root)
  expect_error(
    outpack_packet_add_custom(p, "app1", '{"a": 1, "b": 2'),
    "Syntax error in custom metadata:")
})


test_that("pre-prepared id can be used to start packet", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)

  id <- outpack_id()
  path_src <- temp_file()
  fs::dir_create(path_src)

  p <- outpack_packet_start(path_src, "example", id = id, root = root)
  expect_equal(p$id, id)

  outpack_packet_end(p)
  index <- root$index()
  expect_equal(names(index$metadata), id)
})


test_that("Can hash files on startup", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)

  path_src <- temp_file()
  fs::dir_create(path_src)

  path_src <- temp_file()
  fs::dir_create(path_src)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "png('zzz.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path_src, "data.csv"),
            row.names = FALSE)

  inputs <- c("data.csv", "script.R")

  p <- outpack_packet_start(path_src, "example", root = root)
  expect_equal(outpack_packet_file_list(p),
               data_frame(path = inputs, status = "unknown"))
  outpack_packet_file_mark(p, inputs, "immutable")
  expect_equal(outpack_packet_file_list(p),
               data_frame(path = inputs, status = "immutable"))
  outpack_packet_run(p, "script.R")
  expect_equal(outpack_packet_file_list(p),
               data_frame(path = c(inputs, "zzz.png"),
                          status = c("immutable", "immutable", "unknown")))
  outpack_packet_file_mark(p, "zzz.png", "immutable")
  expect_equal(outpack_packet_file_list(p),
               data_frame(path = c(inputs, "zzz.png"), status = "immutable"))
  outpack_packet_end(p)
})


test_that("Can detect changes to hashed files", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)

  path_src <- temp_file()
  fs::dir_create(path_src)

  path_src <- temp_file()
  fs::dir_create(path_src)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "file.create('data.csv')", # truncates file
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path_src, "data.csv"),
            row.names = FALSE)
  inputs <- c("script.R", "data.csv")
  p <- outpack_packet_start(path_src, "example", root = root)
  outpack_packet_file_mark(p, inputs, "immutable")
  outpack_packet_run(p, "script.R")
  expect_error(
    outpack_packet_end(p),
    "File was changed after being added: 'data.csv'")
})


test_that("Re-adding files triggers hash", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)

  path_src <- temp_file()
  fs::dir_create(path_src)
  write.csv(mtcars, file.path(path_src, "data.csv"))

  p <- outpack_packet_start(path_src, "example", root = root)
  outpack_packet_file_mark(p, "data.csv", "immutable")
  expect_silent(outpack_packet_file_mark(p, "data.csv", "immutable"))
  expect_length(p$files, 1)
  file.create(file.path(path_src, "data.csv"))
  expect_error(outpack_packet_file_mark(p, "data.csv", "immutable"),
               "File was changed after being added: 'data.csv'")
})


test_that("Can ignore files from the final packet", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path_src <- create_temporary_simple_src()

  inputs <- c("data.csv", "script.R")

  p <- outpack_packet_start(path_src, "example", root = root)
  expect_equal(outpack_packet_file_list(p),
               data_frame(path = inputs, status = "unknown"))
  outpack_packet_file_mark(p, "data.csv", "ignored")
  expect_equal(outpack_packet_file_list(p),
               data_frame(path = inputs, status = c("ignored", "unknown")))
  outpack_packet_run(p, "script.R")
  expect_equal(outpack_packet_file_list(p),
               data_frame(path = c(inputs, "zzz.png"),
                          status = c("ignored", "unknown", "unknown")))
  outpack_packet_end(p)

  meta <- root$metadata(p$id)
  expect_equal(meta$files$path, c("log.json", "script.R", "zzz.png"))
  expect_length(root$files$list(), 3)
  expect_setequal(dir(file.path(root$path, "archive", "example", p$id)),
                  c("log.json", "script.R", "zzz.png"))
  expect_setequal(dir(path_src),
                  c("data.csv", "log.json", "script.R", "zzz.png"))
})


test_that("Files cannot be immutable and ignored", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path_src <- create_temporary_simple_src()

  p <- outpack_packet_start(path_src, "example", root = root)
  outpack_packet_file_mark(p, "data.csv", "ignored")
  outpack_packet_file_mark(p, "script.R", "immutable")

  expect_error(
    outpack_packet_file_mark(p, "data.csv", "immutable"),
    "Cannot mark ignored files as immutable: 'data.csv'")
  expect_error(
    outpack_packet_file_mark(p, "script.R", "ignored"),
    "Cannot mark immutable files as ignored: 'script.R'")
})


test_that("Validate a packet is incomplete", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path_src <- create_temporary_simple_src()

  p <- outpack_packet_start(path_src, "example", root = root)
  outpack_packet_finish(p)
  expect_error(check_current_packet(p),
               "Packet '.+' is complete")
})


test_that("can mark subsets of files immutably without error", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)

  path_src <- temp_file()
  fs::dir_create(path_src)
  for (i in letters[1:6]) {
    writeLines(i, file.path(path_src, i))
  }
  hash <- withr::with_dir(path_src,
                          hash_files(letters[1:6], "sha256", named = TRUE))

  p <- outpack_packet_start(path_src, "example", root = root)
  outpack_packet_file_mark(p, c("a", "b", "c"), "immutable")
  expect_equal(p$files$immutable, hash[1:3])

  expect_silent(
    outpack_packet_file_mark(p, c("b", "c"), "immutable"))
  expect_equal(p$files$immutable, hash[1:3])

  expect_silent(
    outpack_packet_file_mark(p, c("b", "e", "f"), "immutable"))
  expect_equal(p$files$immutable, hash[c("a", "b", "c", "e", "f")])

  expect_silent(
    outpack_packet_file_mark(p, "d", "immutable"))
  expect_equal(p$files$immutable, hash[c("a", "b", "c", "e", "f", "d")])

  expect_silent(
    outpack_packet_file_mark(p, names(hash), "immutable"))
  expect_equal(p$files$immutable, hash[c("a", "b", "c", "e", "f", "d")])
})


test_that("can depend based on a simple query", {
  root <- create_temporary_root(path_archive = NULL, use_file_store = TRUE)

  src <- withr::local_tempdir()
  src1 <- file.path(src, "1")
  src2 <- file.path(src, "2")
  fs::dir_create(c(src1, src2))

  id <- list(a = character(), b = character())
  for (i in 1:3) {
    for (name in  c("a", "b")) {
      saveRDS(runif(10), file.path(src1, "data.rds"))
      p <- outpack_packet_start(src1, name, parameters = list(i = i),
                                root = root)
      outpack_packet_end(p)
      id[[name]] <- c(id[[name]], p$id)
    }
  }

  p <- outpack_packet_start(src2, "x", root = root)
  outpack_packet_use_dependency(p, "latest", c("1.rds" = "data.rds"))

  expect_mapequal(
    p$depends[[1]],
    list(packet = id$b[[3]],
         query = "latest()",
         files = data.frame(here = "1.rds", there = "data.rds")))

  query <- orderly_query("latest(parameter:i < 3)", name = "a")
  outpack_packet_use_dependency(p, query, c("2.rds" = "data.rds"))
  expect_mapequal(
    p$depends[[2]],
    list(packet = id$a[[2]],
         query = 'latest(parameter:i < 3 && name == "a")',
         files = data.frame(here = "2.rds", there = "data.rds")))
})


test_that("can depend based on a query with subqueries", {
  root <- create_temporary_root(path_archive = NULL, use_file_store = TRUE)

  src <- withr::local_tempdir()
  src_a <- file.path(src, "a")
  src_b <- file.path(src, "b")
  src_c <- file.path(src, "c")
  fs::dir_create(c(src_a, src_b, src_c))

  id <- list(a = character())
  for (i in 1:3) {
    saveRDS(runif(10), file.path(src_a, "data.rds"))
    p <- outpack_packet_start(src_a, "a", parameters = list(i = i), root = root)
    outpack_packet_end(p)
    id$a <- c(id$a, p$id)
  }

  p1 <- outpack_packet_start(src_b, "b", root = root)
  query1 <- orderly_query("latest(parameter:i < 3)", name = "a")
  outpack_packet_use_dependency(p1, query1, c("2.rds" = "data.rds"))
  outpack_packet_end(p1)
  id$b <- p1$id

  p2 <- outpack_packet_start(src_c, "c", root = root)
  query2 <- orderly_query("latest(usedby({B}))", name = "a",
                          subquery = list(B = id$b))
  outpack_packet_use_dependency(p2, query2, files = c("new.rds" = "data.rds"))
  outpack_packet_end(p2)
  expect_length(p2$depends, 1)
  expect_equal(p2$depends[[1]]$packet, id$a[[2]])
  expect_equal(p2$depends[[1]]$query,
               sprintf('latest(usedby({"%s"}) && name == "a")', id$b))
})


test_that("validate that dependencies must evaluate to a single id", {
  path_src1 <- withr::local_tempdir()
  path_src2 <- withr::local_tempdir()
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)

  p1 <- outpack_packet_start(path_src1, "a", parameters = list(x = 1),
                             root = root)
  saveRDS(runif(5), file.path(path_src1, "data.rds"))
  outpack_packet_end(p1)

  p2 <- outpack_packet_start(path_src2, "b", root = root)
  expect_error(
    outpack_packet_use_dependency(p2, "parameter:x == 1",
                                  c("incoming.rds" = "data.rds")),
    paste("The provided query is not guaranteed to return a single value:",
          "'parameter:x == 1' Did you forget latest(...)?"),
    fixed = TRUE)
})


test_that("error if dependency cannot be resolved", {
  root <- create_temporary_root()
  path_src <- temp_file()
  fs::dir_create(path_src)
  p <- outpack_packet_start(path_src, "example", root = root)
  expect_error(
    outpack_packet_use_dependency(p, quote(latest(name == "data")),
                                  c("data.rds" = "data.rds")),
    'Failed to find packet for query:\n    latest(name == "data")',
    fixed = TRUE)
})


test_that("can pull in dependency from specific location", {
  root <- list()
  ids <- list()
  root$a <- create_temporary_root(use_file_store = TRUE)
  for (name in c("x", "y", "z")) {
    root[[name]] <- create_temporary_root(use_file_store = TRUE)
    ids[[name]] <- vcapply(1:3, function(i) {
      create_random_packet(root[[name]], "data", list(p = i))
    })
    orderly_location_add(name, "path", list(path = root[[name]]$path),
                         root = root$a)
  }
  orderly_location_pull_metadata(root = root$a)
  for (id in ids$z) {
    orderly_location_pull_packet(id, root = root$a)
  }

  path_src <- temp_file()
  fs::dir_create(path_src)

  p <- outpack_packet_start(path_src, "example", root = root$a)
  query <- quote(latest(name == "data" && parameter:p > 2))
  options <- list(location = c("x", "y"), allow_remote = FALSE)
  expect_error(
    outpack_packet_use_dependency(p, query, c("data.rds" = "data.rds"),
                                  search_options = options),
    paste0("Failed to find packet for query:\n    ",
           'latest(name == "data" && parameter:p > 2)'),
    fixed = TRUE)

  for (id in ids$x) {
    orderly_location_pull_packet(id, root = root$a)
  }
  outpack_packet_use_dependency(p, query, c("data1.rds" = "data.rds"),
                                search_options = options)
  expect_equal(p$depends[[1]]$packet, ids$x[[3]])

  for (id in ids$y) {
    orderly_location_pull_packet(id, root = root$a)
  }
  outpack_packet_use_dependency(p, query, c("data2.rds" = "data.rds"),
                                search_options = options)
  expect_equal(p$depends[[2]]$packet, ids$y[[3]])
})


test_that("can pull in dependency when not found, if requested", {
  root <- list()
  ids <- list()
  root$a <- create_temporary_root(use_file_store = TRUE)
  root$b <- create_temporary_root(use_file_store = TRUE,
                                  require_complete_tree = TRUE)
  root$x <- create_temporary_root(use_file_store = TRUE)
  ids <- vcapply(1:3, function(i) {
    create_random_packet(root$x, "data", list(p = i))
  })
  orderly_location_add("x", "path", list(path = root$x$path),
                       root = root$a)
  orderly_location_add("x", "path", list(path = root$x$path),
                       root = root$b)

  path_src_a <- withr::local_tempdir()
  query <- quote(latest(name == "data" && parameter:p > 2))

  p_a <- outpack_packet_start(path_src_a, "example", root = root$a$path)
  expect_error(
    outpack_packet_use_dependency(p_a, query, c("data.rds" = "data.rds")),
    paste0("Failed to find packet for query:\n    ",
           'latest(name == "data" && parameter:p > 2)'),
    fixed = TRUE)

  expect_length(root$a$index()$metadata, 0)
  expect_equal(nrow(root$a$index()$location), 0)
  expect_equal(length(root$a$index()$unpacked), 0)

  outpack_packet_use_dependency(p_a, query, c("data.rds" = "data.rds"),
                                search_options = list(pull_metadata = TRUE,
                                                      allow_remote = TRUE))

  expect_length(root$a$index()$metadata, 3)
  expect_equal(nrow(root$a$index()$location), 3)
  expect_equal(root$a$index()$unpacked, character())
  expect_equal(p_a$depends[[1]]$packet, ids[[3]])

  path_src_b <- withr::local_tempdir()
  p_b <- outpack_packet_start(path_src_b, "example", root = root$b$path)
  outpack_packet_use_dependency(p_b, query, c("data.rds" = "data.rds"),
                                search_options = list(pull_metadata = TRUE,
                                                      allow_remote = TRUE))

  expect_length(root$b$index()$metadata, 3)
  expect_equal(nrow(root$b$index()$location), 4) # compare with above!
  expect_equal(root$b$index()$unpacked, ids[[3]])
  expect_equal(p_b$depends[[1]]$packet, ids[[3]])
})


test_that("can pull in directories", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path <- root$path

  path_src1 <- withr::local_tempdir()
  p1 <- outpack_packet_start(path_src1, "a", root = root)
  fs::dir_create(file.path(path_src1, "data"))
  for (i in letters[1:6]) {
    writeLines(i, file.path(path_src1, "data", i))
  }
  outpack_packet_end(p1)
  id <- p1$id

  dest <- withr::local_tempdir()
  orderly_copy_files(id, files = c(d = "data/"), dest = dest, root = root)
  expect_equal(dir(dest), "d")
  expect_equal(dir(file.path(dest, "d")), letters[1:6])

  path_src2 <- withr::local_tempdir()
  p2 <- outpack_packet_start(path_src2, "b", root = root)
  outpack_packet_use_dependency(p2, 'latest(name == "a")', c(d = "data/"))
  expect_equal(p2$depends[[1]]$files,
               data_frame(here = file.path("d", letters[1:6]),
                          there = file.path("data", letters[1:6])))
})


test_that("exporting directories reports on trailing slashes being missing", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path <- root$path

  path_src1 <- withr::local_tempdir()
  p1 <- outpack_packet_start(path_src1, "a", root = root)
  fs::dir_create(file.path(path_src1, "data"))
  for (i in letters[1:6]) {
    writeLines(i, file.path(path_src1, "data", i))
  }
  outpack_packet_end(p1)
  id <- p1$id

  err <- paste0("Packet '.+' does not contain path 'data'\n",
                "  Consider adding a trailing slash to 'data'")

  dest <- withr::local_tempdir()
  expect_error(
    orderly_copy_files(id, files = c(d = "data"), dest = dest, root = root),
    err)

  path_src2 <- withr::local_tempdir()
  p2 <- outpack_packet_start(path_src2, "b", root = root)
  expect_error(
    outpack_packet_use_dependency(p2, 'latest(name == "a")', c(d = "data")),
    err)
})


test_that("can overwrite dependencies", {
  root <- create_temporary_root()
  id <- create_random_packet(root, "data")
  path_src <- withr::local_tempdir()
  p <- outpack_packet_start(path_src, "next", root = root)
  file.create(file.path(path_src, "data.rds"))
  err <- expect_error(
    outpack_packet_use_dependency(p, id, c("data.rds" = "data.rds"),
                                  overwrite = FALSE))
  ## Default allows overwrite:
  expect_silent(
    outpack_packet_use_dependency(p, id, c("data.rds" = "data.rds")))
  expect_equal(
    hash_file(file.path(path_src, "data.rds")),
    hash_file(file.path(root$path, "archive", "data", id, "data.rds")))
})
