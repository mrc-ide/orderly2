test_that("Can construct metadata with parameters", {
  id <- outpack_id()
  name <- "example"
  time <- list(start = Sys.time() - 1, end = Sys.time())
  parameters <- list(a = 1, b = "two")
  path <- "."
  json <- outpack_metadata_create(path, name, id, time,
                                  parameters = parameters,
                                  files = character(),
                                  depends = NULL,
                                  custom = NULL,
                                  file_hash = NULL,
                                  file_ignore = NULL)
  d <- outpack_metadata_load(json)
  expect_equal(d$parameters, parameters)
})


test_that("Validate parameters", {
  expect_error(
    validate_parameters(list(a = 1, a = 1)),
    "'parameters' must have unique names")
  expect_error(
    validate_parameters(list(1, 1)),
    "'parameters' must be named")
  expect_error(
    validate_parameters(list(a = 1, b = 2:3)),
    "All parameters must be scalar atomics: error for 'b'")
  expect_error(
    validate_parameters(list(a = new.env(), b = 2:3)),
    "All parameters must be scalar atomics: error for 'a', 'b'")
  expect_error(
    validate_parameters(list(a = new.env(), b = 2:3, c = NA)),
    "All parameters must be scalar atomics: error for 'a', 'b', 'c'")
})


test_that("Validate hashes", {
  found <- c(a = "x", b = "y", c = "z")
  expect_silent(validate_hashes(found, found))
  expect_silent(validate_hashes(found, found[1]))
  expect_silent(validate_hashes(found, character()))

  expect_error(validate_hashes(found[1:2], found),
               "File was deleted after being added: 'c'")
  expect_error(validate_hashes(found[1], found),
               "File was deleted after being added: 'b', 'c'")
  expect_error(validate_hashes(character(), found),
               "File was deleted after being added: 'a', 'b', 'c'")

  expect_error(validate_hashes(c(a = "x", b = "y", c = "Z"), found),
               "File was changed after being added: 'c'")
  expect_error(validate_hashes(c(a = "X", b = "y", c = "Z"), found),
               "File was changed after being added: 'a', 'c'")
})


test_that("reading metadata via top-level function is same as from root", {
  root <- create_temporary_root(use_file_store = TRUE)
  id <- create_random_packet(root)
  meta <- root$metadata(id, TRUE)
  expect_identical(
    outpack_metadata_read(file.path(root$path, ".outpack", "metadata", id)),
    meta)
  expect_identical(outpack_metadata(id, root), meta)
})


test_that("Sensible error if metadata file not found", {
  expect_error(
    outpack_metadata_read(tempfile()),
    "File does not exist: ")
})


test_that("Sensible error if metadata file not found", {
  root <- create_temporary_root(use_file_store = TRUE)
  expect_error(
    outpack_metadata(1, root),
    "'id' must be character")
  expect_error(
    outpack_metadata(letters, root),
    "'id' must be a scalar")
  expect_error(
    outpack_metadata("some-id", root),
    "Malformed id 'some-id'")
  expect_error(
    outpack_metadata(outpack_id(), root),
    "id '.+' not found in index")
})
