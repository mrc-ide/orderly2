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
  err <- expect_error(
    validate_parameters(list(a = 1, b = 2:3)),
    "Invalid parameter value\\b")
  expect_equal(
    err$body,
    c("x" = "Values must be scalar, but were not for:",
      "*" = "b"))
  err <- expect_error(
    validate_parameters(list(a = new.env(), b = 2:3)),
    "Invalid parameter values\\b")
  expect_equal(
    err$body,
    c("x" = "Values must be scalar, but were not for:",
      "*" = "a",
      "*" = "b",
      "x" = "Values must be character, numeric or boolean, but were not for:",
      "*" = "a"))
  err <- expect_error(
    validate_parameters(list(a = new.env(), b = 2:3, c = NA)),
    "Invalid parameter values\\b")
  expect_equal(
    err$body,
    c("x" = "Values must be scalar, but were not for:",
      "*" = "a",
      "*" = "b",
      "x" = "Values must be character, numeric or boolean, but were not for:",
      "*" = "a",
      "*" = "c"))
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


test_that("Sensible error if metadata file not found", {
  expect_error(
    orderly_metadata_read(tempfile()),
    "File does not exist: ")
})


test_that("Can read metadata", {
  root <- create_temporary_root()
  id <- create_random_packet(root)
  expect_equal(
    orderly_metadata_read(file.path(root$path, ".outpack", "metadata", id)),
    orderly_metadata(id, root))
})


test_that("Can't get nonexistant metadata", {
  root <- create_temporary_root(path_archive = NULL, use_file_store = TRUE)
  id <- outpack_id()
  expect_error(
    orderly_metadata(id, root),
    sprintf("Packet '%s' not found in outpack index", id))
})


test_that("Sensible error if metadata file not found", {
  root <- create_temporary_root(use_file_store = TRUE)
  expect_error(
    orderly_metadata(1, root),
    "'id' must be character")
  expect_error(
    orderly_metadata(letters, root),
    "'id' must be a scalar")
  expect_error(
    orderly_metadata("some-id", root),
    "Malformed id 'some-id'")
  expect_error(
    orderly_metadata(outpack_id(), root),
    "Packet '.+' not found in outpack index")
})
