test_that("Can hash file", {
  tmp <- temp_file()

  writeLines("hello world", tmp)
  expected <- unname(tools::md5sum(tmp))
  expect_equal(hash_file(tmp, "md5"), paste0("md5:", expected))
  expect_equal(hash_parse(hash_file(tmp, "md5")),
               list(algorithm = "md5", value = expected))

  saveRDS(mtcars, tmp)
  expected <- unname(tools::md5sum(tmp))
  expect_equal(hash_file(tmp, "md5"), paste0("md5:", expected))
})


test_that("can validate hash", {
  tmp <- temp_file()
  data <- "hello world"
  writeLines(data, tmp)
  expected_file <- hash_file(tmp, "md5")
  expected_data <- hash_data(data, "md5")

  expect_silent(hash_validate_data(data, expected_data))
  expect_mapequal(
    withVisible(hash_validate_data(data, expected_data)),
    list(visible = FALSE, value = expected_data))

  expect_silent(hash_validate_file(tmp, expected_file))
  expect_mapequal(
    withVisible(hash_validate_file(tmp, expected_file)),
    list(visible = FALSE, value = expected_file))

  expect_error(
    withr::with_dir(dirname(tmp),
                    hash_validate_file(basename(tmp), expected_data)),
    sprintf("Hash of '%s' does not match", basename(tmp)))
  err <- expect_error(
    hash_validate_data(data, expected_file, "thing"),
    "Hash of thing does not match!")
  expect_equal(names(err$body), c("x", "i"))
  expect_match(err$body[[1]], sprintf("%s.+found", expected_data))
  expect_match(err$body[[2]], sprintf("%s.+want", expected_file))
})


test_that("can use user-facing hash functions", {
  root <- test_prepare_orderly_example("explicit")
  path <- file.path(root, "src", "explicit")
  str <- "hello"
  tmp <- withr::local_tempfile()
  writeLines(str, tmp)

  expect_equal(orderly_hash_data(str, "md5"), hash_data(str, "md5"))
  expect_equal(orderly_hash_data(str, root = root), hash_data(str, "sha256"))
  expect_equal(orderly_hash_data(str, root = path), hash_data(str, "sha256"))
  expect_equal(withr::with_dir(path, orderly_hash_data(str)),
               hash_data(str, "sha256"))

  expect_equal(orderly_hash_file(tmp, "md5"), hash_file(tmp, "md5"))
  expect_equal(orderly_hash_file(tmp, root = root), hash_file(tmp, "sha256"))
  expect_equal(orderly_hash_file(tmp, root = path), hash_file(tmp, "sha256"))
  expect_equal(withr::with_dir(path, orderly_hash_file(tmp)),
               hash_file(tmp, "sha256"))
})
