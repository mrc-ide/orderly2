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
  expect_error(
    hash_validate_data(data, expected_file, "thing"),
    sprintf("Hash of thing does not match:\n - expected: %s\n - found:    %s",
            expected_file, expected_data))
})
