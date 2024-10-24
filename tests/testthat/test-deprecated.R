test_that("can use deprecated pull_metadata with warning", {
  withr::local_options(orderly.quiet = FALSE)
  rlang::reset_warning_verbosity(
    "orderly_deprecate:orderly_location_pull_metadata")
  root <- create_temporary_root()
  res <- evaluate_promise(orderly_location_pull_metadata(root = root))
  expect_match(res$warnings,
               "'orderly_location_pull_metadata()' is deprecated",
               fixed = TRUE)
  expect_match(res$messages, "Fetching metadata from 0 locations")
})


test_that("can use deprecated pull_packet with warning", {
  rlang::reset_warning_verbosity(
    "orderly_deprecate:orderly_location_pull_packet")

  root <- list()
  for (name in c("src", "dst")) {
    root[[name]] <- create_temporary_root(use_file_store = TRUE)
  }

  id <- create_random_packet(root$src)
  orderly_location_add_path("src", path = root$src$path, root = root$dst)
  orderly_location_fetch_metadata(root = root$dst)
  res <- evaluate_promise(orderly_location_pull_packet(id, root = root$dst))
  expect_equal(res$result, id)
  expect_match(res$warnings,
               "'orderly_location_pull_packet()' is deprecated",
               fixed = TRUE)
})
