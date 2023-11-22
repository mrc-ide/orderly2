test_that("reading metadata reports progress if requested", {
  root <- create_temporary_root()
  ids <- create_random_packet_chain(5, root = root)

  mock_progress_bar <- mockery::mock()
  mock_progress_update <- mockery::mock()
  mockery::stub(read_metadata, "cli::cli_progress_bar", mock_progress_bar)
  mockery::stub(read_metadata, "cli::cli_progress_update", mock_progress_update)

  read_metadata(root$path, NULL, FALSE)
  mockery::expect_called(mock_progress_bar, 0)
  mockery::expect_called(mock_progress_update, 0)

  read_metadata(root$path, NULL, TRUE)
  mockery::expect_called(mock_progress_bar, 1)
  mockery::expect_called(mock_progress_update, 5)
})


test_that("reading location reports progress if requested", {
  root <- create_temporary_root()
  ids <- create_random_packet_chain(5, root = root)

  mock_progress_bar <- mockery::mock()
  mock_progress_update <- mockery::mock()
  mockery::stub(read_location, "cli::cli_progress_bar", mock_progress_bar)
  mockery::stub(read_location, "cli::cli_progress_update", mock_progress_update)

  read_location("local", root$path, NULL, FALSE)
  mockery::expect_called(mock_progress_bar, 0)
  mockery::expect_called(mock_progress_update, 0)

  read_location("local", root$path, NULL, TRUE)
  mockery::expect_called(mock_progress_bar, 1)
  mockery::expect_called(mock_progress_update, 5)
})
