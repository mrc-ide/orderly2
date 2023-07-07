test_that("can alert to device stack imbalance", {
  skip_if_not_installed("mockery")

  mock_dev_list <- mockery::mock(c(1L, 2L), cycle = TRUE)
  mock_dev_off <- mockery::mock()
  mockery::stub(check_device_stack, "grDevices::dev.list", mock_dev_list)
  mockery::stub(check_device_stack, "grDevices::dev.off", mock_dev_off)

  ## correct balance
  expect_equal(check_device_stack(2),
               list(success = TRUE, message = NULL))
  mockery::expect_called(mock_dev_list, 1)
  mockery::expect_called(mock_dev_off, 0)

  ## Too many open
  expect_equal(check_device_stack(0),
               list(success = FALSE, message = "Script left 2 devices open"))
  mockery::expect_called(mock_dev_list, 2)
  mockery::expect_called(mock_dev_off, 2)

  ## Too many closed
  expect_equal(check_device_stack(4),
               list(success = FALSE,
                    message = "Script closed 2 more devices than it opened!"))
  mockery::expect_called(mock_dev_list, 3)
  mockery::expect_called(mock_dev_off, 2)
})


test_that("can alert to sink stack imbalance", {
  skip_if_not_installed("mockery")

  mock_sink_number <- mockery::mock(2L, cycle = TRUE)
  mock_sink <- mockery::mock()
  mockery::stub(check_sink_stack, "sink.number", mock_sink_number)
  mockery::stub(check_sink_stack, "sink", mock_sink)

  ## correct balance
  expect_equal(check_sink_stack(2),
               list(success = TRUE, message = NULL))
  mockery::expect_called(mock_sink_number, 1)
  mockery::expect_called(mock_sink, 0)

  ## Too many open
  expect_equal(check_sink_stack(0),
               list(success = FALSE,
                    message = "Script left 2 sinks open"))
  mockery::expect_called(mock_sink_number, 2)
  mockery::expect_called(mock_sink, 2)

  ## Too many closed
  expect_equal(check_sink_stack(4),
               list(success = FALSE,
                    message = "Script closed 2 more sinks than it opened!"))
  mockery::expect_called(mock_sink_number, 3)
  mockery::expect_called(mock_sink, 2)
})
