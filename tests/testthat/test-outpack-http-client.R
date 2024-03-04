test_that("client sends well formed requests", {
  skip_if_not_installed("mockery")
  verb <- mockery::mock(mock_response(json_string("[1,2,3]")))

  res <- http_client_request(verb, "http://example.com/path")
  expect_mapequal(
    res,
    list(status = "success", errors = NULL, data = list(1, 2, 3)))
  mockery::expect_called(verb, 1)
  expect_equal(mockery::mock_args(verb)[[1]],
               list("http://example.com/path"))
})


test_that("client can return json verbatim as text", {
  skip_if_not_installed("mockery")
  ## A little whitespace here to ensure that this has not gone through
  ## any json processor
  verb <- mockery::mock(mock_response(json_string("[1,2, 3]"), wrap = FALSE))

  res <- http_client_request(verb, "http://example.com/path",
                             parse_json = FALSE)
  expect_equal(res, "[1,2, 3]")
})


test_that("client can download files", {
  skip_if_not_installed("mockery")
  content <- charToRaw("result")
  dest <- temp_file()
  verb <- mockery::mock(mock_response(content, download = dest))

  mock_download_options <- mockery::mock(list(TRUE))
  mockery::stub(http_client_request, "http_client_download_options",
                mock_download_options)

  res <- http_client_request(verb, "http://example.com/path",
                             download = dest)

  expect_identical(res, dest)
  mockery::expect_called(verb, 1)
  args <- mockery::mock_args(verb)[[1]]
  expect_equal(args, list("http://example.com/path",
                          list(TRUE)))

  mockery::expect_called(mock_download_options, 1)
  expect_equal(mockery::mock_args(mock_download_options)[[1]], list(dest))
})


test_that("handle errors", {
  str <- paste0(
    '{"status":"failure",',
    '"errors":[{"error":"NOT_FOUND","detail":"Resource not found"}],',
    '"data":null}')
  r <-  mock_response(json_string(str), status = 404, wrap = FALSE)
  err <- expect_error(http_client_handle_error(r),
                      "Resource not found")
  expect_s3_class(err, "outpack_http_client_error")
  expect_equal(err$code, 404)
  expect_equal(err$errors, list(list(error = "NOT_FOUND",
                                     detail = "Resource not found")))
})


test_that("handle errors from packit", {
  str <- paste0(
    '{"status":"failure",',
    '"error":{"error":"NOT_FOUND","detail":"Resource not found"},',
    '"data":null}')
  r <-  mock_response(json_string(str), status = 404, wrap = FALSE)
  err <- expect_error(http_client_handle_error(r),
                      "Resource not found")
  expect_s3_class(err, "outpack_http_client_error")
  expect_equal(err$code, 404)
  expect_equal(err$errors, list(list(error = "NOT_FOUND",
                                     detail = "Resource not found")))
})


test_that("can construct sensible download options", {
  path <- temp_file()
  res <- http_client_download_options(path)
  expect_s3_class(res, "request")
  expect_equal(res$headers, c(Accept = "application/octet-stream"))
  expect_equal(res$output, httr::write_disk(path)$output)
})


test_that("can use the client to make requests", {
  skip_if_not_installed("mockery")
  cl <- outpack_http_client$new("http://example.com", NULL)
  mock_get <- mockery::mock(mock_response(json_string("[1,2,3]")))
  mockery::stub(cl$get, "httr::GET", mock_get)
  res <- cl$get("/path")
  expect_mapequal(
    res,
    list(status = "success", errors = NULL, data = list(1, 2, 3)))
  mockery::expect_called(mock_get, 1)
  expect_equal(mockery::mock_args(mock_get)[[1]],
               list("http://example.com/path", NULL))
})
