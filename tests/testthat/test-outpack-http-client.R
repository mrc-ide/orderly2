test_that("client sends well formed requests", {
  skip_if_not_installed("mockery")

  mock <- local_mock_response(json_string("[1,2,3]"))

  res <- http_client_request("http://example.com/path")
  expect_mapequal(
    res,
    list(status = "success", errors = NULL, data = list(1, 2, 3)))

  mockery::expect_called(mock, 1)
  args <- mockery::mock_args(mock)[[1]]
  expect_equal(args[[1]]$url, "http://example.com/path")
})


test_that("client can add headers", {
  skip_if_not_installed("mockery")

  mock <- local_mock_response(json_string("[1,2,3]"))

  res <- http_client_request(
    "http://example.com/path",
    function(r) httr2::req_headers(r, Authorization = "Bearer yogi"))

  expect_mapequal(
    res,
    list(status = "success", errors = NULL, data = list(1, 2, 3)))

  mockery::expect_called(mock, 1)
  args <- mockery::mock_args(mock)[[1]]
  expect_equal(args[[1]]$url, "http://example.com/path")
  headers <- args[[1]]$headers
  expect_equal(names(headers), "Authorization")
  expect_equal(rlang::wref_value(headers$Authorization), "Bearer yogi")
})


test_that("client can return json verbatim as text", {
  skip_if_not_installed("mockery")

  ## A little whitespace here to ensure that this has not gone through
  ## any json processor
  local_mock_response(json_string("[1,2, 3]"), wrap = FALSE)

  res <- http_client_request("http://example.com/path",
                             parse_json = FALSE)
  expect_equal(res, "[1,2, 3]")
})


test_that("client can download files", {
  skip_if_not_installed("mockery")
  content <- charToRaw("result")

  dest <- temp_file()

  mock <- local_mock_response(content, download = dest)
  res <- http_client_request("http://example.com/path", download = dest)

  expect_identical(res, dest)

  mockery::expect_called(mock, 1)
  args <- mockery::mock_args(mock)[[1]]

  expect_equal(args[[1]]$url, "http://example.com/path")
  expect_equal(args[[1]]$headers,
               list(Accept = "application/octet-stream"),
               ignore_attr = TRUE)
})


test_that("handle errors", {
  str <- paste0(
    '{"status":"failure",',
    '"errors":[{"error":"NOT_FOUND","detail":"Resource not found"}],',
    '"data":null}')

  local_mock_response(json_string(str), status = 404L, wrap = FALSE)

  err <- expect_error(http_client_request("http://example.com/path"),
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

  local_mock_response(json_string(str), status = 404L, wrap = FALSE)

  cl <- outpack_http_client$new("http://example.com")
  err <- expect_error(cl$request("path"), "Resource not found")
  expect_s3_class(err, "outpack_http_client_error")
  expect_equal(err$code, 404)
  expect_equal(err$errors, list(list(error = "NOT_FOUND",
                                     detail = "Resource not found")))
})


test_that("handle plain text errors", {
  local_mock_response("Error", status = 503L, wrap = FALSE)

  cl <- outpack_http_client$new("http://example.com")
  err <- expect_error(cl$request("path"), "Service Unavailable")

  expect_s3_class(err, "outpack_http_client_error")
  expect_equal(err$code, 503)
  expect_null(err$errors)
})


test_that("handle empty errors", {
  local_mock_response(NA, status = 503L, wrap = FALSE)

  cl <- outpack_http_client$new("http://example.com")
  err <- expect_error(cl$request("path"), "Service Unavailable")

  expect_s3_class(err, "outpack_http_client_error")
  expect_equal(err$code, 503)
  expect_null(err$errors)
})


test_that("can use the client to make requests", {
  skip_if_not_installed("mockery")

  mock <- local_mock_response(json_string("[1,2,3]"))

  cl <- outpack_http_client$new("http://example.com")
  res <- cl$request("/path")
  expect_mapequal(res,
                  list(status = "success", errors = NULL, data = list(1, 2, 3)))

  args <- mockery::mock_args(mock)[[1]]
  expect_equal(args[[1]]$url, "http://example.com/path")
})


test_that("can add customization to the client", {
  cl <- outpack_http_client$new("http://example.com", function(r) {
    httr2::req_auth_bearer_token(r, "yogi")
  })

  mock_get <- local_mock_response(json_string("[1,2,3]"))
  res <- cl$request("/path")
  expect_mapequal(
    res,
    list(status = "success", errors = NULL, data = list(1, 2, 3)))

  mockery::expect_called(mock_get, 1)

  args <- mockery::mock_args(mock_get)[[1]]
  expect_equal(args[[1]]$url, "http://example.com/path")
  headers <- args[[1]]$headers
  expect_equal(names(headers), "Authorization")
  expect_equal(rlang::wref_value(headers$Authorization), "Bearer yogi")
})
