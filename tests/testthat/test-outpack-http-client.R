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
    function(r) r |> httr2::req_headers(Authorization = "Bearer yogi"))

  expect_mapequal(
    res,
    list(status = "success", errors = NULL, data = list(1, 2, 3)))

  mockery::expect_called(mock, 1)
  args <- mockery::mock_args(mock)[[1]]
  expect_equal(args[[1]]$url, "http://example.com/path")
  expect_equal(args[[1]]$headers,
               list(Authorization = "Bearer yogi"),
               ignore_attr = TRUE)
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

  cl <- outpack_http_client$new("http://example.com", NULL)
  err <- expect_error(cl$request("path"), "Resource not found")
  expect_s3_class(err, "outpack_http_client_error")
  expect_equal(err$code, 404)
  expect_equal(err$errors, list(list(error = "NOT_FOUND",
                                     detail = "Resource not found")))
})


test_that("handle plain text errors", {
  local_mock_response("Error", status = 503L, wrap = FALSE)

  cl <- outpack_http_client$new("http://example.com", NULL)
  err <- expect_error(cl$request("path"), "Service Unavailable")

  expect_s3_class(err, "outpack_http_client_error")
  expect_equal(err$code, 503)
  expect_null(err$errors)
})


test_that("can use the client to make requests", {
  skip_if_not_installed("mockery")

  mock <- local_mock_response(json_string("[1,2,3]"))

  cl <- outpack_http_client$new("http://example.com", NULL)
  res <- cl$request("/path")
  expect_mapequal(res,
                  list(status = "success", errors = NULL, data = list(1, 2, 3)))

  args <- mockery::mock_args(mock)[[1]]
  expect_equal(args[[1]]$url, "http://example.com/path")
})


test_that("can add auth details to the client", {
  auth <- list(url = "http://example.com/api/login",
               data = list(token = "mytoken"))

  cl <- outpack_http_client$new("http://example.com", auth)

  h <- list("Authorization" = "Bearer yogi")
  mock_login <- mockery::mock(h, cycle = TRUE)
  mockery::stub(cl$authorise, "http_client_login", mock_login)

  cl$authorise()

  mockery::expect_called(mock_login, 1)
  expect_equal(
    mockery::mock_args(mock_login)[[1]],
    list("http://example.com",
         list(enabled = TRUE, url = auth$url, data = auth$data)))
  expect_equal(cl$auth$header, h)

  ## Second call, does not log in
  cl$authorise()
  mockery::expect_called(mock_login, 1)

  ## Actually perform an api call now:
  mock_get <- local_mock_response(json_string("[1,2,3]"))
  res <- cl$request("/path")
  expect_mapequal(
    res,
    list(status = "success", errors = NULL, data = list(1, 2, 3)))

  mockery::expect_called(mock_get, 1)
  args <- mockery::mock_args(mock_get)[[1]]
  expect_equal(args[[1]]$url, "http://example.com/path")
  expect_equal(args[[1]]$headers,
               list(Authorization = "Bearer yogi"),
               ignore_attr = TRUE)
})


test_that("can send authentication request", {
  clear_auth_cache()
  withr::defer(clear_auth_cache())

  mock_post <- local_mock_response(
    to_json(list(token = jsonlite::unbox("mytoken"))),
    wrap = FALSE)

  auth <- list(
    url = "https://example.com/login",
    data = list(token = "98e02a382db6a3a18e9d2e02c698478b"))

  res <- evaluate_promise(
    http_client_login("foo", auth))

  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Logging in to foo")
  expect_match(res$messages[[2]], "Logged in successfully")
  expect_equal(res$result, list("Authorization" = paste("Bearer", "mytoken")))

  mockery::expect_called(mock_post, 1)
  args <- mockery::mock_args(mock_post)[[1]]
  expect_equal(args[[1]]$url, auth$url)
  expect_equal(args[[1]]$body$data, list(token = auth$data$token))
  expect_equal(args[[1]]$body$type, "json")

  ## And a second time, does not call mock_post again:
  expect_silent(
    res2 <- http_client_login("foo", auth))
  expect_equal(res2, res$result)
  mockery::expect_called(mock_post, 1)
})


test_that("can send authenticated request", {
  clear_auth_cache()
  withr::defer(clear_auth_cache())

  mock <- mockery::mock(
    mock_response(to_json(list(token = jsonlite::unbox("mytoken"))), wrap = FALSE),
    mock_response(json_string("[1,2,3]")))
  httr2::local_mocked_responses(function(req) mock(req))

  auth <- list(
    url = "https://example.com/api/login",
    data = list(token = "98e02a382db6a3a18e9d2e02c698478b"))

  cl <- outpack_http_client$new("http://example.com", auth)
  res <- evaluate_promise(cl$request("data"))

  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Logging in to http://example.com")
  expect_match(res$messages[[2]], "Logged in successfully")
  expect_mapequal(
    res$result,
    list(status = "success", errors = NULL, data = list(1, 2, 3)))

  mockery::expect_called(mock, 2)
  post_args <- mockery::mock_args(mock)[[1]]
  expect_equal(post_args[[1]]$url, auth$url)
  expect_equal(post_args[[1]]$body$data, list(token = auth$data$token))
  expect_equal(post_args[[1]]$body$type, "json")

  get_args <- mockery::mock_args(mock)[[2]]
  expect_equal(get_args[[1]]$url, "http://example.com/data")
  expect_equal(get_args[[1]]$headers,
               list(Authorization = paste("Bearer mytoken")),
               ignore_attr = TRUE)
})
