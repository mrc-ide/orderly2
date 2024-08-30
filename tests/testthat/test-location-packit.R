test_that("can authenticate with existing token", {
  clear_auth_cache()
  withr::defer(clear_auth_cache())

  token = "my-github-token"

  mock_post <- local_mock_response(
    to_json(list(token = jsonlite::unbox("my-packit-token"))),
    wrap = FALSE)

  res <- evaluate_promise(
    packit_authorisation("http://example.com/", token))

  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Logging in to http://example.com")
  expect_match(res$messages[[2]], "Logged in successfully")
  expect_equal(res$result,
               list("Authorization" = paste("Bearer", "my-packit-token")))

  mockery::expect_called(mock_post, 1)
  args <- mockery::mock_args(mock_post)[[1]]
  expect_equal(args[[1]]$url, "http://example.com/packit/api/auth/login/api")
  expect_equal(args[[1]]$body$data, list(token = scalar("my-github-token")))
  expect_equal(args[[1]]$body$type, "json")
 
  ## And a second time, does not call mock_post again:
  res2 <- expect_silent(
    packit_authorisation("http://example.com/", token))
  expect_equal(res2, res$result)
  mockery::expect_called(mock_post, 1)
})


test_that("can authenticate using device flow", {
  clear_auth_cache()
  withr::defer(clear_auth_cache())

  mock_post <- local_mock_response(
    to_json(list(token = jsonlite::unbox("my-packit-token"))),
    wrap = FALSE)

  mockery::stub(packit_authorisation, "do_oauth_device_flow", "my-github-token")

  res <- evaluate_promise(packit_authorisation("http://example.com/",
                                               token=NULL))

  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Logging in to http://example.com")
  expect_match(res$messages[[2]], "Logged in successfully")
  expect_equal(res$result,
               list("Authorization" = paste("Bearer", "my-packit-token")))

  mockery::expect_called(mock_post, 1)
  args <- mockery::mock_args(mock_post)[[1]]
  expect_equal(args[[1]]$url, "http://example.com/packit/api/auth/login/api")
  expect_equal(args[[1]]$body$data, list(token = scalar("my-github-token")))
  expect_equal(args[[1]]$body$type, "json")
})

test_that("location_packit uses authentication", {
  clear_auth_cache()
  withr::defer(clear_auth_cache())

  token = "my-github-token"
  id <- outpack_id()
  metadata <- "packet metadata"

  mock_login <- mock_response(
    to_json(list(token = jsonlite::unbox("my-packit-token"))),
    wrap = FALSE)
  mock_get <- mock_response(metadata)
  mock <- mockery::mock(mock_login, mock_get)
  httr2::local_mocked_responses(function(req) mock(req))

  location <- orderly_location_packit("http://example.com", token)
  res <- evaluate_promise(location$metadata(id))

  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Logging in to http://example.com")
  expect_match(res$messages[[2]], "Logged in successfully")
  expect_equal(res$result, setNames(metadata, id))

  mockery::expect_called(mock, 2)

  args <- mockery::mock_args(mock)[[1]]
  expect_equal(args[[1]]$url, "http://example.com/packit/api/auth/login/api")
  expect_equal(args[[1]]$body$data, list(token = scalar("my-github-token")))
  expect_equal(args[[1]]$body$type, "json")

  args <- mockery::mock_args(mock)[[2]]
  expect_match(args[[1]]$url,
               "http://example.com/packit/api/outpack/metadata/.*/text")
  expect_equal(args[[1]]$headers, 
               list(Authorization = "Bearer my-packit-token"),
               ignore_attr = TRUE)

  mock <- mockery::mock(mock_login, mock_get)
})

test_that("can create a packit location using an environment variable token", {
  loc <- withr::with_envvar(
    c("PACKIT_TOKEN" = "abc123"),
    orderly_location_packit("http://example.com", "$PACKIT_TOKEN"))

  mock_login <- local_mock_response(
    to_json(list(token = jsonlite::unbox("my-packit-token"))),
    wrap = FALSE)

  client <- loc$.__enclos_env__$private$client
  evaluate_promise(client$authorise())

  mockery::expect_called(mock_login, 1)

  args <- mockery::mock_args(mock_login)[[1]]
  expect_equal(args[[1]]$url, "http://example.com/packit/api/auth/login/api")
  expect_equal(args[[1]]$body$data, list(token = scalar("abc123")))
  expect_equal(args[[1]]$body$type, "json")
})


test_that("error of token variable not found", {
  withr::with_envvar(
    c("PACKIT_TOKEN" = NA_character_),
    expect_error(
      orderly_location_packit("https://example.com", "$PACKIT_TOKEN"),
      "Environment variable 'PACKIT_TOKEN' was not set"))
})
