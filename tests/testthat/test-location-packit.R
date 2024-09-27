test_that("can authenticate with existing GitHub token", {
  clear_auth_cache()
  withr::defer(clear_auth_cache())

  token <- "ghp_github-token"

  mock_post <- local_mock_response(
    to_json(list(token = jsonlite::unbox("my-packit-token"))),
    wrap = FALSE)

  res <- evaluate_promise(packit_authorisation("http://example.com/", token))

  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Logging in to http://example.com")
  expect_match(res$messages[[2]], "Logged in successfully")
  expect_equal(res$result,
               list("Authorization" = paste("Bearer", "my-packit-token")))

  mockery::expect_called(mock_post, 1)
  args <- mockery::mock_args(mock_post)[[1]]
  expect_equal(args[[1]]$url, "http://example.com/packit/api/auth/login/api")
  expect_equal(args[[1]]$body$data, list(token = scalar("ghp_github-token")))
  expect_equal(args[[1]]$body$type, "json")

  ## And a second time, does not call mock_post again:
  res2 <- expect_silent(
    packit_authorisation("http://example.com/", token))
  expect_equal(res2, res$result)
  mockery::expect_called(mock_post, 1)
})


test_that("can authenticate with existing Packit token", {
  clear_auth_cache()
  withr::defer(clear_auth_cache())

  token <- "my-packit-token"

  result <- packit_authorisation("http://example.com/", token)
  expect_equal(result,
               list("Authorization" = paste("Bearer", "my-packit-token")))
})


test_that("can authenticate using device flow", {
  clear_auth_cache()
  withr::defer(clear_auth_cache())

  mock_post <- local_mock_response(
    to_json(list(token = jsonlite::unbox("my-packit-token"))),
    wrap = FALSE)

  mockery::stub(packit_authorisation, "do_oauth_device_flow",
                "ghp_github-token")

  res <- evaluate_promise(packit_authorisation("http://example.com/",
                                               token = NULL,
                                               save_token = TRUE))

  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Logging in to http://example.com")
  expect_match(res$messages[[2]], "Logged in successfully")
  expect_equal(res$result,
               list("Authorization" = paste("Bearer", "my-packit-token")))

  mockery::expect_called(mock_post, 1)
  args <- mockery::mock_args(mock_post)[[1]]
  expect_equal(args[[1]]$url, "http://example.com/packit/api/auth/login/api")
  expect_equal(args[[1]]$body$data, list(token = scalar("ghp_github-token")))
  expect_equal(args[[1]]$body$type, "json")
})

test_that("location_packit uses authentication", {
  clear_auth_cache()
  withr::defer(clear_auth_cache())

  token <- "ghp_github-token"
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
  expect_equal(args[[1]]$body$data, list(token = scalar("ghp_github-token")))
  expect_equal(args[[1]]$body$type, "json")

  args <- mockery::mock_args(mock)[[2]]
  expect_match(args[[1]]$url,
               "http://example.com/packit/api/outpack/metadata/.*/text")
  expect_equal(args[[1]]$headers,
               list(Authorization = "Bearer my-packit-token"),
               ignore_attr = TRUE)

  mock <- mockery::mock(mock_login, mock_get)
})


test_that("Can configure oauth caching behaviour", {
  clear_auth_cache()
  withr::defer(clear_auth_cache())

  mock_token <- mockery::mock("token", cycle = TRUE)
  testthat::local_mocked_bindings(do_oauth_device_flow = mock_token)

  local_mock_response(to_json(list(token = jsonlite::unbox("my-packit-token"))),
                      cycle = TRUE)

  location <- orderly_location_packit("http://example.com", save_token = TRUE)
  suppressMessages(location$client$authorise())

  clear_auth_cache()

  location <- orderly_location_packit("http://example.com", save_token = FALSE)
  suppressMessages(location$client$authorise())

  mockery::expect_called(mock_token, 2)
  args <- mockery::mock_args(mock_token)
  expect_equal(args[[1]]$cache_disk, TRUE)
  expect_equal(args[[2]]$cache_disk, FALSE)
})


test_that("can create a packit location using an environment variable token", {
  loc <- withr::with_envvar(
    c("GITHUB_TOKEN" = "ghp_abc123"),
    orderly_location_packit("http://example.com", "$GITHUB_TOKEN"))

  mock_login <- local_mock_response(
    to_json(list(token = jsonlite::unbox("my-packit-token"))),
    wrap = FALSE)

  evaluate_promise(loc$client$authorise())

  mockery::expect_called(mock_login, 1)

  args <- mockery::mock_args(mock_login)[[1]]
  expect_equal(args[[1]]$url, "http://example.com/packit/api/auth/login/api")
  expect_equal(args[[1]]$body$data, list(token = scalar("ghp_abc123")))
  expect_equal(args[[1]]$body$type, "json")
})


test_that("error if token variable not found", {
  withr::with_envvar(
    c("PACKIT_TOKEN" = NA_character_),
    expect_error(
      orderly_location_packit("https://example.com", "$PACKIT_TOKEN"),
      "Environment variable 'PACKIT_TOKEN' was not set"))
})
