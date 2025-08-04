# httr2's built-in mocking functionality operates at a too high level and
# doesn't receive the authentication details that we are interested in testing.
#
# As far as I can tell, the only way to do this properly is by running a full
# HTTP server and make requests to it. The webfakes package helps us in this.
#
# Unfortunately, as the server runs in a separate process, there is no easy way
# to write assertions against the contents of the request. Because of this, the
# server records metadata about requests and exposes the last request under
# `/last/:name`. The test process can hit that endpoint to see the contents of
# the request. Similary, tokens are managed using the `/token/:name` and
# `/count` endpoints.
packit_app <- function() {
  app <- webfakes::new_app()
  app$locals$requests <- list()
  app$locals$tokens <- list()
  app$locals$token_count <- 0

  # We can't use a simple `/instance/:name` pattern because of
  # https://github.com/r-lib/webfakes/issues/120
  app$get(
    webfakes::new_regexp("/instance/(?<name>[a-z]+)/packit/api/outpack/"),
    function(req, res) {
      # HTTP headers is actually all the tests care about. We could capture more
      # of the request if needed.
      req$app$locals$requests[[req$params$name]] <- list(
        headers = req$headers
      )
      res$send_json(list(status = "success"), auto_unbox = TRUE)
    })
  app$post("/instance/:name/packit/api/deviceAuth", function(req, res) {
    res$send_json(list(
      device_code = "xxx",
      user_code = "yyy",
      verification_uri = "zzz",
      expires_in = 3600,
      interval = 0
    ), auto_unbox = TRUE)
  })
  app$post("/instance/:name/packit/api/deviceAuth/token", function(req, res) {
    app$locals$token_count <- app$locals$token_count + 1
    res$send_json(
      list(
        access_token = res$app$locals$tokens[[req$params$name]],
        token_type = "bearer"),
      auto_unbox = TRUE)
  })
  app$get("/last/:name", function(req, res) {
    res$send_json(res$app$locals$requests[[req$params$name]], auto_unbox = TRUE)
  })
  app$post("/token/:name", function(req, res) {
    res$app$locals$tokens[[req$params$name]] <- req$query$value
    res$send_status(200)
  })
  app$get("/count", function(req, res) {
    res$send_json(app$locals$token_count, auto_unbox = TRUE)
  })
  app
}

# Setting up the app is a bit slow so we use a single instance of it
# across all tests in the file.  We need this to work where webfakes
# is not available so create a fake webfake (!) that skips tests on
# use in that case.
if (requireNamespace("webfakes", quietly = TRUE)) {
  packit <- webfakes::local_app_process(packit_app())
} else {
  packit <- list(
    url = function(...) {
      testthat::skip("webfakes is not installed")
    }
  )
}

packit_url <- function(name = "default") {
  packit$url(paste0("/instance/", name, "/"))
}

last_request <- function(name = "default") {
  httr2::request(packit$url()) |>
    httr2::req_url_path_append("last", name) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

# Configure the mock server to reply to an authentication attempt with the
# given token.
set_device_flow_token <- function(token, name = "default") {
  httr2::request(packit$url()) |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("token", name) |>
    httr2::req_url_query(value = token) |>
    httr2::req_perform()
}

# Get the number of tokens issued while the argument is evaluated.
#
# Because we use a single long-running app, we need to take the difference in
# count from before and after.
count_issued_tokens <- function(f) {
  req <- httr2::request(packit$url("/count"))

  before <- req |> httr2::req_perform() |> httr2::resp_body_json()
  force(f)
  after <- req |> httr2::req_perform() |> httr2::resp_body_json()

  after - before
}

# Set the httr2 on-disk cache to a temporary directory.
local_oauth_cache <- function(.local_envir = parent.frame()) {
  path <- withr::local_tempdir(.local_envir = .local_envir)
  withr::local_envvar("HTTR2_OAUTH_CACHE" = path, .local_envir = .local_envir)
}

send_packit_request <- function(...) {
  # The oauth process is a bit more chatty than what we want for tests.
  suppressMessages({
    # The fully-qualified is need to be able to use this with callr.
    orderly2:::orderly_location_packit(...)$verify()
  })
}

test_that("Cannot authenticate with GitHub token", {
  token <- "ghp_github-token"
  expect_error(
    orderly_location_packit("http://example.com", token),
    "Using a GitHub token to login to Packit isn't supported anymore.")
})

test_that("Can authenticate with an existing Packit token", {
  local_oauth_cache()
  token <- "my-packit-token"

  send_packit_request(packit_url(), token)

  expect_equal(last_request()$headers$Authorization, "Bearer my-packit-token")
})

test_that("Can create a packit location using an environment variable token", {
  withr::local_envvar(PACKIT_TOKEN = "packit-token-from-env")

  send_packit_request(packit_url(), "$PACKIT_TOKEN")

  req <- last_request()
  expect_equal(req$headers$Authorization, "Bearer packit-token-from-env")
})

test_that("Error if token variable not found", {
  withr::local_envvar(PACKIT_TOKEN = NA_character_)
  expect_error(
    orderly_location_packit(packit_url(), "$PACKIT_TOKEN"),
    "Environment variable 'PACKIT_TOKEN' was not set")
})

test_that("Can authenticate using device flow", {
  local_oauth_cache()
  set_device_flow_token("device-packit-token")

  send_packit_request(packit_url())

  expect_equal(last_request()$headers$Authorization,
               "Bearer device-packit-token")
})

test_that("Authentication is cached", {
  local_oauth_cache()
  set_device_flow_token("device-packit-token")

  n <- count_issued_tokens({
    send_packit_request(packit_url())
    send_packit_request(packit_url())
  })
  expect_equal(n, 1)
})

test_that("Authentication cache is keyed by server URL", {
  local_oauth_cache()

  set_device_flow_token(token = "token-foo", name = "foo")
  set_device_flow_token(token = "token-bar", name = "bar")

  n <- count_issued_tokens({
    send_packit_request(packit_url(name = "foo"))
    send_packit_request(packit_url(name = "bar"))
  })

  expect_equal(n, 2)
  expect_equal(last_request("foo")$headers$Authorization, "Bearer token-foo")
  expect_equal(last_request("bar")$headers$Authorization, "Bearer token-bar")
})

# This tests checks that the authentication cache is saved to disk and not just
# memory. It works using callr to start sub-processes that will each try to
# connect to Packit. The sub-processes are needed to make sure we aren't just
# hitting the in-memory cache.
test_that("Authentication cache persists across sessions", {
  local_oauth_cache()

  set_device_flow_token("first-token")
  callr::r(send_packit_request, args = list(url = packit_url()))
  expect_equal(last_request()$headers$Authorization, "Bearer first-token")

  set_device_flow_token("second-token")
  n <- count_issued_tokens({
    callr::r(send_packit_request, args = list(url = packit_url()))
  })
  # No authentication took place. The client still uses the first token it got.
  expect_equal(n, 0)
  expect_equal(last_request()$headers$Authorization, "Bearer first-token")
})

test_that("On-disk authentication cache can be disabled", {
  local_oauth_cache()

  set_device_flow_token("first-token")
  n <- count_issued_tokens({
    callr::r(function(f, ...) {
      f(..., save_token = FALSE)
      f(..., save_token = FALSE)
    }, args = list(f = send_packit_request, url = packit_url()))
  })

  # The in-memory cache is still effective. We only had one authentication
  # attempt, in spite of the two requests.
  expect_equal(n, 1)
  expect_equal(last_request()$headers$Authorization, "Bearer first-token")

  set_device_flow_token("second-token")
  n <- count_issued_tokens({
    callr::r(function(f, ...) {
      f(..., save_token = FALSE)
      f(..., save_token = FALSE)
    }, args = list(f = send_packit_request, url = packit_url()))
  })

  # Unlike the earlier test, the token is not reused across sessions and a new
  # one is obtained.
  expect_equal(n, 1)
  expect_equal(last_request()$headers$Authorization, "Bearer second-token")
})
