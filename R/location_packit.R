# This has the same semantics as httr2::req_url_path_append and is mostly just
# copied from there. It always adds the argument to the end of the URL, adding
# a slash only when necessary.
url_append <- function(url, path) {
  path <- sub("^([^/])", "/\\1", path)

  url <- httr2::url_parse(url)
  url$path <- paste0(sub("/$", "", url$path), path)
  httr2::url_build(url)
}

# httr2 has a pretty unintuitive output when running interactively.  It waits
# for the user to press <Enter> and then opens up a browser, but the wording
# isn't super clear. It also does not work at all if a browser can't be opened,
# eg. in an SSH session.
#
# Thankfully, if we pretend to not be interactive the behaviour is a lot more
# obvious. It will just print the link to the console and with instructions for
# the user to open it up.
#
# This function is copied from httr2's req_oauth_device, just switching out the
# flow function.  Unfortunately it involves poking at some unexported API for
# the cache. Once https://github.com/r-lib/httr2/pull/763 is released, we should
# be able to get rid of this and set `open_browser = FALSE`.
req_oauth_device_noninteractive <- function(
  req, client, auth_url, scope = NULL, auth_params = list(),
  token_params = list(), cache_disk = FALSE, cache_key = NULL) {
  params <- list(
    client = client,
    auth_url = auth_url,
    scope = scope,
    auth_params = auth_params,
    token_params = token_params
  )
  cache <- httr2:::cache_choose(client, cache_disk, cache_key)

  flow <- function(...) {
    rlang::with_interactive(value = FALSE, httr2::oauth_flow_device(...))
  }
  httr2::req_oauth(req, flow, params, cache = cache)
}

# This returns a function that modifies a httr2 request object, adding whatever
# authentication mechanism is appropriate.
packit_authentication <- function(base_url, token, save_token) {
  if (is.null(token)) {
    auth_url <- url_append(base_url, "deviceAuth")
    token_url <- url_append(base_url, "deviceAuth/token")

    # Packit actually ignores the Client ID. We can use whatever.
    #
    # The `name` argument is used by httr2 to pick the cache location. They
    # recommend using the package name.
    client <- httr2::oauth_client(
      id = "orderly2",
      token_url = token_url,
      name = "orderly2"
    )

    function(r) {
      # The cache_key is needed to differentiate between different Packit
      # instances and not mix tokens.
      #
      # It also sets us apart from previous version of orderly which performed
      # OAuth authentication against GitHub instead of Packit, and has a NULL
      # cache_key. We don't want to accidentally use an old cached GitHub token
      # in place of a Packit one.
      req_oauth_device_noninteractive(
        r, client, auth_url,
        cache_disk = save_token,
        cache_key = base_url)
    }
  } else {
    if (grepl("^\\$", token)) {
      token_variable <- sub("^\\$", "", token)
      token <- Sys.getenv(token_variable, NA_character_)
      if (is.na(token)) {
        cli::cli_abort("Environment variable '{token_variable}' was not set")
      }
    }

    if (grepl("^gh._", token)) {
      cli::cli_abort(c(
        "Using a GitHub token to login to Packit isn't supported anymore.",
        "i" = paste("Either use a Packit token or omit the token",
                    "to use interactive authentication..")))
    }

    function(req) httr2::req_auth_bearer_token(req, token)
  }
}

orderly_location_packit <- function(url, token = NULL, save_token = TRUE) {
  assert_scalar_character(url)
  assert_scalar_character(token, allow_null = TRUE)
  assert_scalar_logical(save_token)

  api_url <- url_append(url, "packit/api")

  orderly_location_http$new(
    url_append(api_url, "outpack"),
    customize = packit_authentication(api_url, token, save_token))
}
