# This has the same semantics as httr2::req_url_path_append and is mostly just
# copied from there. It always adds the argument to the end of the URL, adding
# a slash only when necessary.
url_append <- function(url, path) {
  path <- sub("^([^/])", "/\\1", path)

  url <- httr2::url_parse(url)
  url$path <- paste0(sub("/$", "", url$path), path)
  httr2::url_build(url)
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
      httr2::req_oauth_device(
        r, client, auth_url,
        cache_disk = save_token,
        cache_key = base_url,
        open_browser = FALSE)
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
