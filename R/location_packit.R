github_oauth_client <- function() {
  # Surprisingly, we don't actually need the Client ID here to match the one
  # used by Packit. It should be fine to hardcode a value regardless of which
  # server we are talking to.
  httr2::oauth_client(
    id = "Ov23liUrbkR0qUtAO1zu",
    token_url = "https://github.com/login/oauth/access_token",
    name = "orderly2"
  )
}

do_oauth_device_flow <- function(base_url, cache_disk) {
  # httr2 has a pretty unintuitive output when running interactively.
  # It waits for the user to press <Enter> and then opens up a browser, but the
  # wording isn't super clear. It also does not work at all if a browser can't
  # be opened, eg. in an SSH session.
  #
  # Thankfully, if we pretend to not be interactive the behaviour is a lot more
  # obvious. It will just print the link to the console and with instructions
  # for the user to open it up.
  res <- rlang::with_interactive(value = FALSE, {
    httr2::oauth_token_cached(
      client = github_oauth_client(),
      flow = httr2::oauth_flow_device,
      flow_params = list(
        auth_url = "https://github.com/login/device/code",
        scope = "read:org"),
      cache_disk = cache_disk)
  })
  res$access_token
}

# Logging in with packit is quite slow and we'll want to cache this; but we
# won't be holding a persistent handle to the root.  So for now at least we'll
# keep a pool of generated bearer token headers, stored against the hash of the
# auth details. We only store this on successful login.
#
# This does mean there's no way to flush the cache and force a login, but that
# should hopefully not be that big a problem.  We'll probably want to refresh
# the tokens from the request anyway.
#
# It also means the user cannot easily use two different identities on the same
# server from within the same session.
auth_cache <- new.env(parent = emptyenv())
packit_authorisation <- function(base_url, token, save_token) {
  # If a non-Github token is provided, we assume it is a native Packit token
  # and use that directly.
  if (!is.null(token) && !grepl("^gh._", token)) {
    return(list("Authorization" = paste("Bearer", token)))
  }

  key <- rlang::hash(list(base_url = base_url, token = token))

  if (is.null(auth_cache[[key]])) {
    cli_alert_info("Logging in to {base_url}")
    if (is.null(token)) {
      token <- do_oauth_device_flow(base_url, cache_disk = save_token)
    }

    login_url <- paste0(base_url, "packit/api/auth/login/api")
    res <- http_client_request(
      login_url,
      function(r) http_body_json(r, list(token = scalar(token))))

    cli_alert_success("Logged in successfully")

    auth_cache[[key]] <- list("Authorization" = paste("Bearer", res$token))
  }
  auth_cache[[key]]
}

orderly_location_packit <- function(url, token = NULL, save_token = TRUE) {
  assert_scalar_character(url)
  assert_scalar_character(token, allow_null = TRUE)
  assert_scalar_logical(save_token)

  if (!is.null(token) && grepl("^\\$", token)) {
    token_variable <- sub("^\\$", "", token)
    token <- Sys.getenv(token_variable, NA_character_)
    if (is.na(token)) {
      cli::cli_abort(
        "Environment variable '{token_variable}' was not set")
    }
  }

  if (!grepl("/$", url)) {
    url <- paste0(url, "/")
  }

  orderly_location_http$new(
    paste0(url, "packit/api/outpack"),
    function() packit_authorisation(url, token, save_token))
}
