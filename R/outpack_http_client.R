outpack_http_client <- R6::R6Class(
  "outpack_http_client",

  public = list(
    url = NULL,
    auth = NULL,

    initialize = function(url, auth) {
      self$url <- sub("/$", "", url)
      if (is.null(auth)) {
        self$auth <- list(enabled = FALSE)
      } else {
        self$auth <- list(enabled = TRUE, url = auth$url, data = auth$data)
      }
    },

    authorise = function() {
      needs_auth <- self$auth$enabled && is.null(self$auth$header)
      if (needs_auth) {
        self$auth$header <- http_client_login(self$url, self$auth)
      }
    },

    request = function(path, customize = identity, ...) {
      self$authorise()
      http_client_request(
        self$url,
        function(r) {
          r <- httr2::req_url_path_append(r, path)
          r <- httr2::req_headers(r, !!!self$auth$header)
          customize(r)
        }, ...)
    }
  ))

http_client_request <- function(url, customize = identity, download = NULL,
                                parse_json = TRUE) {
  req <- httr2::request(url)
  if (!is.null(download)) {
    req <- httr2::req_headers(req, Accept = "application/octet-stream")
  }

  req <- customize(req)
  resp <- tryCatch(
    httr2::req_perform(req, path = download),
    httr2_http = function(cnd) {
      http_client_handle_error(cnd$resp)
    })

  if (is.null(download)) {
    if (parse_json) {
      httr2::resp_body_json(resp, simplifyVector = FALSE)
    } else {
      httr2::resp_body_string(resp)
    }
  } else {
    download
  }
}


http_client_handle_error <- function(response) {
  ## TODO: we can cope with timeouts here with some care; if we know
  ## that an expired timeout produces a certain error code we watch
  ## for that and then reauthenticate; that requires that a callback
  ## is passed through here too.
  if (httr2::resp_is_error(response)) {
    if (httr2::resp_content_type(response) == "application/json") {
      res <- httr2::resp_body_json(response)
      ## I am seeing Packit returning an element 'error' not a list of
      ## errors
      errors <- if ("error" %in% names(res)) list(res$error) else res$errors
      stop(http_client_error(errors[[1]]$detail,
                             httr2::resp_status(response),
                             errors))
    } else {
      stop(http_client_error(httr2::resp_status_desc(response),
                             httr2::resp_status(response),
                             NULL))
    }
  }
  response
}


http_client_error <- function(msg, code, errors) {
  err <- list(message = msg, errors = errors, code = code)
  class(err) <- c("outpack_http_client_error", "error", "condition")
  err
}


## Logging in with packit is quite slow and we'll want to cache this;
## but we won't be holding a persistant handle to the root.  So for
## now at least we'll keep a pool of generated bearer token headers,
## stored against the hash of the auth details (so the url and the
## token used to log in with).  We only store this on successful
## login.
##
## This does mean there's no way to flush the cache and force a login,
## but that should hopefully not be that big a problem.  We'll
## probably want to refresh the tokens from the request anyway.
auth_cache <- new.env(parent = emptyenv())
http_client_login <- function(name, auth) {
  key <- rlang::hash(auth)
  if (is.null(auth_cache[[key]])) {
    cli::cli_alert_info("Logging in to {name}")

    res <- http_client_request(auth$url,
                               function(r) httr2::req_body_json(r, auth$data))

    cli::cli_alert_success("Logged in successfully")
    auth_cache[[key]] <- list("Authorization" = paste("Bearer", res$token))
  }
  auth_cache[[key]]
}
