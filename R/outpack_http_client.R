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

    get = function(path, ...) {
      self$authorise()
      http_client_request(httr::GET, paste0(self$url, path), ...,
                          self$auth$header)
    },

    post = function(path, body, ...) {
      self$authorise()
      http_client_request(httr::POST, paste0(self$url, path), body = body, ...,
                          self$auth$header)
    }
  ))


http_client_request <- function(verb, url, ..., parse_json = TRUE,
                                download = NULL) {
  if (is.null(download)) {
    response <- verb(url, ...)
  } else {
    response <- verb(url, ...,
                     http_client_download_options(download))
  }

  http_client_handle_error(response)
  if (is.null(download)) {
    txt <- httr::content(response, "text", encoding = "UTF-8")
    if (parse_json) {
      from_json(txt)
    } else {
      txt
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
  code <- httr::status_code(response)
  if (httr::http_error(code)) {
    if (httr::http_type(response) == "application/json") {
      txt <- httr::content(response, "text", encoding = "UTF-8")
      res <- from_json(txt)
      ## I am seeing Packit returning an element 'error' not a list of
      ## errors
      errors <- if ("error" %in% names(res)) list(res$error) else res$errors
      stop(http_client_error(errors[[1]]$detail, code, errors))
    } else {
      stop(http_client_error(httr::http_status(code)$message, code, NULL))
    }
  }
  response
}


http_client_error <- function(msg, code, errors) {
  err <- list(message = msg, errors = errors, code = code)
  class(err) <- c("outpack_http_client_error", "error", "condition")
  err
}


http_client_download_options <- function(dest) {
  c(httr::write_disk(dest),
    httr::accept("application/octet-stream"))
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
    res <- http_client_request(httr::POST, auth$url,
                               body = auth$data, encode = "json")
    cli::cli_alert_success("Logged in successfully")
    auth_cache[[key]] <- httr::add_headers(
      "Authorization" = paste("Bearer", res$token))
  }
  auth_cache[[key]]
}
