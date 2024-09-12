outpack_http_client <- R6::R6Class(
  "outpack_http_client",

  public = list(
    url = NULL,
    authorise = NULL,

    initialize = function(url, authorise = NULL) {
      self$url <- sub("/$", "", url)
      if (is.null(authorise)) {
        self$authorise <- function() NULL
      } else {
        self$authorise <- authorise
      }
    },

    request = function(path, customize = identity, ...) {
      auth_headers <- self$authorise()
      http_client_request(
        self$url,
        function(r) {
          r <- httr2::req_url_path_append(r, path)
          r <- httr2::req_headers(r, !!!auth_headers)
          customize(r)
        }, ...)
    }
  ))

http_body_json <- function(request, body) {
  httr2::req_body_json(request, body, auto_unbox = FALSE)
}

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
    if (identical(httr2::resp_content_type(response), "application/json")) {
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
