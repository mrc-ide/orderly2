## NOTE: none of the auth bits here are done yet - we have a system in
## the orderlyweb client that lets us do this in a fairly pluggable
## way, supporting username/password and token based auth (with the
## former getting a token via username/password)
outpack_http_client <- R6::R6Class(
  "outpack_http_client",

  public = list(
    url = NULL,

    initialize = function(url) {
      self$url <- url
    },

    get = function(path, ...) {
      http_client_request(httr::GET, self$url, path, ...)
    }
  ))


http_client_request <- function(verb, server, path, ..., parse_json = TRUE,
                                download = NULL) {
  if (is.null(download)) {
    response <- verb(paste0(server, path), ...)
  } else {
    response <- verb(paste0(server, path), ...,
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


## This could probably be simplified considerably, it was designed to
## cope with something like docker or vault where we were less in
## control of the errors, and we can always put back in some better
## support later.
http_client_handle_error <- function(response) {
  code <- httr::status_code(response)
  if (code >= 400) {
    txt <- httr::content(response, "text", encoding = "UTF-8")
    res <- from_json(txt)
    stop(http_client_error(res$errors[[1]]$detail, code, res$errors))
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
