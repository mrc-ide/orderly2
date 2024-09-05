mock_headers <- function(...) {
  structure(list(...), class = c("insensitive", "list"))
}

mock_response <- function(content, status = 200L, wrap = TRUE,
                          download = NULL) {
  if (is.raw(content)) {
    headers <- mock_headers("content-type" = "application/octet-stream")
  } else if (inherits(content, "json")) {
    headers <- mock_headers("content-type" = "application/json")
    if (wrap) {
      content <- sprintf('{"status":"success","errors":null,"data":%s}',
                         content)
    }
    class(content) <- NULL
    content <- c(writeBin(content, raw()), as.raw(0L))
  } else if (is.character(content)) {
    headers <- mock_headers("content-type" = "text/plain")
    content <- c(writeBin(content, raw()), as.raw(0L))
  } else if (is.na(content)) {
    headers <- mock_headers()
    content <- raw(0)
  } else {
    stop("Unhandled mock response type")
  }

  if (!is.null(download)) {
    writeBin(content, download)
  }

  httr2::response(status_code = status,
                  headers = headers,
                  body = content)
}


json_string <- function(s) {
  class(s) <- "json"
  s
}


clear_auth_cache <- function() {
  rm(list = ls(auth_cache), envir = auth_cache)
}

local_mock_response <- function(..., env = rlang::caller_env()) {
  mock <- mockery::mock(mock_response(...))
  httr2::local_mocked_responses(function(req) mock(req), env = env)
  mock
}
