mock_headers <- function(...) {
  structure(list(...), class = c("insensitive", "list"))
}

mock_response <- function(content, status = 200, wrap = TRUE, download = NULL) {
  headers <- mock_headers()
  if (!is.null(download)) {
    headers <- mock_headers("content-type" = "application/octet-stream")
    writeBin(content, download)
  } else if (inherits(content, "json")) {
    headers <- mock_headers("content-type" = "application/json")
    if (wrap) {
      content <- sprintf('{"status":"success","errors":null,"data":%s}',
                         content)
    }
    class(content) <- NULL
    content <- c(writeBin(content, raw()), as.raw(0L))
  } else {
    stop("Unhandled mock response type")
  }
  structure(list(status_code = status,
                 headers = headers,
                 content = content),
            class = "response")
}


json_string <- function(s) {
  class(s) <- "json"
  s
}
