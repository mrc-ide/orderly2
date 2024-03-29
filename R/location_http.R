orderly_location_http <- R6::R6Class(
  "orderly_location_http",

  private = list(
    client = NULL
  ),

  public = list(
    initialize = function(url, auth = NULL) {
      private$client <- outpack_http_client$new(url, auth)
    },

    list = function() {
      dat <- private$client$get("/metadata/list", parse_json = TRUE)$data
      data_frame(
        packet = vcapply(dat, "[[", "packet"),
        time = num_to_time(vnapply(dat, "[[", "time")),
        hash = vcapply(dat, "[[", "hash"))
    },

    metadata = function(packet_ids) {
      ret <- vcapply(packet_ids, function(id) {
        tryCatch(
          trimws(private$client$get(sprintf("/metadata/%s/text", id),
                                    parse_json = FALSE)),
          outpack_http_client_error = function(e) {
            if (e$code == 404) {
              e$message <- sprintf("Some packet ids not found: '%s'", id)
              class(e) <- c("simpleError", class(e))
            }
            stop(e)
          })
      })
      names(ret) <- packet_ids
      ret
    },

    fetch_file = function(hash, dest) {
      ## TODO: not totally obvious how we should set the progress here
      ## (currently always FALSE), possibly via some sort of option,
      ## possibly via whatever logging interface we come up with as we
      ## could turn it on via log levels. Once we decide we can enable
      ## progress in the client, but there's not much point until
      ## then.
      tryCatch(
        private$client$get(sprintf("/file/%s", hash), download = dest),
        outpack_http_client_error = function(e) {
          if (e$code == 404) {
            unlink(dest)
            e$message <- sprintf("Hash '%s' not found at location", hash)
            class(e) <- c("simpleError", class(e))
          }
          stop(e)
        })
    },

    ## TODO: we could get the schemas here from outpack_server too
    list_unknown_packets = function(ids) {
      body <- to_json(list(ids = ids, unpacked = scalar(TRUE)), NULL)
      content <- httr::content_type("application/json")
      res <- private$client$post("/packets/missing", body, content)
      list_to_character(res$data)
    },

    list_unknown_files = function(hashes) {
      body <- to_json(list(hashes = hashes), NULL)
      res <- private$client$post("/files/missing", body,
                                 httr::content_type("application/json"))
      list_to_character(res$data)
    },

    push_file = function(src, hash) {
      body <- httr::upload_file(src, "application/octet-stream")
      res <- private$client$post(sprintf("/file/%s", hash), body)
      invisible(NULL)
    },

    push_metadata = function(packet_id, hash, path) {
      meta <- read_string(path)
      res <- private$client$post(sprintf("/packet/%s", hash), meta,
                                 httr::content_type("text/plain"))
      invisible(NULL)
    }
  ))


orderly_location_packit <- function(url, token) {
  assert_scalar_character(url)
  assert_scalar_character(token)
  if (grepl("^\\$", token)) {
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
  url_login <- paste0(url, "packit/api/auth/login/api")
  url_outpack <- paste0(url, "packit/api/outpack")

  auth <- list(url = url_login, data = list(token = scalar(token)))
  orderly_location_http$new(url_outpack, auth)
}
