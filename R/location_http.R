orderly_location_http <- R6::R6Class(
  "orderly_location_http",

  private = list(
    client = NULL
  ),

  public = list(
    initialize = function(url, authorise = NULL) {
      private$client <- outpack_http_client$new(url, authorise)
    },

    list = function() {
      dat <- private$client$request("/metadata/list")$data
      data_frame(
        packet = vcapply(dat, "[[", "packet"),
        time = num_to_time(vnapply(dat, "[[", "time")),
        hash = vcapply(dat, "[[", "hash"))
    },

    metadata = function(packet_ids) {
      ret <- vcapply(packet_ids, function(id) {
        tryCatch(
          trimws(private$client$request(sprintf("/metadata/%s/text", id),
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
        private$client$request(sprintf("/file/%s", hash), download = dest),
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
      res <- private$client$request(
        "/packets/missing",
        function(r) http_body_json(r, list(ids = ids, unpacked = scalar(TRUE))))
      list_to_character(res$data)
    },

    list_unknown_files = function(hashes) {
      res <- private$client$request(
        "/files/missing",
        function(r) http_body_json(r, list(hashes = hashes)))
      list_to_character(res$data)
    },

    push_file = function(src, hash) {
      res <- private$client$request(
        sprintf("/file/%s", hash),
        function(r) httr2::req_body_file(r, src, "application/octet-stream"))

      invisible(NULL)
    },

    push_metadata = function(packet_id, hash, path) {
      meta <- read_string(path)
      res <- private$client$request(
        sprintf("/packet/%s", hash),
        function(r) httr2::req_body_raw(r, meta, "text/plain"))
      invisible(NULL)
    }
  )
)
