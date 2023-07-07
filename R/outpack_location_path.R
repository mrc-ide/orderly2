outpack_location_path <- R6::R6Class(
  "outpack_location_path",

  private = list(
    root = NULL,
    local_id = NULL
  ),

  public = list(
    initialize = function(path) {
      private$root <- outpack_root_open(path, locate = FALSE)
      private$local_id <- local_location_id(private$root)
    },

    list = function() {
      dat <- private$root$index()$location
      dat[dat$location == private$local_id, c("packet", "time", "hash")]
    },

    metadata = function(packet_ids) {
      ## TODO: if we're filtering based on which location we're
      ## shipping results from, then we need to validate that these
      ## ids are all found within our data.
      dat <- private$root$index()$location
      msg <- setdiff(packet_ids, dat$packet[dat$location == private$local_id])
      if (length(msg) > 0) {
        stop("Some packet ids not found: ",
             paste(squote(msg), collapse = ", "))
      }
      paths <- file.path(private$root$path, ".outpack", "metadata", packet_ids)
      ret <- vcapply(paths, read_string)
      names(ret) <- packet_ids
      ret
    },

    fetch_file = function(hash, dest) {
      ## TODO: we might need to give some better hints here as to what
      ## the user was looking for, but almost certainly that's better
      ## done by the calling function.
      if (private$root$config$core$use_file_store) {
        path <- private$root$files$filename(hash)
        if (!file.exists(path)) {
          stop(sprintf("Hash '%s' not found at location", hash))
        }
      } else {
        path <- find_file_by_hash(private$root, hash)
        if (is.null(path)) {
          stop(sprintf("Hash '%s' not found at location", hash))
        }
      }
      fs::file_copy(path, dest)
      dest
    },

    list_unknown_packets = function(ids) {
      root_list_unknown_packets(ids, root = private$root)
    },

    list_unknown_files = function(hashes) {
      root_list_unknown_files(hashes, private$root)
    },

    push_file = function(src, hash) {
      location_path_import_file(src, hash, private$root)
    },

    push_metadata = function(packet_id, root) {
      hash <- get_metadata_hash(packet_id, root)
      path <- file.path(root$path, ".outpack", "metadata", packet_id)
      location_path_import_metadata(read_string(path), hash, private$root)
    }
  ))


## This split just acts to make the http one easier to think about -
## it's not the job of the driver to do validation, but the server.
location_path_import_metadata <- function(str, hash, root) {
  meta <- outpack_metadata_load(as_json(str))
  id <- meta$id
  hash_validate_data(str, hash, sprintf("metadata for '%s'", id))

  unknown_files <- root_list_unknown_files(meta$files$hash, root)
  if (length(unknown_files) > 0) {
    stop(
      sprintf("Can't import metadata for '%s', as files missing:\n%s",
              id, paste(sprintf("  - %s", unknown_files), collapse = "\n")))
  }
  unknown_packets <- root_list_unknown_packets(meta$depends$packet, root)
  if (length(unknown_packets) > 0) {
    stop(sprintf(
      "Can't import metadata for '%s', as dependencies missing:\n%s",
      id, paste(sprintf("  - %s", unknown_packets), collapse = "\n")))
  }

  if (!is.null(root$config$core$path_archive)) {
    dst <- file.path(root$path, root$config$core$path_archive,
                     meta$name, id, meta$files$path)
    root$files$get(meta$files$hash, dst)
  }

  writeLines(str, file.path(root$path, ".outpack", "metadata", id))
  local_id <- local_location_id(root)
  time <- Sys.time()
  mark_packet_known(id, local_id, hash, time, root)
}


location_path_import_file <- function(path, hash, root) {
  if (!root$config$core$use_file_store) {
    stop("Can't push files into this server, as it does not have a file store")
  }
  root$files$put(path, hash)
}
