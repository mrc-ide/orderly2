outpack_root <- R6::R6Class(
  "outpack_root",
  cloneable = FALSE,

  private = list(
    index_data = list()
  ),

  public = list(
    path = NULL,
    config = NULL,
    files = NULL,
    logger = NULL,

    initialize = function(path) {
      assert_file_exists(path)
      assert_file_exists(file.path(path, ".outpack"))
      path <- as.character(fs::path_real(path))
      self$path <- path
      self$config <- config_read(path)
      if (self$config$core$use_file_store) {
        self$files <- file_store$new(file.path(path, ".outpack", "files"))
      }
      self$logger <- self$config$logging
      lockBinding("path", self)
    },

    metadata = function(id) {
      ## TODO: this contains more logic than ideal but attempts to
      ## avoid updating the index if needed.  The other thing to do
      ## would _always_ be to update the index but that feels wasteful
      ## really.
      ##
      ## We could probably be much more efficient if we cached all
      ## roots within a session, though doing that safely would
      ## practically mean putting a key file in each root so that we
      ## can detect directory moves.
      meta <- private$index_data$metadata[[id]] %||%
        self$index()$metadata[[id]]
      if (is.null(meta)) {
        stop(sprintf("id '%s' not found in index", id))
      }
      meta
    },

    index = function(skip_cache = FALSE) {
      prev <- if (skip_cache) list() else private$index_data
      private$index_data <- index_update(self, prev, skip_cache)
      private$index_data
    }
  ))


read_location <- function(location_name, root_path, prev) {
  path <- file.path(root_path, ".outpack", "location", location_name)
  packets <- dir(path, re_id)
  is_new <- !(packets %in% prev$packet[prev$location == location_name])
  if (!any(is_new)) {
    return(NULL)
  }

  dat <- lapply(file.path(path, packets[is_new]), jsonlite::read_json)
  data_frame(packet = vcapply(dat, "[[", "packet"),
             time = num_to_time(vnapply(dat, "[[", "time")),
             hash = vcapply(dat, "[[", "hash"),
             location = location_name)
}


read_locations <- function(root, prev) {
  location_name <- root$config$location$name
  if (is.null(prev)) {
    prev <- data_frame(packet = character(),
                       time = empty_time(),
                       hash = character(),
                       location = character())
  }
  new <- do.call(rbind, lapply(location_name, read_location, root$path, prev))
  ret <- rbind(prev, new)
  ## Always sort by location, then id
  ret <- ret[order(match(ret$location, location_name), ret$packet), ]
  ## Avoids weird computed rownames - always uses 1:n
  rownames(ret) <- NULL
  ret
}


read_metadata <- function(root, prev) {
  path <- file.path(root$path, ".outpack", "metadata")
  id_new <- setdiff(dir(path), names(prev))

  if (length(id_new) == 0) {
    return(prev)
  }

  files <- file.path(path, id_new)
  new <- lapply(files, outpack_metadata_index_read)
  names(new) <- id_new
  ret <- c(prev, new)
  ret[order(names(ret))]
  ret
}


## The index consists of a few bits:
## $location - data.frame of id, location and date
## $metadata - named list of full metadata
##
## Later on we'll want to have some sort of index over this (e.g.,
## name/id/parameters) to support the query interface, but that can
## wait.
index_update <- function(root, prev, skip_cache) {
  root_path <- root$path
  path_index <- file.path(root_path, ".outpack", "index", "outpack.rds")

  if (length(prev) == 0 && file.exists(path_index) && !skip_cache) {
    prev <- readRDS(path_index)
  }

  data <- prev

  ## TODO: Add some logging through here.
  data$location <- read_locations(root, data$location)
  data$metadata <- read_metadata(root, data$metadata)
  data$unpacked <- data$location$packet[data$location$location == local]

  if (!identical(data, prev)) {
    fs::dir_create(dirname(path_index))
    saveRDS(data, path_index)
  }

  data
}


## Not just for the file store, but this is how we can interact with
## the files safely:
file_export <- function(root, id, there, here, dest, overwrite) {
  ## This validation *always* occurs; does the packet even claim to
  ## have this path?
  validate_packet_has_file(root, id, there)
  ## TODO: log file copy information, including hashes.  Because copy
  ## can be slow for large files, we might want to do this file by
  ## file?

  ## TODO: The copy should ideally all succeed or all fail wherever
  ## possible

  ## TODO: check that no dependency destination exists, or offer solution
  ## to overwrite (requires argument here, flowing back to the interface)
  here_full <- file.path(dest, here)
  meta <- root$metadata(id)
  hash <- meta$files$hash[match(there, meta$files$path)]
  stopifnot(all(!is.na(hash)))
  fs::dir_create(dirname(here_full))

  if (root$config$core$use_file_store) {
    for (i in seq_along(here_full)) {
      root$files$get(hash[[i]], here_full[[i]], overwrite)
    }
  } else {
    there_full <- file.path(root$path, root$config$core$path_archive,
                     meta$name, meta$id, there)
    if (!all(file.exists(there_full))) {
      missing <- hash[!file.exists(there_full)]
      message <- paste("File not found in archive:\n%s",
                       paste(sprintf("  - %s", missing), collapse = "\n"))
      stop(not_found_error(message, missing))
    }
    ## TODO: Ideally we would have an argument/option support a faster
    ## possibility here if requested (e.g., no validation validate just
    ## size, validate hash); this only applies to this non-file-store
    ## using branch, so typically would affect users running "draft"
    ## type analyses
    for (i in seq_along(here_full)) {
      tryCatch(
        hash_validate_file(there_full[[i]], hash[[i]]),
        error = function(e) stop(not_found_error(e$message, there_full[[i]])))
    }
    fs::file_copy(there_full, here_full, overwrite)
  }
}


file_import_store <- function(root, path, file_path, file_hash) {
  for (i in seq_along(file_path)) {
    if (!is.null(path)) {
      fp <- file.path(path, file_path[[i]])
    } else {
      fp <- file_path[[i]]
    }
    root$files$put(fp, file_hash[[i]])
  }
}


file_import_archive <- function(root, path, file_path, name, id) {
  dest <- file.path(root$path, root$config$core$path_archive, name, id)

  ## TODO: These should not ever happen, so just asserting here.  If
  ## it does happen it requires that the user has provided an id,
  ## and also copied files around?  Not sure how we'd recover here
  ## either.
  stopifnot(path != dest,
            !file.exists(dest))

  ## TODO: open question as to if we should filter this down to just
  ## the required files (as we do here); this means that if the user
  ## has provided "files" to the metadata function we'd be leaving
  ## some files behind.  This does match the behaviour of the file
  ## store version, but not of orderly.
  file_path_dest <- file.path(dest, file_path)
  fs::dir_create(dirname(file_path_dest))
  ## overwrite = FALSE; see assertion above
  fs::file_copy(file.path(path, file_path), file_path_dest, overwrite = FALSE)
}


find_file_by_hash <- function(root, hash) {
  index <- root$index()

  path_archive <- file.path(root$path, root$config$core$path_archive)
  algorithm <- hash_parse(hash)$algorithm

  ## TODO: allow short circuiting validation (e.g., check only the
  ## size matches, or check nothing)
  for (id in index$unpacked) {
    meta <- index$metadata[[id]]
    for (i in which(meta$files$hash == hash)) {
      path <- file.path(path_archive, meta$name, id, meta$files$path[[i]])
      if (file.exists(path) && hash_file(path, algorithm) == hash) {
        return(path)
      }
      ## TODO: incorporate this into logging later:
      message(sprintf("Rejecting file '%s' in '%s/%s'",
                      meta$files$path[[i]], meta$name, id))
    }
  }

  NULL
}


validate_packet_has_file <- function(root, id, path) {
  files <- root$metadata(id)$files$path

  is_dir <- grepl("/$", path)
  found <- path %in% files
  found[is_dir] <- vlapply(
    path[is_dir], function(x) any(string_starts_with(x, files)),
    USE.NAMES = FALSE)

  if (all(found)) {
    return(invisible())
  }

  ## Then, look to see if any of the missing ones are actually directories:
  msg <- path[!found]
  found_if_dir <- vlapply(with_trailing_slash(msg),
                          function(x) any(string_starts_with(x, files)),
                          USE.NAMES = FALSE)

  err <- sprintf("Packet '%s' does not contain path %s",
                 id, paste(squote(msg), collapse = ", "))
  if (any(found_if_dir)) {
    err <- sprintf("%s\n  Consider adding a trailing slash to %s",
                   err, paste(squote(msg[found_if_dir]), collapse = ", "))
  }

  stop(err, call. = FALSE)
}


root_list_unknown_packets <- function(ids, root) {
  setdiff(ids, root$index()$unpacked)
}


root_list_unknown_files <- function(hashes, root) {
  if (root$config$core$use_file_store) {
    hashes[!root$files$exists(hashes)]
  } else {
    idx <- root$index()
    if (length(idx$unpacked) == 0) {
      return(hashes)
    }
    ## This could be quite a slow operation, especially if we always
    ## validate each file (as we currently do)
    hashes[vlapply(hashes, function(h) is.null(find_file_by_hash(root, h)))]
  }
}


add_file_store <- function(root) {
  ## TODO: tidy this up to be clearer about the loop; here we are
  ## hitting the index and looping over the metadata but there's no
  ## need to do so in such a weird way.
  root$files <- file_store$new(file.path(root$path, ".outpack", "files"))
  invisible(lapply(root$index()$unpacked, function(id) {
    meta <- root$metadata(id)
    path <- lapply(meta$files$hash,
                   function(hash) find_file_by_hash(root, hash))
    failed <- vlapply(path, is.null)
    if (any(failed)) {
      missing <- meta$files$path[failed]
      message <- sprintf(
        "the following files were missing or corrupted: '%s'",
        paste(missing, collapse = ", ")
      )
      stop(sprintf("Failed to import packet '%s': %s",
                   id, message))
    }
    path <- vcapply(path, identity)
    file_import_store(root, NULL, path, meta$files$hash)
  }))
}


remove_file_store <- function(root) {
  root$files$destroy()
  root$files <- NULL
}
