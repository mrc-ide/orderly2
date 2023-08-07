outpack_root <- R6::R6Class(
  "outpack_root",
  cloneable = FALSE,

  private = list(
    index_ = NULL
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
      private$index_ <- outpack_index$new(path)
      lockBinding("path", self)
    },

    metadata = function(id) {
      private$index_$metadata(id)
    },

    index = function(skip_cache = FALSE) {
      private$index_$refresh(skip_cache)$data()
    }
  ))


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
  meta <- outpack_metadata_core(id, root)
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
  files <- outpack_metadata_core(id, root)$files$path

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
    meta <- outpack_metadata_core(id, root)
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
