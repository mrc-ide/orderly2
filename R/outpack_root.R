outpack_root <- R6::R6Class(
  "outpack_root",
  cloneable = FALSE,

  public = list(
    path = NULL,
    config = NULL,
    files = NULL,
    index = NULL,

    initialize = function(path) {
      assert_file_exists(path)
      assert_file_exists(file.path(path, ".outpack"))
      path <- as.character(fs::path_real(path))
      self$path <- path
      self$config <- config_read(path)
      if (self$config$core$use_file_store) {
        self$files <- file_store$new(file.path(path, ".outpack", "files"))
      }
      self$index <- outpack_index$new(path)
      lockBinding("path", self)
      lockBinding("index", self)
    }
  ))


## Not just for the file store, but this is how we can interact with
## the files safely:
file_export <- function(root, id, there, here, dest, overwrite, call = NULL) {
  ## This validation *always* occurs; does the packet even claim to
  ## have this path?
  validate_packet_has_file(root, id, there, call)
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
    root$files$get(hash, here_full, overwrite)
  } else {
    there_full <- file.path(root$path, root$config$core$path_archive,
                            meta$name, meta$id, there)
    if (!all(file.exists(there_full))) {
      cli::cli_abort(
        c("File not found in archive",
          set_names(there[!file.exists(there_full)], "x")),
        class = "not_found_error",
        call = call)
    }
    for (i in seq_along(here_full)) {
      tryCatch(
        hash_validate_file(there_full[[i]], hash[[i]]),
        error = function(e) {
          cli::cli_abort(
            "File '{there}' in '{meta$name}/{meta$id}' is corrupt",
            parent = e,
            class = "not_found_error",
            call = call)
        })
    }

    copy_files(there_full, here_full, overwrite = overwrite)

    # Files in the archive are read-only to avoid accidental corruption.
    # This is however an implementation detail, and we should export them as
    # writable again.
    if (length(here_full) > 0) { # https://github.com/r-lib/fs/issues/471
      fs::file_chmod(here_full, "u+w")
    }
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

  ## overwrite = FALSE; see assertion above
  copy_files(file.path(path, file_path),
             file_path_dest,
             overwrite = FALSE)

  if (length(file_path_dest) > 0) { # https://github.com/r-lib/fs/issues/471
    fs::file_chmod(file_path_dest, "a-w")
  }
}


find_file_by_hash <- function(root, hash) {
  index <- root$index$data()

  path_archive <- file.path(root$path, root$config$core$path_archive)
  algorithm <- hash_parse(hash)$algorithm

  ## TODO: allow short circuiting validation (e.g., check only the
  ## size matches, or check nothing)
  for (id in index$unpacked) {
    meta <- index$metadata[[id]]
    for (i in which(meta$files$hash == hash)) {
      filename <- meta$files$path[[i]]
      path <- file.path(path_archive, meta$name, id, filename)
      if (!file.exists(path)) {
        cli::cli_alert_warning(
          "Missing file from archive '{filename}' in '{meta$name}/{id}'")
        next
      }
      hash_found <- hash_file(path, algorithm)
      if (file.exists(path) && hash_found == hash) {
        return(path)
      }
      ## Not actually a warning; formats in a way that works within
      ## the overal logging. What is not obvious is that this is
      ## potentially coming from a remote and that's not always clear,
      ## so we need a way of nesting output
      cli::cli_alert_warning(
        "Rejecting file from archive '{filename}' in '{meta$name}/{id}'")
      cli::cli_alert_info(
        "Expected ({cli::symbol$tick}) and found ({cli::symbol$cross}) hashes:")
      cli::cli_alert_success(hash)
      cli::cli_alert_danger(hash_found)
    }
  }

  NULL
}


validate_packet_has_file <- function(root, id, path, call = NULL) {
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
  if (any(found_if_dir)) {
    hint <- "Consider adding a trailing slash to {squote(msg[found_if_dir])}"
  } else {
    hint <- NULL
  }

  near <- near_matches(msg[!found_if_dir], files)
  if (any(i <- lengths(near) > 0)) {
    suggestion <- vcapply(near[i], collapseq, last = " or ")
    hint <- c(hint,
              sprintf("For '%s' did you mean %s", names(near)[i], suggestion))
    add_hint_case <- any(
      mapply(function(a, b) any(tolower(a) %in% tolower(b)), names(near), near))
    if (add_hint_case) {
      hint <- c(hint, "Remember that all orderly paths are case sensitive")
    }
  }

  err <- paste("Packet '{id}' does not contain the requested",
               "{cli::qty(msg)} path{?s} {.path {msg}}")
  cli::cli_abort(c(err, set_names(hint, "i")), call = call)
}


root_list_unknown_packets <- function(ids, root) {
  setdiff(ids, root$index$unpacked())
}


root_list_unknown_files <- function(hashes, root) {
  if (root$config$core$use_file_store) {
    hashes[!root$files$exists(hashes)]
  } else {
    if (length(root$index$unpacked()) == 0) {
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
  invisible(lapply(root$index$unpacked(), function(id) {
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
