##' Export packets as a zip file.
##'
##' The packets can be imported into a different repository using the
##' [orderly2::orderly_import_zip] function.
##'
##' This is useful as one-time way to publish your results, for example as an
##' artefact accompanying a paper. For back-and-forth collaboration, a shared
##' location should be priviledged.
##'
##' @param path the path where the zip file will be created
##'
##' @param packets One or more packets to export
##'
##' @inheritParams orderly_metadata
##'
##' @return Nothing
##' @export
orderly_export_zip <- function(path, packets, root = NULL, locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())

  metadata <- root$index$data()$metadata
  packets <- find_all_dependencies(packets, metadata)
  files <- find_all_files(packets, metadata)

  dest <- withr::local_tempfile()
  fs::dir_create(dest)
  fs::dir_create(file.path(dest, "metadata"))
  store <- file_store$new(file.path(dest, "files"))

  fs::file_copy(
    file.path(root$path, ".outpack", "metadata", packets),
    file.path(dest, "metadata", packets))

  for (hash in files) {
    store$put(find_file_by_hash(root, hash), hash)
  }

  zip::zip(fs::path_abs(path), root = dest, files = c("metadata", "files"))
  invisible()
}

##' Import packets from a zip file.
##'
##' @param path the path to the zip file to be imported.
##'
##' @inheritParams orderly_metadata
##'
##' @return Invisibly, the IDs of the imported packets
##' @export
orderly_import_zip <- function(path, root = NULL, locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())
  index <- root$index$data()

  hash_algorithm <- root$config$core$hash_algorithm

  src <- withr::local_tempfile()
  zip::unzip(path, exdir = src)
  store <- file_store$new(file.path(src, "files"))

  ids <- dir(file.path(src, "metadata"))

  # TODO: is using the root's hash algorithm correct? What if the origin had
  # used a different hash, now there are two hashes for the same packet. We
  # don't record the hash algorithm anywhere in the zip files, maybe we should.
  metadata_hashes <- hash_metadata_files(
    file.path(src, "metadata", ids), hash_algorithm)

  known_packets <- ids %in% names(index$metadata)
  missing_packets <- !(ids %in% index$unpacked)

  import_check_hashes(src, ids[known_packets], metadata_hashes[known_packets],
                      root, call = environment())

  fs::file_copy(
    file.path(src, "metadata", ids[!known_packets]),
    file.path(root$path, ".outpack", "metadata", ids[!known_packets]))

  if (root$config$core$use_file_store) {
    # The index needs reloading to take into account the new metadata we just
    # pulled.
    index <- root$index$data()
    files <- find_all_files(ids, index$metadata)
    files <- files[!root$files$exists(files)]
    for (hash in files) {
      file_path <- store$get(hash, root$files$tmp(), overwrite = FALSE)
      root$files$put(file_path, hash, move = TRUE)
    }
  }

  for (i in which(missing_packets)) {
    if (!is.null(root$config$core$path_archive)) {
      location_pull_files_archive(ids[[i]], store, root)
    }
    mark_packet_known(ids[[i]], local, metadata_hashes[[i]], Sys.time(), root)
  }

  invisible(ids)
}

import_check_hashes <- function(src, ids, hashes, root, call) {
  index <- root$index$data()
  hash_algorithm <- root$config$core$hash_algorithm

  hash_here <- index$location$hash[match(ids, index$location$packet)]
  err <- hashes != hash_here
  if (any(err)) {
    cli::cli_abort(
      c("Imported file has conflicting metadata",
        x = paste("This is {.strong really} bad news. The zip file contains",
                  "packets with a different hash than the metadata already in",
                  "this repository. I'm not going to import this new metadata",
                  "but there's no guarantee that the older metadata is",
                  "actually what you want!"),
        i = "Conflicts for: {squote(ids[err])}",
        i = "We would be interested in this case, please let us know"),
      call = call)
  }
  invisible()
}
