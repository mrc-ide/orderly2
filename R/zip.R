##' Export packets as a zip file.
##'
##' The packets can be imported into a different repository using the
##' [orderly2::orderly_zip_import] function.
##'
##' This is useful as one-time way to publish your results, for example as an
##' artefact accompanying a paper. For back-and-forth collaboration, a shared
##' location should be preferred, as this offers more flexibility.
##'
##' @param path the path where the zip file will be created
##'
##' @param packets One or more packets to export
##'
##' @inheritParams orderly_metadata
##'
##' @return Invisibly, the path to the zip file
##' @export
orderly_zip_export <- function(path, packets, root = NULL, locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())

  index <- root$index$data()
  packets <- find_all_dependencies(packets, index$metadata)
  files <- find_all_files(packets, index$metadata)

  dest <- withr::local_tempfile()
  fs::dir_create(dest)
  fs::dir_create(file.path(dest, "metadata"))
  store <- file_store$new(file.path(dest, "files"))

  fs::file_copy(
    file.path(root$path, ".outpack", "metadata", packets),
    file.path(dest, "metadata", packets))

  if (root$config$core$use_file_store) {
    for (hash in files) {
      store$put(root$files$filename(hash), hash)
    }
  } else {
    for (hash in files) {
      store$put(find_file_by_hash(root, hash), hash)
    }
  }

  packet_list <- index$location[
    match(index$location$packet, packets), c("packet", "hash")]
  contents <- list(packets = packet_list)

  writeLines(to_json(contents, "orderly/export.json"),
             file.path(dest, "outpack.json"))

  zip::zip(fs::path_abs(path), root = dest,
           files = c("outpack.json", "metadata", "files"))

  invisible(path)
}

##' Import packets from a zip file.
##'
##' @param path the path to the zip file to be imported.
##'
##' @inheritParams orderly_metadata
##'
##' @return Invisibly, the IDs of the imported packets
##' @export
orderly_zip_import <- function(path, root = NULL, locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())

  if (!("outpack.json" %in% zip::zip_list(path)$filename)) {
    cli::cli_abort(
      c("Zip file does not contain an 'outpack.json' file at its root",
        i = paste("Are you sure this file was produced by",
                  "orderly2::orderly_zip_export?")))
  }

  src <- withr::local_tempfile()
  zip::unzip(path, exdir = src)

  contents <- jsonlite::read_json(file.path(src, "outpack.json"),
                                  simplifyVector = TRUE)

  zip_import_metadata(root, src, contents$packets, call = environment())
  zip_import_packets(root, src, contents$packets)

  invisible(contents$packets$packet)
}

zip_import_metadata <- function(root, src, packets, call) {
  index <- root$index$data()
  new_packets <- !(packets$packet %in% names(index$metadata))

  ids <- packets$packet[new_packets]
  src_paths <- file.path(src, "metadata", ids)
  dst_paths <- file.path(root$path, ".outpack", "metadata", ids)
  expected_hash <- packets$hash[new_packets]

  for (i in seq_along(src_paths)) {
    metadata <- read_string(src_paths[[i]])

    hash_validate_data(metadata, expected_hash[[i]],
                       sprintf("metadata for '%s'", id), call = call)

    writeLines(metadata, dst_paths[[i]])
  }

  seen_before <- intersect(packets$packet, index$location$packet)
  hash_there <- packets$hash[match(seen_before, packets$packet)]
  hash_here <- index$location$hash[match(seen_before, index$location$packet)]
  err <- hash_there != hash_here
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

zip_import_packets <- function(root, src, packets) {
  store <- file_store$new(file.path(src, "files"))
  index <- root$index$data()
  missing_packets <- packets[!(packets$packet %in% index$unpacked), ]

  if (root$config$core$use_file_store) {
    files <- find_all_files(missing_packets$packet, index$metadata)
    files <- files[!root$files$exists(files)]
    for (hash in files) {
      file_path <- store$get(hash, root$files$tmp(), overwrite = FALSE)
      root$files$put(file_path, hash, move = TRUE)
    }
  }

  for (i in seq_along(missing_packets$packet)) {
    if (!is.null(root$config$core$path_archive)) {
      location_pull_files_archive(missing_packets$packet[[i]], store, root)
    }
    mark_packet_known(missing_packets$packet[[i]], local,
                      missing_packets$hash[[i]], Sys.time(), root)
  }
}
