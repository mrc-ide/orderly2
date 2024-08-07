outpack_insert_packet <- function(path, json, root = NULL) {
  assert_is(root, "outpack_root")
  meta <- outpack_metadata_core_load(json)
  assert_is_directory(path)

  hash_algorithm <- root$config$core$hash_algorithm

  ## At this point we need to require that 'id' is not known to the
  ## system at least in any remote, but possibly also not in the
  ## entire metadata store?
  id <- meta$id

  ## TODO: For 'insert', rather than 'import', do we want to check for
  ## *any* packet that exists?  For now it's academic as there's no
  ## equivalent to "pull" so this is the only way that things might
  ## appear.
  exists <- any(root$index$location(local)$packet == id)
  if (exists) {
    stop(sprintf("'%s' has already been added for '%s'", id, local))
  }

  for (i in seq_len(nrow(meta$depends))) {
    validate_packet_has_file(root, meta$depends$packet[[i]],
                             meta$depends$files[[i]]$there)
  }

  ## LOGGING: Report on things like the number of files added to the
  ## archives
  if (root$config$core$use_file_store) {
    file_import_store(root, path, meta$files$path, meta$files$hash)
  }
  if (!is.null(root$config$core$path_archive)) {
    file_import_archive(root, path, meta$files$path, meta$name, meta$id)
  }

  path_meta <- file.path(root$path, ".outpack", "metadata", id)
  writeLines(json, path_meta, sep = "")

  ## TODO: once we get more flexible remotes, this will get moved into
  ## its own thing.
  hash <- hash_data(json, hash_algorithm)
  time <- Sys.time()
  mark_packet_known(id, local, hash, time, root)
}


mark_packet_known <- function(packet_id, location, hash, time, root) {
  dat <- list(packet = scalar(packet_id),
              time = scalar(time_to_num(time)),
              hash = scalar(hash))
  dest <- file.path(root$path, ".outpack", "location", location, packet_id)
  fs::dir_create(dirname(dest))
  writeLines(to_json(dat, "outpack/location.json"), dest)
}
