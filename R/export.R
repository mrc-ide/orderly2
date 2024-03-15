##' @param path the destination directory where to create the static location
##' @param packets One or more packets to export
##' @inheritParams orderly_metadata
##' @export
orderly_export_static_location <- function(path, packets, root = NULL, locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())

  metadata <- root$index$data()$metadata
  packets <- find_all_dependencies(packets, metadata)
  files <- unique(unlist(lapply(packets, function(i) metadata[[i]]$files$hash)))

  fs::dir_create(path)
  fs::dir_create(file.path(path, "metadata"))
  fs::dir_create(file.path(path, "file"))

  for (hash in files) {
    fs::file_copy(find_file_by_hash(root, hash), file.path(path, "file", hash), overwrite=TRUE)
  }
  for (id in packets) {
    fs::dir_create(file.path(path, "metadata", id))
    fs::file_copy(file.path(root$path, ".outpack", "metadata", id), file.path(path, "metadata", id, "text"), overwrite=TRUE)
  }

  index <- lapply(packets, function(id) {
    list(
      packet = scalar(id),
      hash = scalar(get_metadata_hash(id, root)),
      time = scalar(time_to_num()))
  })
  writeLines(to_json(list(data=index)), file.path(path, "metadata", "list"))
}
