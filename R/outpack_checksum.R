## TODO: I am not sure we need to keep this; it's not used anywhere at
## present. It might have been intended for the R-backed server, now
## defunct?
outpack_checksum <- function(hash_algorithm = NULL, root = NULL) {
  root <- root_open(root, require_orderly = FALSE)
  hash_algorithm <- hash_algorithm %||% root$config$core$hash_algorithm

  ## Don't use the index ever here; we want to always read this from
  ## disk.
  ids <- sort(dir(file.path(root$path, ".outpack", "metadata")))
  hash_data(paste(ids, collapse = ""), hash_algorithm)
}
