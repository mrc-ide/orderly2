##' Compute a checksum over all known packet metadata. This can be
##' used to check whether a cache of all metadata needs refreshing (or
##' agrees between two roots) without having to read a lot of data. It
##' is unlikely that most users will call this function.
##'
##' This function matches the behaviour (and hash) of the outpack
##' server. We may expand the behaviour or either or both of these to
##' include a similar checksum that includes location data.
##'
##' @title Compute packet metadata checksum
##'
##' @param hash_algorithm The hash algorithm to use, defaulting to the
##'   root's hash algorithm.
##'
##' @inheritParams outpack_search
##'
##' @return A hash string
##' @export
outpack_checksum <- function(hash_algorithm = NULL, root = NULL) {
  root <- outpack_root_open(root, locate = TRUE)
  hash_algorithm <- hash_algorithm %||% root$config$core$hash_algorithm

  ## Don't use the index ever here; we want to always read this from
  ## disk.
  ids <- sort(dir(file.path(root$path, ".outpack", "metadata")))
  hash_data(paste(ids, collapse = ""), hash_algorithm)
}
