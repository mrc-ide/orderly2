##' Prune orphan packets from your metadata store.  This function can
##' be used to remove references to packets that are no longer
##' reachable; this could have happened because you deleted a packet
##' manually from the archive and ran
##' [orderly2::orderly_validate_archive] or because you removed a
##' location.
##'
##' If an orphan packet is not used anywhere, then we can easily drop
##' it - it's as if it never existed.  If it is referenced by metadata
##' that you know about from elsewhere but not locally, then that is a
##' problem for the upstream location (and one that should not
##' happen). If you have referenced it in a packet that you have run
##' locally, thne the metadata is not deleted.
##'
##' @title Prune orphan packet metadata
##'
##' @inheritParams orderly_metadata
##'
##' @return Invisibly, a character vector of orphaned packet ids
orderly_prune_orphans <- function(root = NULL, locate = TRUE) {
  root <- root_open(root, locate = TRUE, require_orderly = FALSE)
  id <- root$index$location("orphan")$packet
  if (length(id) == 0) {
    return(invisible(id))
  }
  browser()
  ## I 
  idx <- new_query_index(root, orderly_search_options(location = local))
  a <- lapply(id, idx$get_packet_uses, Inf)
  b <- lapply(id, idx$get_packet_depends, Inf)
  if (any(lengths(a) > 0) || any(lengths(b) > 0)) {
    browser()
  }

  ## loc <- root$index$location(NULL)
  ## err <- intersect(loc$packet[loc$location != orphan], id)
  ## if (length(err) > 0) {
  ##   browser()
  ## }
  
  id_drop <- id
  cli::cli_alert_info("Orphaning {length(id_drop)} orphan packet{?s}")
  fs::file.remove(file.path(root$path, "location", orphan, id_drop))
  fs::file.remove(file.path(root$path, "metadata", id_drop))
  invisible(id_drop)
}
