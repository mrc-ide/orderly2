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
##' locally, the the metadata is not deleted.
##'
##' We expose this function mostly for users who want to expunge
##' permanently any reference to previously run packets. We hope that
##' there should never need to really be a reason to run it.
##'
##' @title Prune orphan packet metadata
##'
##' @inheritParams orderly_metadata
##'
##' @return Invisibly, a character vector of orphaned packet ids
##' @export
orderly_prune_orphans <- function(root = NULL) {
  root <- root_open(root, require_orderly = FALSE)
  id <- root$index$location("orphan")$packet
  if (length(id) == 0) {
    return(invisible(id))
  }

  idx <- new_query_index(root, build_search_options(location = local))
  is_used <- lengths(lapply(id, idx$get_packet_uses, Inf)) > 0
  if (any(is_used)) {
    cli::cli_alert_info(
      paste("Can't prune {sum(is_used)} orphan packet{?s}, as",
            "{?it is/they are} referenced by other packets"))
    id_drop <- id[!is_used]
  } else {
    id_drop <- id
  }

  if (length(id_drop) == 0) {
    return(invisible(id_drop))
  }

  cli::cli_alert_info("Pruning {length(id_drop)} orphan packet{?s}")
  fs::file_delete(file.path(root$path, ".outpack", "location", orphan, id_drop))
  fs::file_delete(file.path(root$path, ".outpack", "metadata", id_drop))
  root$index$rebuild()
  invisible(id_drop)
}
