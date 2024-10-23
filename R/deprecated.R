##' Deprecated functions in orderly2. These functions still work (*for
##' now*) but will be removed shortly.  Please update your code.
##'
##' * `orderly_location_pull_packet`; please use [orderly_location_pull()]
##' * `orderly_location_pull_metadata`; please use
##'   [orderly_location_metadata_fetch()]
##'
##' @param ... Arguments forwarded to the new version
##' @return See the linked pages above for the current return type.
##' @rdname orderly2-deprecated
##' @name orderly2-deprecated
NULL


deprecate_warn <- function(old, new, body = NULL) {
  cli::cli_warn(
    c(paste("'{old}()' is deprecated and will be removed soon, please",
            "use '{new}()' instead"),
      body),
    .frequency = "regularly",
    .frequency_id = paste0("orderly_deprecate:", old))
}
