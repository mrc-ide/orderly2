##' List source reports - that is, directories within `src/` that
##' contain a file `orderly.R`
##'
##' @title List source reports
##'
##' @inheritParams orderly_run
##'
##' @return A character vector of names of source reports, suitable
##'   for passing to [orderly2::orderly_run]
##'
##' @seealso [orderly2::orderly_metadata_extract] for listing packets
##'   that have completed
##'
##' @export
orderly_list_src <- function(root = NULL, locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = TRUE,
                    call = environment())
  if (!file.exists(file.path(root$path, "src"))) {
    return(character())
  }
  pos <- fs::dir_ls(file.path(root$path, "src"), type = "directory")
  basename(pos)[file_exists(file.path(pos, "orderly.R"))]
}
