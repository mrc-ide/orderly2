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
##' @examples
##' path <- orderly2::orderly_example("default")
##' orderly2::orderly_list_src(root = path)
orderly_list_src <- function(root = NULL, locate = TRUE) {
  root_path <- orderly_src_root(root, locate)
  if (!file.exists(file.path(root_path, "src"))) {
    return(character())
  }
  pos <- fs::dir_ls(file.path(root_path, "src"), type = "directory")
  basename(pos)[file_exists(file.path(pos, "orderly.R"))]
}
