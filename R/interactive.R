## This is something that we might improve over time - it will likely
## be useful to have some sort of "register interactive" function
## which we could then just look up.
##
## I am not sure if we also want to allow working interactively from a
## draft directory too.
detect_orderly_interactive_path <- function(path = getwd()) {
  path_split <- fs::path_split(path)[[1]]
  is_plausible <- length(path_split) > 2 &&
    path_split[[length(path_split) - 1]] == "src" &&
    file.exists(file.path(path, "../..", "orderly_config.yml"))
  if (!is_plausible) {
    stop(sprintf("Failed to detect orderly path at '%s'", path))
  }
  root_path <- as.character(fs::path_norm(file.path(path, "../..")))
  orderly_root(root_path, FALSE)
}


.interactive <- new.env(parent = emptyenv())

##' Set search options for interactive use of orderly; see
##' [orderly3::orderly_dependency] and [orderly3::orderly_run] for
##' details. This may be either an [outpack::outpack_search_options]
##' object, or a list that will be coerced into one at the point of
##' use (or `NULL`). This applies only for the current session, but
##' applies to all interactive uses of orderly functions that might
##' have recieved a copy of `search_options` via
##' [orderly3::orderly_run]
##'
##' @title Set search options for interactive use
##'
##' @param options Optional control over locations, when used
##'   with [orderly3::orderly_dependency]; see of Details section of
##'   [orderly3::orderly_run].
##'
##' @inheritParams orderly_run
##'
##' @return Nothing, called for its side effects
##' @export
orderly_interactive_set_search_options <- function(options = NULL,
                                                   root = NULL,
                                                   locate = TRUE) {
  .interactive$search_options <- options
}
