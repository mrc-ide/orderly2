##' Initialise an empty orderly repository. An orderly repository is
##' also an outpack repository, so most of the work here is done by
##' [orderly2::outpack_init()], however, you should call this
##' function!  You can also turn any outpack repository into an
##' orderly one, adding appropriate bits of configuration into it,
##' though this function.
##'
##' @title Initialise an orderly repository
##'
##' @param path The path to initialise the repository at.  If the
##'   repository is already initialised, this operation does nothing.
##'
##' @param path_archive Path to the archive directory, used to store
##'   human-readable copies of packets.  If `NULL`, no such copy is
##'   made, and `file_store` must be `TRUE`
##'
##' @param use_file_store Logical, indicating if we should use a
##'   content-addressable file-store as the source of truth for
##'   packets.  If `archive` is non-`NULL`, the file-store will be
##'   used as the source of truth and the duplicated files in archive
##'   exist only for convenience.
##'
##' @param require_complete_tree Logical, indicating if we require a
##'   complete tree of packets.  This currently affects
##'   [orderly2::outpack_location_pull_packet], by requiring that it
##'   always operates in recursive mode.  This is `FALSE` by default,
##'   but set to `TRUE` if you want your archive to behave well as a
##'   location; if `TRUE` you will always have all the packets that
##'   you hold metadata about.
##'
##' @param logging_console Logical, indicating if we should log to the
##'   console. If `TRUE`, then many operations will produce
##'   informational output; set to `FALSE` to prevent this.
##'
##' @param logging_threshold The degree of verbosity; one of `info`,
##'   `debug` or `trace` in increasing order of verbosity.
##'
##' @return An orderly root object, invisibly. Typically this is
##'   called only for its side effect.
##'
##' @export
orderly_init <- function(path,
                         path_archive = "archive",
                         use_file_store = FALSE,
                         require_complete_tree = FALSE,
                         logging_console = TRUE,
                         logging_threshold = "info") {
  ## TODO: there's an inconsistency here between the arguments to this
  ## function using underscores and the arguments to
  ## 'outpack_config_set' which uses dots. We might take a list of
  ## options and dots here, which would allow sharing more code. That
  ## would also allow addition of more options more easily.
  ##
  ## TODO: the constraints here need writing out clearly for any other
  ## implementation that might seek to change them!
  assert_scalar_character(path)
  has_orderly_config <- file.exists(file.path(path, "orderly_config.yml"))
  if (!has_orderly_config && file.exists(path)) {
    if (!is_directory(path)) {
      stop("'path' exists but is not a directory")
    }
    if (!file.exists(file.path(path, ".outpack"))) {
      if (length(dir(path, all.files = TRUE, no.. = TRUE)) > 0) {
        stop("'path' exists but is not empty, or an outpack archive")
      }
    }
  }

  path_outpack <- file.path(path, ".outpack")
  if (file.exists(path_outpack)) {
    ## TODO: check no args given, and if given check that they don't
    ## differ from the versions that were baked in.
    root <- outpack_root_open(path, FALSE)
  } else {
    config <- config_new(path_archive, use_file_store, require_complete_tree,
                         logging_console, logging_threshold)
    fs::dir_create(path_outpack)
    fs::dir_create(file.path(path_outpack, "metadata"))
    fs::dir_create(file.path(path_outpack, "location"))
    config_write(config, path)
    root <- outpack_root$new(path)
    outpack_log_info(root, "init", path, "orderly2::outpack_init")
  }

  if (!has_orderly_config) {
    writeLines(empty_config_contents(), file.path(path, "orderly_config.yml"))
  }

  invisible(orderly_root(path, locate = FALSE, environment()))
}


orderly_root <- function(path, locate, call = NULL) {
  if (inherits(path, "orderly_root")) {
    return(path)
  }
  if (!file.exists(file.path(path, ".outpack"))) {
    cli::cli_abort(
      c(sprintf("orderly directory '%s' not initialised", root),
        x = "Did not find an '.outpack' directory within path",
        i = 'Please run orderly2::orderly_init("{root}") to initialise',
        i = "See ?orderly_init for more arguments to this function"),
      call = call)
  }
  root <- outpack_root_open(path, locate)
  root$config$orderly <- orderly_config(path)
  class(root) <- c("orderly_root", class(root))
  root
}


empty_config_contents <- function() {
  'minimum_orderly_version: "1.99.0"'
}
