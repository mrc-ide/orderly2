##' Initialise an empty orderly repository, or initialise a source
##' copy of an orderly repository (see Details). An orderly repository
##' is defined by the presence of a file `orderly_config.yml` at its
##' root, along with a directory `.outpack/` at the same level.
##'
##' It is expected that `orderly_config.yml` will be saved in version
##' control, but that `.outpack` will be excluded from version
##' control; this means that for every clone of your project you will
##' need to call `orderly2::orderly_init()` to initialise the
##' `.outpack` directory. If you forget to do this, an error will be
##' thrown reminding you of what you need to do.
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
##' @return The full, normalised, path to the root,
##'   invisibly. Typically this is called only for its side effect.
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
    outpack_log_info(root, "init", path, "orderly2::orderly_init")
  }

  if (!has_orderly_config) {
    writeLines(empty_config_contents(), file.path(path, "orderly_config.yml"))
  }

  root <- orderly_root_open(path, locate = FALSE, environment())
  invisible(root$path)
}


empty_config_contents <- function() {
  'minimum_orderly_version: "1.99.0"'
}


## There's quite a few options here:
##
## * find an existing outpack root that is not an orderly root
##   - sometimes require that we upgrade (e.g., run) [require orderly t]
##   - sometimes just work with it (e.g., search, extract, copy) [r o f]
## * find an existing orderly root that is not an outpack root
##   - always error, indicate what to do
##
## * also check that the outpack and orderly path are compatibible
##   (this is actually quite hard to get right, but should be done
##   before anything is created I think)
new_root_open <- function(path, locate, require_orderly = FALSE, call = NULL) {
  if (inherits(path, "outpack_root")) {
    if (require_orderly && !inherits(path, "orderly_root")) {
      browser()
    }
    return(path)
  }
  if (is.null(path)) {
    path <- getwd()
  }
  assert_scalar_character(path)
  assert_is_directory(path)
  if (locate) {
    path_outpack <- find_file_descend(".outpack", path)
    path_orderly <- find_file_descend("orderly_config.yml", path)
    has_outpack <- !is.null(path_outpack)
    has_orderly <- !is.null(path_orderly)
    is_inconsistent <- require_orderly && has_outpack && has_orderly &&
      path_outpack != path_orderly
    if (is_inconsistent) {
      stop("TODO: this requires guidance")
    }
    path_open <- path_outpack
  } else {
    has_outpack <- file.exists(file.path(path, ".outpack"))
    has_orderly <- file.exists(file.path(path, "orderly_config.yml"))
    path_open <- path
  }
  if (!has_outpack && !has_orderly) {
    cli::cli_abort(
      c(sprintf(
        "Did not find existing orderly (or outpack) root in '%s'", path),
        i = paste("Expected to find file 'orderly_config.yml' or directory",
                  "'.outpack/'"),
        i = if (locate) "Looked in parents of this path without success"),
      call = call)
  }
  if (has_orderly && !has_outpack) {
    cli::cli_abort(
      c(sprintf("orderly directory '%s' not initialised", path),
        x = "Did not find an '.outpack' directory within path",
        i = 'Please run orderly2::orderly_init("{path}") to initialise',
        i = "See ?orderly_init for more arguments to this function"),
      call = call)
  }

  root <- outpack_root$new(path_open)

  if (has_orderly) {
    root$config$orderly <- orderly_config(root$path)
    class(root) <- c("orderly_root", class(root))
  } else if (require_orderly) {
    stop("TODO: handle this case well too")
  }

  root
}


orderly_root_open <- function(path, locate, call = NULL) {
  new_root_open(path, locate, TRUE, call)
}


outpack_root_open <- function(path, locate, call = NULL) {
  new_root_open(path, locate, FALSE, call)
}
