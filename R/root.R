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
##' @param ... Additional arguments passed through to
##'   [orderly2::outpack_init], if (and only if) the outpack
##'   repository is not already created.
##'
##' @return An orderly root object, invisibly. Typically this is
##'   called only for its side effect.
##'
##' @export
orderly_init <- function(path, ...) {
  assert_scalar_character(path)
  if (file.exists(file.path(path, "orderly_config.yml"))) {
    return(orderly_root(path, locate = FALSE))
  }
  if (file.exists(path)) {
    if (!is_directory(path)) {
      stop("'path' exists but is not a directory")
    }
    if (!file.exists(file.path(path, ".outpack"))) {
      if (length(dir(path, all.files = TRUE, no.. = TRUE)) > 0) {
        stop("'path' exists but is not empty, or an outpack archive")
      }
    }
  }

  if (file.exists(file.path(path, ".outpack"))) {
    root <- outpack_root_open(path, FALSE)
  } else {
    root <- outpack_init(path, ...)
  }
  writeLines(empty_config_contents(), file.path(path, "orderly_config.yml"))
  invisible(orderly_root(root, locate = FALSE))
}


## In orderly, there's quite a bit more to read here, coping with the
## configuration (orderly_config.yml) which also marks the root.
##
## In orderly, the key fields in the configuration are:
##
## Things that will be removed
##
## * destination - orderly's internal db, now ignored
## * tags - removed as unused
##
## Things that will be entirely reworked
##
## * remote - configuration of remotes, will get a full change as this
##   has lots of issues as is, and because this interacts with
##   outpack's 'location' support
## * changelog - will get overhauled because it's not clear that it works
##   well at the moment
##
## Things that will probably come in without too much change:
##
## * fields - custom fields
## * vault - vault configuration
## * global_resources - might be the first bit to come back in?
## * database - lower priority, as only VIMC and everything else must work first
## * minimum_orderly_version - the required version
orderly_root <- function(root, locate) {
  root <- outpack_root_open(root, locate)
  ## NOTE: it's can't be changed yet, but core.path_archive cannot be
  ## "draft" for this to work well.
  path <- root$path
  config <- orderly_config(path)
  ret <- list(outpack = root, config = config, path = path)
  class(ret) <- "orderly_root"
  ret
}



empty_config_contents <- function() {
  'minimum_orderly_version: "1.99.0"'
}
