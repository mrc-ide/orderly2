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
##' You can safely call `orderly2::orderly_init()` on an
##' already-initialised directory, however, any arguments passed
##' through must exactly match the configuration of the current root,
##' otherwise an error will be thrown. Please use
##' [orderly2::orderly_config_set] to change the configuration, as
##' this ensures that the change in configuration is possible. If
##' configuration options are given but match those that the directory
##' already uses, then nothing happens.
##'
##' If the repository that you call `orderly2::orderly_init()` on is
##' already initialised with an `.outpack` directory but not an
##' `orderly_config.yml` file, then we will write that file too.
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
##'   [orderly2::orderly_location_pull_packet], by requiring that it
##'   always operates in recursive mode.  This is `FALSE` by default,
##'   but set to `TRUE` if you want your archive to behave well as a
##'   location; if `TRUE` you will always have all the packets that
##'   you hold metadata about.
##'
##' @return The full, normalised, path to the root,
##'   invisibly. Typically this is called only for its side effect.
##'
##' @export
##' @examples
##'
##' # We'll use an automatically cleaned-up directory for the root:
##' path <- withr::local_tempdir()
##'
##' # Initialise a new repository, setting an option:
##' orderly2::orderly_init(path, use_file_store = TRUE)
##'
##' # fs::dir_tree(path, all = TRUE)
orderly_init <- function(path,
                         path_archive = "archive",
                         use_file_store = FALSE,
                         require_complete_tree = FALSE) {
  assert_scalar_character(path)
  has_orderly_config <- file.exists(file.path(path, "orderly_config.yml"))
  if (!has_orderly_config && file.exists(path)) {
    if (!is_directory(path)) {
      cli::cli_abort("'path' exists but is not a directory")
    }
    if (!file.exists(file.path(path, ".outpack"))) {
      ## We may need to relax this, but it's not really clear to me
      ## how the user gets into this position; they have a bunch of
      ## files there and they want to a root into it?
      ##
      ## One option is provide a boolean arg to proceed anyway in this
      ## case, at the moment there's not a lot that can be done to
      ## undo this situation.
      if (length(dir(path, all.files = TRUE, no.. = TRUE)) > 0) {
        cli::cli_abort(c(
          "'path' exists but is not empty, or an outpack archive",
          i = "Please have a chat with us if this is something you need to do"))
      }
    }
  }

  config <- config_new(path_archive, use_file_store, require_complete_tree)

  path_outpack <- file.path(path, ".outpack")
  if (file.exists(path_outpack)) {
    root <- root_open(path, locate = FALSE, require_orderly = FALSE)
    root_validate_same_configuration(match.call(), config, root, environment())
  } else {
    fs::dir_create(path_outpack)
    fs::dir_create(file.path(path_outpack, "metadata"))
    fs::dir_create(file.path(path_outpack, "location"))
    config_write(config, path)
    root <- outpack_root$new(path)
    cli::cli_alert_success("Created orderly root at '{path}'")
  }

  if (!has_orderly_config) {
    writeLines(empty_config_contents(), file.path(path, "orderly_config.yml"))
  }

  root <- root_open(path, locate = FALSE, require_orderly = TRUE,
                    call = environment())
  invisible(root$path)
}


empty_config_contents <- function() {
  'minimum_orderly_version: "1.99.0"'
}


## There's quite a few scenarios here:
##
## * find an existing outpack root that is not an orderly root
##   - sometimes require that we upgrade (e.g., run)
##   - sometimes just work with it (e.g., search, extract, copy)
## * find an existing orderly root that is not an outpack root
##   - always error, indicate what to do
##
## * also check that the outpack and orderly path are compatibible
##   (this is actually quite hard to get right, but should be done
##   before anything is created I think)
root_open <- function(path, locate, require_orderly = FALSE, call = NULL) {
  if (is.null(call)) {
    call <- environment()
  }
  if (inherits(path, "outpack_root")) {
    if (!require_orderly || !is.null(path$config$orderly)) {
      return(path)
    }
    ## This is going to error, but the error later will do.
    path <- path$path
    locate <- FALSE
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
    if (has_outpack && has_orderly && path_outpack != path_orderly) {
      if (fs::path_has_parent(path_outpack, path_orderly)) {
        order <- c(orderly = path_orderly, outpack = path_outpack)
      } else {
        order <- c(outpack = path_outpack, orderly = path_orderly)
      }
      cli::cli_abort(c(
        "Found incorrectly nested orderly and outpack directories",
        i = "{names(order)[[1]]} was found at '{order[[1]]}'",
        i = "{names(order)[[2]]} was found at '{order[[2]]}'",
        x = paste("{names(order)[[2]]} is nested within {names(order)[[1]]}",
                  "at {fs::path_rel(order[[2]], order[[1]])}"),
        i = "How did you even do this? Please let us know!"))
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
    root$config$orderly <- orderly_config_read(root$path, call)
  } else if (require_orderly) {
    cli::cli_abort(
      c("Did not find 'orderly_config.yml' in '{path}'",
        x = paste("Your directory has an '.outpack/' path, so is a valid",
                  "outpack root, but does not contain 'orderly_config.yml' so",
                  "cannot be used as an orderly root"),
        i = 'Please run orderly2::orderly_init("{path}") to initialise',
        i = "See ?orderly_init for more arguments to this function"))
  }

  root_check_git(root, call)

  root
}


## This is pretty unpleasant, but does the trick.
root_validate_same_configuration <- function(args, config, root, call) {
  argmap <- list(
    path_archive = c("core", "path_archive"),
    use_file_store = c("core", "use_file_store"),
    require_complete_tree = c("core", "require_complete_tree"))
  check <- intersect(names(argmap), names(args))
  if (length(check) > 0) {
    cmp <- lapply(check, function(nm) {
      current <- root$config[[argmap[[nm]]]]
      given <- config[[argmap[[nm]]]]
      changed <- !identical(current, given)
      list(changed = changed, name = nm, current = current, given = given)
    })
    err <- vlapply(cmp, "[[", "changed")
    if (any(err)) {
      err_str <- sprintf(
        "%s: was '%s' but was given '%s'",
        vcapply(cmp[err], "[[", "name"),
        vcapply(cmp[err], function(x) as.character(x$current)),
        vcapply(cmp[err], function(x) as.character(x$given)))
      cli::cli_abort(
        c("Trying to change configuration when re-initialising",
          set_names(err_str, rep("x", length(err_str))),
          i = "Use 'orderly2::orderly_config_set()' to change configuration"),
        call = call)
    }
  }
}


root_check_git <- function(root, call) {
  path_ok <- file.path(root$path, ".outpack", "r", "git_ok")
  if (file.exists(path_ok)) {
    return()
  }
  git_root <- git_open(root$path)
  if (is.null(git_root)) {
    return()
  }

  files <- gert::git_ls(git_root)$path
  files_full <- file.path(gert::git_info(git_root)$path, files)
  special <- c(".outpack", "draft", root$config$core$path_archive)
  err <- vapply(special, function(p) {
    fs::path_has_parent(files_full, file.path(root$path, p))
  }, logical(length(files)))
  dim(err) <- c(length(files), length(special))

  if (any(err)) {
    files_err <- files[rowSums(err) > 0]
    types_err <- paste0(special[colSums(err) > 0], "/")
    url <- "https://mrc-ide.github.io/orderly2/articles/troubleshooting.html"
    warn_only <- getOption("orderly_git_error_is_warning", FALSE)
    msg <- c("Detected {length(files_err)} outpack file{?s} committed to git",
             x = "Detected files were found in {squote(types_err)}",
             i = "For tips on resolving this, please see {.url {url}}")
    if (warn_only) {
      cli::cli_warn(msg, call = call, .frequency = "once",
                    .frequency_id = paste0("orderly_git_warning-", root$path))
    } else {
      hint_warn_only <- paste(
        "To turn this into a warning and continue anyway",
        "set the option 'orderly_git_error_is_warning' to TRUE",
        "by running options(orderly_git_error_is_warning = TRUE)")
      cli::cli_abort(c(msg, i = hint_warn_only), call = call)
    }
  }

  do_orderly_gitignore_update("(root)", root)

  fs::dir_create(dirname(path_ok))
  fs::file_create(path_ok)
}
