##' Initialise an empty orderly repository. The destination must be an
##' empty directory or not exist.  The resulting orderly archive will
##' be "unconfigured" and you may want to edit the
##' `orderly_config.yml` file to add any required configuration.  Note
##' that compared with orderly 1.x.x there are relatively few
##' configuration options available (at present only a single option
##' that does not require a plugin)
##'
##' @title Create an empty orderly repository
##'
##' @param path Path to use
##'
##' @param outpack_args Optionally, a named list of arguments to
##'   forward to [outpack::outpack_init].
##'
##' @return The path to the created archive
##' @export
##' tmp <- tempfile()
##' orderly3::orderly_init(tmp)
##' fs::dir_ls(tmp)
orderly_init <- function(path, outpack_args = NULL) {
  if (file.exists(path)) {
    if (!is_directory(path) || length(fs::dir_ls(path, all = TRUE)) > 0) {
      stop("'path', if it already exists, must be an empty directory")
    }
  } else {
    fs::dir_create(path)
  }
  root <- orderly_outpack_init(path, outpack_args)
  write_orderly_config(path)
  write_gitignore(path, root$config$core$path_archive)
  orderly_root(path, locate = FALSE)
}


write_orderly_config <- function(path) {
  fs::file_copy(system_file("orderly_config.yml", package = "orderly3"),
                file.path(path, "orderly_config.yml"))
}


write_gitignore <- function(path, path_archive) {
  txt <- readLines(system_file("gitignore", package = "orderly3"))
  txt <- gsub("ARCHIVE", path_archive %||% "archive", txt, fixed = TRUE)
  writeLines(txt, file.path(path, ".gitignore"))
}


orderly_outpack_init <- function(path, outpack_args) {
  if (length(outpack_args) > 0) {
    assert_named(outpack_args, unique = TRUE)
  }
  ## TODO: outpack should be taking path here, not root. We'll make
  ## this forward compatible by filtering both options.
  valid <- setdiff(names(formals(outpack::outpack_init)), c("path", "root"))
  err <- setdiff(names(outpack_args), valid)
  if (length(err) > 0) {
    stop(sprintf(
      "Unknown argument in 'outpack_args': %s, see ?outpack_args for details",
      paste(squote(err), collapse = ", ")))
  }
  do.call(outpack::outpack_init, c(list(path), outpack_args))
}
