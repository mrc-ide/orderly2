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


##' Create a new empty report.
##'
##' @title Create a new report
##'
##' @param name The name of the report
##'
##' @param template The template to use. The only acceptable values
##'   for now are `NULL` (uses the built-in default) and `FALSE` which
##'   suppresses any default content.  We may support customisable
##'   templates in future - let us know if this would be useful.
##'
##' @param force Create an `orderly.R` file within an existing
##'   directory `src/<name>`; this may be useful if you have already
##'   created the directory and some files first but want help
##'   creating the orderly file.
##'
##' @inheritParams orderly_run
##'
##' @return Nothing, called for its side effects only
##' @export
orderly_new <- function(name, template = NULL, force = FALSE,
                        root = NULL, locate = TRUE) {
  root <- root_open(root, locate, require_orderly = TRUE, call = environment())
  dest <- file.path(root$path, "src", name)

  if (file.exists(file.path(dest, "orderly.R"))) {
    cli::cli_abort("'src/{name}/orderly.R' already exists")
  }
  if (file.exists(dest) && !fs::is_dir(dest)) {
    cli::cli_abort(
      c("'src/{name}' already exists, but is not a directory",
        i = "This file really should not be here, you might need to tidy up"))
  }
  if (length(dir(dest, all.files = TRUE, no.. = TRUE)) > 0 && !force) {
    cli::cli_abort(
      c("'src/{name}/' already exists and contains files",
        i = paste("If you want to add an orderly.R to this directory,",
                  "rerun {.code orderly_new()} with {.code force = TRUE}")))
  }

  if (is.null(template)) {
    contents <- readLines(orderly2_file("template/default.R"))
  } else if (isFALSE(template)) {
    contents <- character()
  } else {
    cli::cli_abort("'template' must be 'NULL' or 'FALSE' for now")
  }

  fs::dir_create(dest)
  writeLines(contents, file.path(dest, "orderly.R"))
  cli::cli_alert_success("Created 'src/{name}/orderly.R'")
}
