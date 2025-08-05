##' List source reports - that is, directories within `src/` that
##' look suitable for running with orderly; these will be directories
##' that contain an entrypoint file - a `.R` file with the same name
##' as the directory (e.g., `src/data/data.R` corresponds to `data`).
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
##' path <- orderly_example()
##' orderly_list_src(root = path)
orderly_list_src <- function(root = NULL) {
  root_path <- orderly_src_root(root)
  if (!file.exists(file.path(root_path, "src"))) {
    return(character())
  }
  pos <- fs::dir_ls(file.path(root_path, "src"), type = "directory")
  entrypoint <- vcapply(pos, function(path) {
    find_entrypoint_filename(path, suppress_zero_files = TRUE,
                             suppress_multiple_files = TRUE)
  })
  basename(pos)[!is.na(entrypoint)]
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
##' @param force Create an orderly file - `<name>.R` within an existing
##'   directory `src/<name>`; this may be useful if you have already
##'   created the directory and some files first but want help
##'   creating the orderly file.
##'
##' @inheritParams orderly_run
##'
##' @return Nothing, called for its side effects only
##' @export
##' @examples
##' path <- withr::local_tempdir()
##'
##' # Initialise a new repository, setting an option:
##' orderly_init(path)
##'
##' # Create a new report 'myreport' in this root:
##' orderly_new("myreport", root = path)
##'
##' # We now see:
##' fs::dir_tree(path, all = TRUE)
##'
##' # By default, the new path will contain some hints, you can
##' # customise this by writing a template
##' cli::cli_code(readLines(file.path(path, "src", "myreport", "myreport.R")))
orderly_new <- function(name, template = NULL, force = FALSE, root = NULL) {
  root <- root_open(root, require_orderly = TRUE)
  dest <- file.path(root$path, "src", name)
  existing_entrypoint_filename <- find_entrypoint_filename(
    dest, suppress_zero_files = TRUE
  )
  new_report_filename <- sprintf("%s.R", name)

  if (!is.na(existing_entrypoint_filename)) {
    cli::cli_abort("'src/{name}/{existing_entrypoint_filename}' already exists")
  }
  if (file.exists(dest) && !fs::is_dir(dest)) {
    cli::cli_abort(
      c("'src/{name}' already exists, but is not a directory",
        i = "This file really should not be here, you might need to tidy up"))
  }
  if (length(dir(dest, all.files = TRUE, no.. = TRUE)) > 0 && !force) {
    cli::cli_abort(
      c("'src/{name}/' already exists and contains files",
        i = paste("If you want to add a {new_report_filename} to this",
                  "directory, rerun {.code orderly_new()} with",
                  "{.code force = TRUE}")))
  }

  if (is.null(template)) {
    contents <- readLines(orderly2_file("template/default.R"))
  } else if (isFALSE(template)) {
    contents <- character()
  } else {
    cli::cli_abort("'template' must be 'NULL' or 'FALSE' for now")
  }

  fs::dir_create(dest)
  writeLines(contents, file.path(dest, new_report_filename))
  cli::cli_alert_success("Created 'src/{name}/{new_report_filename}'")
}
