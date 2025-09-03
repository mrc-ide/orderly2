##' Migrate source code from orderly2 (version 1.99.81 and previous)
##' to refer to orderly (version 1.99.82 and subsequent).  This is a
##' one-off and will not exist in the package for long, get it while
##' it's hot.  This function does a very simple-minded replacement of
##' `orderly2` with `orderly` in R files (extension `.R` or `.r`),
##' markdown (`.md`), R markdown (`.Rmd`) and quarto (`.qmd`).  It
##' requires a clean git status before it is run, and will be best to
##' run against a fresh clone of a repository.  After running, review
##' changes (if any) with `git diff` and then commit.
##'
##' @title Migrate orderly2 sources
##'
##' @param path Path to the repo.  We will not change anything here
##'   unless the path is under source control, and unless the git
##'   status is "up to date" (i.e., no local unsaved modifications or
##'   untracked files).  It is recommended to run this against a fresh
##'   clone.
##'
##' @param dry_run Logical, indicating if no changes would be made,
##'   but just print information about the changes that would be made.
##'
##' @return Primarily called for side effects, but returns (invisibly)
##'   `TRUE` if any changes were made, `FALSE` otherwise.
##'
##' @export
orderly_migrate_source_from_orderly2 <- function(path = ".", dry_run = FALSE) {
  status <- tryCatch(
    gert::git_status(repo = path),
    error = function(e) {
      cli::cli_abort(
        "Not migrating '{path}' as it does not appear to be version controlled")
    })
  if (nrow(status) != 0 && !dry_run) {
    cli::cli_abort(
      c("Not migrating '{path}' as 'git status' is not clean",
        i = "Try running this in a fresh clone"))
  }
  root <- gert::git_info(repo = path)$path
  files <- dir(root, pattern = "\\.(R|Rmd|qmd|md)$",
               recursive = TRUE, ignore.case = TRUE)
  cli::cli_alert_info("Checking {length(files)} file{?s} in '{root}'")
  changed <- 0
  for (i in seq_along(files)) {
    changed <- changed + orderly_migrate_file(root, files[[i]], dry_run)
  }
  changed <- changed +
    update_minimum_orderly_version(
      file.path(path, "orderly_config.yml"),
      ORDERLY_MINIMUM_VERSION,
      dry_run)
  any_changed <- changed > 0
  if (!any_changed) {
    cli::cli_alert_success("Nothing to change!")
  } else if (dry_run) {
    cli::cli_alert_info("Would change {sum(changed)} file{?s}")
  } else {
    cli::cli_alert_success("Changed {changed} file{?s}")
    cli::cli_alert_info("Please add and commit these to git")
  }
  invisible(any_changed)
}


orderly_migrate_file <- function(path, file, dry_run) {
  filename <- file.path(path, file)
  prev <- txt <- readLines(filename, warn = FALSE)
  txt <- sub("orderly2::", "orderly::", txt, fixed = TRUE)
  txt <- sub("library(orderly2)", "library(orderly)", txt, fixed = TRUE)
  n <- sum(txt != prev)
  changed <- n > 0
  if (changed) {
    if (dry_run) {
      cli::cli_alert_info("Would update {n} line{?s} in {file}")
    } else {
      cli::cli_alert_info("Updated {n} line{?s} in {file}")
      writeLines(txt, filename)
    }
  }
  changed
}


update_minimum_orderly_version <- function(filename, version, dry_run) {
  assert_file_exists(filename)
  ## Everything about yaml is terrible.  We would like to edit the
  ## value within the yaml, but we can't easily roundtrip the
  ## contents.  So instead we'll edit the strings that it contains,
  ## which is disgusting.  However, the majority of these that we will
  ## hit are written by us, and are very simple, so in practice this
  ## should be reasonable.
  txt <- readLines(filename, warn = FALSE)
  pattern <- "^minimum_orderly_version\\s*:\\s+(.*?)\\s*$"
  i <- grep(pattern, txt)
  if (length(i) == 0) {
    cli::cli_abort(
      c("Failed to find key 'minimum_orderly_version' in orderly config",
        i = "Looked in '{filename}'",
        i = "Please edit this file yourself"))
  }
  if (length(i) > 1) {
    cli::cli_abort(
      c("Found more than one key 'minimum_orderly_version' in orderly config",
        i = "Looked in '{filename}'",
        x = "This is probably not valid yaml, does this even work for you?"))
  }
  existing <- gsub("[\"']", "", sub(pattern, "\\1", txt[[i]]))
  if (numeric_version(existing) >= version) {
    cli::cli_alert_success(
      "Minimum orderly version already at {existing}")
    return(FALSE)
  }

  txt[[i]] <- sprintf('minimum_orderly_version: "%s"', version)
  if (dry_run) {
    cli::cli_alert_info(
      "Would update minimum orderly version from {existing} to {version}")
  } else {
    writeLines(txt, filename)
    cli::cli_alert_success(
      "Updated minimum orderly version from {existing} to {version}")
  }
  TRUE
}
