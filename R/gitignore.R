##' Update a gitignore, which is useful to prevent accidentally
##' committing files to source control that are generated. This
##' includes artefacts, shared resources and dependencies (within a
##' report directory) or at the global level all the contents of the
##' `.outpack` directory, the draft folder and the archive directory.
##'
##' If this function fails with a message `Can't edit '.gitignore',
##' markers are corrupted`, then look for the special markers within
##' the `.gitignore` file.  It should look like
##'
##' ```
##' # ---VVV--- added by orderly ---VVV----------------
##' # Don't manually edit content between these markers
##' ... patterns
##' # ---^^^--- added by orderly ---^^^----------------
##' ```
##'
##' We can't edit the file if:
##'
##' * any of these lines appears more than once in the file
##' * there is anything between the first two lines
##' * they are not in this order
##'
##' If you get the error message, search and remove these lines and
##' rerun.
##'
##' @title Update a gitignore file
##'
##' @param name The name of the gitignore file to update, or the
##'   string "(root)"
##'
##' @inheritParams orderly_run
##'
##' @return Nothing, called for its side effects
##' @export
orderly_gitignore_update <- function(name, root = NULL, locate = TRUE) {
  root <- root_open(root, locate, require_orderly = TRUE, call = environment())
  assert_scalar_character(name)

  if (name == "(root)") {
    path <- ".gitignore"
    value <- gitignore_content_root(root)
  } else {
    validate_orderly_directory(name, root, environment())
    path <- file.path("src", name, ".gitignore")
    value <- gitignore_content_src(name, root)
  }

  ## TODO (mrc-4447): check that none of these are _already_ in git,
  ## and offer help towards fixing this.

  if (gitignore_update_file(root$path, path, value)) {
    cli::cli_alert_success("Wrote '{path}'")
  }
  invisible(TRUE)
}


gitignore_content_root <- function(root) {
  c(".outpack",
    "orderly_envir.yml",
    "draft",
    root$config$core$path_archive)
}


gitignore_content_src <- function(name, root) {
  dat <- orderly_read_r(file.path(root$path, "src", name, "orderly.R"))

  ignore_deps <- unlist(lapply(dat$dependency, function(x) names(x$files)))
  ignore_artefacts <- unlist(lapply(dat$artefacts, "[[", "files"))
  ignore_shared <- names(dat$shared_resource)
  ignore <- unique(c(ignore_deps, ignore_artefacts, ignore_shared))
  ignore <- setdiff(ignore, dat$resources)

  ## We're not clever enough to work with loops over dependencies yet,
  ## but if we were we'd want to swap out ("\\$\\{.*?\\}" for "*"

  ignore
}


gitignore_markers <- c(
  "# ---VVV--- added by orderly ---VVV----------------",
  "# Don't manually edit content between these markers",
  "# ---^^^--- added by orderly ---^^^----------------")


gitignore_update_contents <- function(content_old, value, path, root) {
  if (!any(gitignore_markers %in% content_old)) {
    if (length(content_old) > 0) {
      content_old <- c(content_old, "")
    }
    return(c(content_old, gitignore_markers[1:2], value, gitignore_markers[3]))
  }

  i <- lapply(gitignore_markers, function(x) which(x == content_old))
  err <- !all(lengths(i) == 1)
  if (!err) {
    i <- list_to_numeric(i)
    err <- i[[2]] != i[[1]] + 1 || i[[3]] <= i[[2]]
  }
  if (err) {
    cli::cli_abort(c(
      "Can't edit '{path}', markers are corrupted",
      i = "(within orderly root '{root}')",
      i = "Please see ?orderly_gitignore_update for more details"))
  }

  c(content_old[seq_len(i[[1]] - 1)],
    gitignore_markers[1:2],
    value,
    content_old[seq(i[[3]], length(content_old))])
}


gitignore_update_file <- function(root, path, value) {
  path_full <- file.path(root, path)
  gitignore_exists <- file.exists(path_full)
  content_old <- if (gitignore_exists) readLines(path_full) else character()
  content_new <- gitignore_update_contents(content_old, value, path, root)
  if (identical(content_old, content_new)) {
    return(FALSE)
  }
  writeLines(content_new, path_full)
  TRUE
}