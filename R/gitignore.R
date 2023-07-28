##' Update a gitignore, which is useful to prevent accidentally
##' comitting files to source control that are generated. This
##' includes artefacts, shared resources and dependencies (within a
##' report directory) or at the global level all he contents of the
##' `.outpack` directory,
##'
##' @title Update a gitignore file
##'
##' @param name The name of the gitignore file to update, or the
##'   string "(root)"
##'
##' @param prompt Conditions under which you will be prompted before
##'   writing anything to a file (in a non-interactive session, we
##'   error instead). Options here are
##'
##'   * `if_manually_created`": (the default), which prompts if a
##'     .gitignore file exists with content that does not include
##'      special orderly2 markers
##'   * `if_new_file`: if we would create a new file
##'   * `always`: on any change to contents
##'   * `never`: never prompt, just write the changes
##'
##' @inheritParams orderly_run
##'
##' @return Nothing, called for its side effects
##' @export
orderly_gitignore_update <- function(name, prompt = "if_manually_created",
                                     root = NULL, locate = TRUE) {
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

  ## TODO: this should also check that things are not added to git and
  ## point at advice for fixing this, but that might also be best
  ## elsewhere...

  if (gitignore_update_file(root$path, path, value, prompt)) {
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


## Value for prompt could be if markers are not added, never, always
## (or if the file does not exist)
gitignore_update_contents <- function(content_old, value) {
  if (!any(gitignore_markers %in% content_old)) {
    if (length(content_old) > 0) {
      content_old <- c(content_old, "")
    }
    return(c(content_old, gitignore_markers[1:2], value, gitignore_markers[3]))
  }

  i <- lapply(gitignore_markers, function(x) match(x, content_old))
  if (!all(lengths(i) == 1)) {
    stop("nope 1")
  }
  i <- list_to_numeric(i)
  ok <- i[[2]] == i[[1]] + 1 && i[[3]] > i[[2]]
  if (!ok) {
    stop("nope 2")
  }

  c(content_old[seq_len(i[[1]] - 1)],
    gitignore_markers[1:2],
    value,
    content_old[seq(i[[3]], length(content_old))])
}


gitignore_update_file <- function(root, path, value, prompt) {
  path_full <- file.path(root, path)
  gitignore_exists <- file.exists(path_full)
  content_old <- if (gitignore_exists) readLines(path_full) else character()
  content_new <- gitignore_update_contents(content_old, value)
  if (identical(content_old, content_new)) {
    return(FALSE)
  }

  prompt <- switch(
    prompt,
    if_manually_created = gitignore_exists &&
      !any(gitignore_markers %in% content_old),
    if_new_file = !gitignore_exists,
    always = TRUE,
    never = FALSE,
    stop("Invalid value for 'prompt'"))

  if (prompt && !prompt_update(content_old, content_new, path, root)) {
    return(FALSE)
  }

  writeLines(content_new, path_full)
  TRUE
}


prompt_update <- function(old, new, path, root) {
  cli::cli_alert("I am going to make changes to the file '{path}'")
  cli::cli_alert_info("(within orderly root '{root}')")
  cli::cli_alert_info("Proposed changes:")
  str <- suppressWarnings(format(cli::diff_chr(old, new)))
  message(paste0(str, "\n", collapse = ""))
  continue <- prompt_ask_yes_no("OK to apply these changes?")
  if (!continue) {
    message("Not making any changes to the file")
  }
  continue
}
