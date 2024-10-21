is_plausible_orderly_report <- function(path) {
  path_split <- fs::path_split(path)[[1]]

  length(path_split) > 2 &&
    path_split[[length(path_split) - 1]] == "src" &&
    file.exists(file.path(path, "../..", "orderly_config.yml"))
}

rstudio_get_current_active_editor_path <- function() {
  # Avoid looking at the RStudio state when running tests inside of it.
  if (!is_testing() && rstudioapi::isAvailable()) {
    rstudioapi::getSourceEditorContext()$path
  } else {
    NULL
  }
}

## This is something that we might improve over time - it will likely
## be useful to have some sort of "register interactive" function
## which we could then just look up.
##
## I am not sure if we also want to allow working interactively from a
## draft directory too.
detect_orderly_interactive_path <- function(
  path = getwd(),
  editor_path = rstudio_get_current_active_editor_path()) {
  is_valid <- is_plausible_orderly_report(path)
  suggested_wd <- NULL
  if (!is.null(editor_path)) {
    dir <- fs::path_dir(editor_path)
    if (!paths_are_identical(dir, path) && is_plausible_orderly_report(dir)) {
      suggested_wd <- dir
    }
  }

  if (!is_plausible_orderly_report(path)) {
    msg <- c("Working directory {.path {path}} is not a valid orderly report.")
    if (!is.null(suggested_wd)) {
      cli::cli_abort(c(msg, i = paste(
        "Use {.code setwd({.str {suggested_wd}})} to set the working",
        "directory to the report currently open in RStudio.")))
    } else {
      cli::cli_abort(msg)
    }
  }

  if (!is.null(suggested_wd)) {
    # For some reason, cli_warn has a different behaviour than cli_abort and
    # doesn't make individual bullet points available in the condition object.
    # The following mimicks cli_abort more closely, making testing easier.
    # https://github.com/r-lib/cli/issues/666
    msg <- c(
      cli::format_inline(paste(
        "Working directory {.path {path}} does not match the report currently",
        "open in RStudio.")),
      i = cli::format_inline(paste(
        "Use {.code setwd({.str {suggested_wd}})}",
        "to switch working directories.")))
    rlang::warn(msg, use_cli_format = TRUE)
  }
  as.character(fs::path_norm(file.path(path, "../..")))
}


.interactive <- new.env(parent = emptyenv())

##' Set search options for interactive use of orderly; see
##' [orderly2::orderly_dependency] and [orderly2::orderly_run] for
##' details. This may be either an [orderly2::orderly_search_options]
##' object, or a list that will be coerced into one at the point of
##' use (or `NULL`). This applies only for the current session, but
##' applies to all interactive uses of orderly functions that might
##' have received a copy of `search_options` via
##' [orderly2::orderly_run]
##'
##' @title Set search options for interactive use
##'
##' @inheritParams orderly_search_options
##'
##' @return Nothing, called for its side effects
##' @export
orderly_interactive_set_search_options <- function(location = NULL,
                                                   allow_remote = NULL,
                                                   pull_metadata = FALSE) {
  options <- build_search_options(location = location,
                                  allow_remote = allow_remote,
                                  pull_metadata = pull_metadata)
  .interactive$search_options <- options
}


get_parameter_interactive <- function(name, call = NULL) {
  value <- readline(sprintf("%s > ", name))
  if (!nzchar(value)) {
    cli::cli_abort("Expected a value for parameter '{name}'", call = call)
  }
  parsed <- tryCatch(parse(text = value)[[1L]], error = identity)
  explain_string <- paste(
    "If entering a string, you must use quotes, this helps",
    'disambiguate numbers and booleans. For example, TRUE and "TRUE" will',
    "parse as a boolean and a string, respectively")
  if (inherits(parsed, "error")) {
    cli::cli_abort(
      c("Failed to parse value for parameter '{name}'",
        x = "Was given: {value}",
        i = explain_string),
      parent = parsed, call = call)
  }
  if (is.name(parsed)) {
    cli::cli_abort(
      c("Invalid input for parameter '{name}'",
        i = 'Did you mean: {.strong "{as.character(parsed)}"} (in quotes)?',
        i = explain_string),
      call = call)
  }
  if (!is_simple_scalar_atomic(parsed)) {
    cli::cli_abort(
      c("Invalid input for parameter '{name}': {value}",
        i = "Must be a simple boolean, number or string"),
      call = call)
  }
  parsed[[1]]
}


get_missing_parameters_interactive <- function(required, envir, call = NULL) {
  msg <- setdiff(required, names(envir))
  if (length(msg) == 0) {
    return()
  }
  if (getOption("orderly_interactive_parameters_missing_error", FALSE)) {
    cli::cli_abort(
      c("Missing parameters: {squote(msg)}",
        i = paste("Erroring because option",
                  "'orderly_interactive_parameters_missing_error'",
                  "is 'TRUE'")),
      call = call)
  }
  n <- length(msg)
  cli::cli_alert(
    "Please enter values for {n} missing {cli::qty(n)}parameter{?s}:")
  found <- list()
  for (nm in msg) {
    found[[nm]] <- get_parameter_interactive(nm, call)
  }
  list2env(found, envir)
}
