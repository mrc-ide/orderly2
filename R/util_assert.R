assert_scalar <- function(x, name = deparse(substitute(x)), arg = name,
                          call = NULL) {
  if (length(x) != 1) {
    cli::cli_abort(c("'{name}' must be a scalar",
                     i = "{name} has length {length(x)}"),
                   call = call, arg = arg)
  }
}

assert_character <- function(x, name = deparse(substitute(x)),
                             arg = name, call = NULL) {
  if (!is.character(x)) {
    cli::cli_abort("'{name}' must be character", call = call, arg = arg)
  }
}

assert_logical <- function(x, name = deparse(substitute(x)), arg = name,
                           call = NULL) {
  if (!is.logical(x)) {
    cli::cli_abort("'{name}' must be logical", call = call, arg = arg)
  }
  invisible(x)
}

assert_scalar_character <- function(x, name = deparse(substitute(x)),
                                    arg = name, call = NULL) {
  assert_scalar(x, name, arg = arg, call = call)
  assert_character(x, name, arg = arg, call = call)
}

assert_scalar_logical <- function(x, name = deparse(substitute(x)),
                                  arg = name, call = NULL) {
  assert_scalar(x, name, arg = arg, call = call)
  assert_logical(x, name, arg = arg, call = call)
}

assert_simple_scalar_atomic <- function(x, name = deparse(substitute(x)),
                                        arg = name, call = NULL) {
  assert_scalar(x, name)
  if (!is_simple_atomic(x)) {
    cli::cli_abort("'{name}' must be atomic (string, numeric, logical)",
                   call = call, arg = arg)
  }
  invisible(x)
}

assert_named <- function(x, unique = FALSE, name = deparse(substitute(x)),
                         arg = name, call = NULL) {
  ## TODO: we get bad quotes here from static_orderly_parameters
  if (is.null(names(x))) {
    cli::cli_abort("'{name}' must be named", call = call, arg = arg)
  }
  if (unique && any(duplicated(names(x)))) {
    dups <- unique(names(x)[duplicated(names(x))])
    cli::cli_abort(
      c("'{name}' must have unique names",
        i = "Found {length(dups)} duplicate{?s}: {collapseq(dups)}"),
      call = call, arg = arg)
  }
}

assert_is <- function(x, what, name = deparse(substitute(x)),
                      arg = name, call = NULL) {
  if (!inherits(x, what)) {
    cli::cli_abort(
      c("'{name}' must be a {paste(what, collapse = ' / ')}",
        "{name} was a {paste(class(x), collapse = ' / ')}"),
      call = call, arg = arg)
  }
}

assert_file_exists <- function(files, name = "File", call = NULL, arg = NULL) {
  err <- !file.exists(files)
  if (any(err)) {
    n <- cli::qty(sum(err))
    cli::cli_abort(
      "{name}{n}{?s} {?does/do} not exist: {collapseq(files[err])}",
      call = call, arg = arg)
  }
}


find_entrypoint_filename <- function(src, suppress_zero_files = FALSE,
                                     suppress_multiple_files = FALSE) {
  reportname <- basename(src)
  names <- c(sprintf("%s.R", reportname), "orderly.R")
  files_exist <- file.exists(file.path(src, names))
  n_found <- sum(files_exist)
  if (n_found > 1 && !suppress_multiple_files) {
    cli::cli_abort(
      paste("Please only create {names[[1]]} file, orderly.R",
            "has been deprecated")
    )
  }
  if (n_found == 0 && !suppress_zero_files) {
    cli::cli_abort(
      "Please create {names[[1]]} file"
    )
  }
  if (files_exist[[2]]) {
    cli::cli_warn(
      paste("Naming convention orderly.R will be deprecated",
            "soon. Please change orderly file name to",
            "<reportname>.R"),
      .frequency = "regularly",
      .frequency_id = "deprecate_orderly_file_name"
    )
  }
  if (n_found == 1) names[files_exist] else NA_character_
}


assert_file_exists_relative <- function(files, workdir, name, call = NULL) {
  assert_relative_path(files, name, workdir, call)

  assert_character(files, call = call)
  err <- !file_exists(files, workdir = workdir)
  if (any(err)) {
    n <- cli::qty(sum(err))
    cli::cli_abort(
      c("{name}{n}{?s} {?does/do} not exist: {collapseq(files[err])}",
        i = "Looked within directory '{workdir}'"),
      call = call)
  }

  files_canonical <- file_canonical_case(files, workdir)
  err <- is.na(files_canonical) | fs::path(files) != files_canonical
  if (any(err)) {
    i <- err & !is.na(files_canonical)
    hint_case <- sprintf("For '%s', did you mean '%s'?",
                         files[i], files_canonical[i])
    n <- cli::qty(sum(err))
    cli::cli_abort(
      c("{name}{n}{?s} {?does/do} not exist: {collapseq(files[err])}",
        set_names(hint_case, "i"),
        i = paste("If you don't use the canonical case for a file, your code",
                  "is not portable across different platforms"),
        i = "Looked within directory '{workdir}'"),
      call = call)
  }
}

assert_is_directory <- function(path, name = "Directory", call = NULL,
                                arg = NULL) {
  assert_scalar_character(path, arg = arg, call = call)
  assert_file_exists(path, name = name, arg = arg, call = call)
  if (!is_directory(path)) {
    cli::cli_abort("Path exists but is not a directory: {path}",
                   call = call, arg = arg)
  }
}

assert_relative_path <- function(files, name, workdir, call = NULL,
                                 arg = NULL) {
  err <- fs::is_absolute_path(files)
  if (any(err)) {
    n <- cli::qty(sum(err))
    cli::cli_abort(
      c("{name}{n}{?s} must be {?a/} relative path{?s}",
        set_names(files[err], "x"),
        i = "Path was relative to directory '{workdir}'"),
      call = call, arg = arg)
  }

  err <- vlapply(fs::path_split(files), function(x) any(x == ".."))
  if (any(err)) {
    n <- cli::qty(sum(err))
    cli::cli_abort(
      c("{name}{n}{?s} must not contain '..' (parent directory) components",
        set_names(files[err], "x"),
        i = "Path was relative to directory '{workdir}'"),
      call = call, arg = arg)
  }
}


assert_directory_does_not_exist <- function(x, name = "Directory", arg = NULL,
                                            call = NULL) {
  ok <- !fs::dir_exists(x)
  if (!all(ok)) {
    cli::cli_abort("{name} already exists: {collapseq(x[!ok])}",
                   call = call, arg = arg)
  }
  invisible(x)
}


match_value <- function(x, choices, name = deparse(substitute(x)),
                        arg = name, call = NULL) {
  assert_scalar_character(x, call = call, arg = arg)
  if (!(x %in% choices)) {
    cli::cli_abort(
      c("'{name}' must be one of {collapseq(choices)}",
        i = "Instead we were given '{x}'"),
       call = call, arg = arg)
  }
  x
}
