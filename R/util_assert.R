assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("'%s' must be a scalar", name), call. = FALSE)
  }
}

assert_character <- function(x, name = deparse(substitute(x)), call = NULL) {
  if (!is.character(x)) {
    cli::cli_abort("'{name}' must be character", call = call)
  }
}

assert_logical <- function(x, name = deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("'%s' must be logical", name), call. = FALSE)
  }
  invisible(x)
}

assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
}

assert_scalar_logical <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_logical(x, name)
}

assert_simple_scalar_atomic <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  if (!is_simple_atomic(x)) {
    stop(sprintf("'%s' must be atomic (string, numeric, logical)", name))
  }
  invisible(x)
}

assert_named <- function(x, unique = FALSE, name = deparse(substitute(x))) {
  if (is.null(names(x))) {
    stop(sprintf("'%s' must be named", name), call. = FALSE)
  }
  if (unique && any(duplicated(names(x)))) {
    stop(sprintf("'%s' must have unique names", name), call. = FALSE)
  }
}

assert_is <- function(x, what, name = deparse(substitute(x))) {
  if (!inherits(x, what)) {
    stop(sprintf("'%s' must be a %s", name,
                 paste(what, collapse = " / ")), call. = FALSE)
  }
}

assert_file_exists <- function(files, name = "File", call = NULL) {
  err <- !file.exists(files)
  if (any(err)) {
    n <- cli::qty(sum(err))
    cli::cli_abort(
      "{name}{n}{?s} {?does/do} not exist: {collapseq(files[err])}",
      call = call)
  }
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

assert_is_directory <- function(path, name = "Directory", call = NULL) {
  assert_scalar_character(path)
  assert_file_exists(path, name = name, call = call)
  if (!is_directory(path)) {
    cli::cli_abort("Path exists but is not a directory: {path}",
                   call = call)
  }
}

assert_relative_path <- function(files, name, workdir, call = NULL) {
  err <- fs::is_absolute_path(files)
  if (any(err)) {
    n <- cli::qty(sum(err))
    cli::cli_abort(
      c("{name}{n}{?s} must be {?a/} relative path{?s}",
        set_names(files[err], "x"),
        i = "Path was relative to directory '{workdir}'"),
      call = call)
  }

  err <- vlapply(fs::path_split(files), function(x) any(x == ".."))
  if (any(err)) {
    n <- cli::qty(sum(err))
    cli::cli_abort(
      c("{name}{n}{?s} must not contain '..' (parent directory) components",
        set_names(files[err], "x"),
        i = "Path was relative to directory '{workdir}'"),
      call = call)
  }
}


assert_directory_does_not_exist <- function(x, name = "Directory") {
  ok <- !fs::dir_exists(x)
  if (!all(ok)) {
    stop(sprintf("%s already exists: %s",
                 name, paste(squote(x[!ok]), collapse = ", ")))
  }
  invisible(x)
}


match_value <- function(arg, choices, name = deparse(substitute(arg))) {
  assert_scalar_character(arg)
  if (!(arg %in% choices)) {
    stop(sprintf("%s must be one of %s",
                 name, paste(squote(choices), collapse = ", ")),
         call. = FALSE)
  }
  arg
}
