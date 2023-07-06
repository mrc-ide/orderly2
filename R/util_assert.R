assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("'%s' must be a scalar", name), call. = FALSE)
  }
}

assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("'%s' must be character", name), call. = FALSE)
  }
}

assert_numeric <- function(x, name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    stop(sprintf("'%s' must be numeric", name), call. = FALSE)
  }
  invisible(x)
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

assert_scalar_numeric <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_numeric(x, name)
}


assert_scalar_logical <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_logical(x, name)
}

assert_scalar_atomic <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  if (!is.atomic(x)) {
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

assert_file_exists <- function(x, check_case = TRUE, workdir = NULL,
                               name = "File") {
  err <- !file_exists(x, check_case = check_case, workdir = workdir)
  if (any(err)) {
    i <- attr(err, "incorrect_case")
    if (!is.null(i)) {
      msg_case <- x[i]
      msg_totally <- x[err & !i]
      if (length(msg_case) > 0L) {
        correct_case <- attr(err, "correct_case")
        msg_case <- sprintf("'%s' (should be '%s')",
                            names(correct_case), correct_case)
      }
      msg <- c(msg_case, squote(msg_totally))
    } else {
      msg <- squote(x[err])
    }
    stop(sprintf("%s does not exist: %s", name, paste(msg, collapse = ", ")),
         call. = FALSE)
  }
}

assert_is_directory <- function(x, check_case = TRUE, workdir = NULL,
                                name = "File") {
  assert_file_exists(x, check_case, workdir, name)
  path <- if (is.null(workdir)) x else file.path(workdir, x)
  if (!is_directory(path)) {
    stop(sprintf("%s exists but is not a directory: %s",
                 name, paste(x, collapse = ", ")),
         call. = FALSE)
  }
}

assert_relative_path <- function(x, no_dots = FALSE,
                                 name = deparse(substitute(x))) {
  err <- fs::is_absolute_path(x)
  if (any(err)) {
    stop(sprintf("'%s' must be relative %s",
                 name, ngettext(length(x), "path", "paths")),
         call. = FALSE)
  }
  if (no_dots && any(grepl("..", x, fixed = TRUE))) {
    stop(sprintf("'%s' must not contain '..' path components", name),
         call. = FALSE)
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
