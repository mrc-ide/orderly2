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

assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
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
