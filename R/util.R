`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


is_directory <- function(x) {
  file.info(x, extra_cols = FALSE)$isdir
}
