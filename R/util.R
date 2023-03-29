`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


is_directory <- function(x) {
  file.info(x, extra_cols = FALSE)$isdir
}


is_call <- function(x, name) {
  is.recursive(x) && is.name(x[[1]]) && as.character(x[[1]]) == name
}


is_orderly_call <- function(x, name) {
  is_call(x, name) || (
    is.recursive(x) &&
    is_call(x[[1]], "::") &&
    as.character(x[[1]][[2]]) == "orderly3" &&
    as.character(x[[1]][[3]]) == name)
}


vlapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, logical(1), ...)
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}
