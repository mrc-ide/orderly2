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


dir_ls_local <- function(path, ...) {
  withr::with_dir(path, fs::dir_ls(path = ".", ...))
}


scalar <- function(x) {
  jsonlite::unbox(x)
}


to_json <- function(obj, pretty = FALSE) {
  jsonlite::toJSON(obj, pretty = pretty, auto_unbox = FALSE, na = "null",
                   null = "null", json_verbatim = TRUE, digits = NA)
}


rep_along <- function(x, v) {
  rep_len(x, length(v))
}


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}


squote <- function(x) {
  sprintf("'%s'", x)
}
