db_config <- function(data, filename) {
  if (!is.list(data) || is.null(names(data)) || length(data) == 0) {
    stop(sprintf("Expected a named list for %s:example.db", filename))
  }
  if (length(data$path) != 1 || !is.character(data$path)) {
    stop(sprintf("Expected a string for %s:example.db:path", filename))
  }
  if (!file.exists(data$path)) {
    stop(sprintf(
      "The database '%s' does not exist (%s:example.db:path)",
      filename, data$path))
  }
  data
}
