cache <- new.env(parent = emptyenv())

custom_metadata_schema <- function() {
  if (is.null(cache$custom_metadata_schema)) {
    path <- system.file("outpack-custom.json", package = "orderly3",
                        mustWork = TRUE)
    cache$custom_metadata_schema <- paste(readLines(path), collapse = "\n")
  }
  cache$custom_metadata_schema
}
