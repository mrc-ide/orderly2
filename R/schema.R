cache <- new.env(parent = emptyenv())

outpack_schema_version <- function() {
  if (is.null(cache$schema_version)) {
    path <- orderly_file("schema/outpack/metadata.json")
    cache$schema_version <- jsonlite::read_json(path)$version
  }
  cache$schema_version
}


load_schema <- function(key) {
  if (is.null(cache$schema[[key]])) {
    if (is.null(cache$schema)) {
      cache$schema <- list()
    }
    key_split <- strsplit(key, "/", fixed = TRUE)[[1]]
    if (key_split[[1]] %in% c("orderly", "outpack")) {
      path <- orderly_file(file.path("schema", key))
    } else {
      ## This peculiar construction lets us load devmode packages from
      ## orderly when it is installed itself as a real package...
      path <- file.path(pkg_root(key_split[[1]]), key_split[[2]])
    }
    ensure_jsonvalidate()
    cache$schema[[key]] <- jsonvalidate::json_schema$new(path)
  }
  cache$schema[[key]]
}


should_validate_schema <- function(schema) {
  !is.null(schema) && getOption("outpack.schema_validate", FALSE)
}


ensure_jsonvalidate <- function() {
  if (!requireNamespace("jsonvalidate")) {
    cli::cli_abort(
      c("The jsonvalidate package is missing, but you have requested it",
        i = paste("The option 'outpack.schema_validate' is 'TRUE', but you",
                  "don't have 'jsonvalidate' installed so we can't do this.")))
  }
}
