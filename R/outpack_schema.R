outpack_schema_version <- function() {
  if (is.null(cache$schema_version)) {
    path <- outpack_file("schema/metadata.json")
    cache$schema_version <- jsonlite::read_json(path)$version
  }
  cache$schema_version
}


outpack_schema <- function(name) {
  load_schema(name, outpack_file(sprintf("schema/%s.json", name)))
}


custom_schema <- function(schema) {
  load_schema(hash_data(schema, "sha1"), schema)
}


load_schema <- function(key, schema) {
  if (is.null(cache$schema[[key]])) {
    if (is.null(cache$schema)) {
      cache$schema <- list()
    }
    cache$schema[[key]] <- jsonvalidate::json_schema$new(schema)
  }
  cache$schema[[key]]
}


should_validate_schema <- function(schema) {
  !is.null(schema) && getOption("outpack.schema_validate", FALSE)
}
