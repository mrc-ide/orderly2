.onLoad <- function(...) {
  schema <- system.file("schema.json", package = "example.db", mustWork = TRUE)
  orderly3::orderly_plugin_register("example.db", db_config, db_serialise,
                                    schema)
}
