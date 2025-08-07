db_config <- function(data, filename) {
  if (!is.list(data) || is.null(names(data)) || length(data) == 0) {
    stop("Expected a named list for orderly_config.yml:example.db")
  }
  if (length(data$path) != 1 || !is.character(data$path)) {
    stop("Expected a string for orderly_config.yml:example.db:path")
  }
  if (!file.exists(data$path)) {
    stop(sprintf(
      "The database '%s' does not exist (orderly_config:example.db:path)",
      data$path))
  }
  data
}

query <- function(sql) {
  ctx <- orderly2::orderly_plugin_context("example.db")
  dbname <- ctx$config$path
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
  on.exit(DBI::dbDisconnect(con))
  d <- DBI::dbGetQuery(con, sql)
  info <- list(sql = sql, rows = nrow(d), cols = names(d))
  orderly2::orderly_plugin_add_metadata("example.db", "query", info)
  d
}

.onLoad <- function(...) {
  orderly2::orderly_plugin_register(
    name = "example.db",
    config = db_config,
    serialise = db_serialise,
    deserialise = db_deserialise,
    schema = "schema.json")
}
