query <- function(as, query) {
  ctx <- orderly3::orderly_plugin_context("example.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), ctx$config$path)
  on.exit(DBI::dbDisconnect(con))
  d <- DBI::dbGetQuery(con, query)
  ctx$env[[as]] <- d
  info <- list(as = as, query = query, rows = nrow(d), cols = names(d))
  orderly3::orderly_plugin_add_metadata("example.db", "query", info)
  invisible()
}
