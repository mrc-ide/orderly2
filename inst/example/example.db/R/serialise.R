db_serialise <- function(data) {
  lapply(data$query, function(x) {
    list(query = jsonlite::unbox(x$query),
         as = jsonlite::unbox(x$as),
         rows = jsonlite::unbox(x$rows),
         cols = x$cols)
  })
}
