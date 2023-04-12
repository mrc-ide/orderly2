numbers <- function(as, n) {
  ctx <- orderly3::orderly_plugin_context("example.random")
  x <- ctx$config$generator(n)
  ctx$env[[as]] <- x
  info <- list(as = as, mean = mean(x), variance = var(x))
  orderly3::orderly_plugin_add_metadata("example.random", "numbers", info)
  invisible()
}

read <- function(data, filename) {
  orderly3:::assert_named(
    data, name = paste0(filename, ":example.random"))
  orderly3:::assert_scalar_character(
    data$distribution,
    paste0(filename, ":example.random:distribution"))
  data$generator <- switch(
    data$distribution,
    normal = rnorm,
    uniform = runif,
    stop(sprintf("Unknown value '%s' for '%s:example.random:distribution'",
                 data$distribution, filename)))
  data
}

serialise <- function(data) {
  jsonlite::toJSON(data$numbers, auto_unbox = TRUE, digits = NA)
}

.onLoad <- function(...) {
  schema <- system.file("schema.json", package = "example.random",
                        mustWork = TRUE)
  orderly3::orderly_plugin_register("example.random", read, serialise, schema)
}