numbers <- function(as, n) {
  ctx <- orderly2::orderly_plugin_context("example.random")
  x <- ctx$config$generator(n)
  ctx$envir[[as]] <- x
  info <- list(as = as, mean = mean(x), variance = var(x))
  orderly2::orderly_plugin_add_metadata("example.random", "numbers", info)
  invisible()
}

config <- function(data, filename) {
  orderly2:::assert_named(
    data, name = paste0(filename, ":example.random"))
  orderly2:::assert_scalar_character(
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
  orderly2::orderly_plugin_register("example.random",
                                    config = config,
                                    serialise = serialise,
                                    schema = schema)
}
