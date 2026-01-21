# Add metadata from plugin

Add plugin-specific metadata to a running packet. This will take some
describing. You accumulate any number of bits of metadata into arbitrary
fields, and then later on serialise these to json.

## Usage

``` r
orderly_plugin_add_metadata(name, field, data)
```

## Arguments

- name:

  The name of the plugin; must be the same as used in
  [`orderly_plugin_register()`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_register.md)
  and
  [`orderly_plugin_context()`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_context.md)

- field:

  The name of a field to add the data to. This is required even if your
  plugin only produces one sort of data, in which case you can remove it
  later on within your serialisation function.

- data:

  Arbitrary data to be added to the currently running packet

## Value

Nothing, called only for its side effects

## Examples

``` r
# The example code from vignette("plugins") is available in the package
fs::dir_tree(system.file("examples/example.db", package = "orderly"))
#> /home/runner/work/_temp/Library/orderly/examples/example.db
#> ├── DESCRIPTION
#> ├── NAMESPACE
#> ├── R
#> │   └── plugin.R
#> └── inst
#>     └── schema.json

# See orderly_plugin_add_metadata in context here:
orderly_example_show("R/plugin.R", example = "example.db")
#> 
#> ── R/plugin.R ──────────────────────────────────────────────────────────────────
#> db_config <- function(data, filename) {
#>   if (!is.list(data) || is.null(names(data)) || length(data) == 0) {
#>     stop("Expected a named list for orderly_config.json:example.db")
#>   }
#>   if (length(data$path) != 1 || !is.character(data$path)) {
#>     stop("Expected a string for orderly_config.json:example.db:path")
#>   }
#>   if (!file.exists(data$path)) {
#>     stop(sprintf(
#>       "The database '%s' does not exist (orderly_config:example.db:path)",
#>       data$path))
#>   }
#>   data
#> }
#>  
#> query <- function(sql) {
#>   ctx <- orderly::orderly_plugin_context("example.db")
#>   dbname <- ctx$config$path
#>   con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
#>   on.exit(DBI::dbDisconnect(con))
#>   d <- DBI::dbGetQuery(con, sql)
#>   info <- list(sql = sql, rows = nrow(d), cols = names(d))
#>   orderly::orderly_plugin_add_metadata("example.db", "query", info)
#>   d
#> }
#>  
#> .onLoad <- function(...) {
#>   orderly::orderly_plugin_register(
#>     name = "example.db",
#>     config = db_config,
#>     serialise = db_serialise,
#>     deserialise = db_deserialise,
#>     schema = "schema.json")
#> }
```
