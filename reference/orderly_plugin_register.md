# Register an orderly plugin

Create an orderly plugin. A plugin is typically defined by a package and
is used to extend orderly by enabling new functionality, declared in
your orderly configuration (`orderly_config.json`) and your orderly file
(`<name>.R`), and affecting the running of reports primarily by creating
new objects in the report environment. This system is discussed in more
detail in
[`vignette("plugins")`](https://mrc-ide.github.io/orderly/articles/plugins.md).

## Usage

``` r
orderly_plugin_register(
  name,
  config,
  serialise = NULL,
  deserialise = NULL,
  cleanup = NULL,
  schema = NULL
)
```

## Arguments

- name:

  The name of the plugin, typically the package name

- config:

  A function to read, check and process the configuration section in the
  orderly configuration. This function will be passed the deserialised
  data from the plugin's section of
  `orderly_config.json, and the full path to that file. As the order of loading of plugins is not defined, each plugin must standalone and should not try and interact with other plugins at load. It should return a processed copy of the configuration data, to be passed in as the second argument to `read\`.

- serialise:

  A function to serialise any metadata added by the plugin's functions
  to the outpack metadata. It will be passed a list of all entries
  pushed in via
  [`orderly_plugin_add_metadata()`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_add_metadata.md);
  this is a named list with names corresponding to the `field` argument
  to `orderly_plugin_add_metadata` and each list element being an
  unnamed list with values corresponding to `data`. If `NULL`, then no
  serialisation is done, and no metadata from your plugin will be added.

- deserialise:

  A function to deserialise any metadata serialised by the `serialise`
  function. This is intended to help deal with issues disambiguating
  unserialising objects from json (scalars vs arrays of length 1,
  data.frames vs lists-of-lists etc), and will make your plugin nicer to
  work with
  [`orderly_metadata_extract()`](https://mrc-ide.github.io/orderly/reference/orderly_metadata_extract.md).
  This function will be given a single argument `data` which is the data
  from `jsonlite::fromJSON(..., simplifyVector = FALSE)` and you should
  apply any required simplifications yourself, returning a modified copy
  of the argument.

- cleanup:

  Optionally, a function to clean up any state that your plugin uses.
  You can call `orderly_plugin_context` from within this function and
  access anything you need from that. If not given, then no cleanup is
  done.

- schema:

  Optionally a path, within the package, to a schema for the metadata
  created by this plugin; you should omit the `.json` extension. So if
  your file contains in its sources the file `inst/plugin/myschema.json`
  you would pass `plugin/myschema`. See
  [`vignette("plugins")`](https://mrc-ide.github.io/orderly/articles/plugins.md)
  for details.

## Value

Nothing, this function is called for its side effect of registering a
plugin.

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

# See orderly_plugin_register in context here:
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
