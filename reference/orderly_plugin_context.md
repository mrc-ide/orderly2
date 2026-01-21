# Fetch plugin context

Fetch the running context, for use within a plugin. The intention here
is that within free functions that your plugin makes available, you will
call this function to get information about the state of a packet. You
will then typically call
[`orderly_plugin_add_metadata()`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_add_metadata.md)
afterwards.

## Usage

``` r
orderly_plugin_context(name, envir)
```

## Arguments

- name:

  Name of the plugin

- envir:

  The environment of the calling function. You can typically pass
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) (or
  [`rlang::caller_env()`](https://rlang.r-lib.org/reference/stack.html))
  here if the function calling `orderly_plugin_context()` is the
  function that would be called by a user. This argument only has an
  effect in interactive use (where `envir` is almost certainly the
  global environment).

## Value

A list with elements:

- `is_active`: a logical, indicating if we're running under
  [`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md);
  you may need to change behaviour depending on this value.

- `path`: the path of the running packet. This is almost always the
  working directory, unless the packet contains calls to
  [`setwd()`](https://rdrr.io/r/base/getwd.html) or similar. You may
  create files here.

- `config`: the configuration for this plugin, after processing with the
  plugin's `read` function (see
  [`orderly_plugin_register`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_register.md))

- `envir`: the environment that the packet is running in. Often this
  will be the global environment, but do not assume this! You may read
  and write from this environment.

- `src`: the path to the packet source directory. This is different to
  the current directory when the packet is running, but the same when
  the user is interactively working with a report. You may *read* from
  this directory but *must not write to it*

- `parameters`: the parameters as passed through to the run the report.

## Details

When a plugin function is called, orderly will be running in one of two
modes; (1) from within
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md),
in which case we're part way through creating a packet in a brand new
directory, and possibly using a special environment for evaluation, or
(2) interactively, with a user developing their report. The plugin needs
to be able to support both modes, and this function will return
information about the state to help you cope with either case.

## See also

[`orderly_plugin_register()`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_register.md),
[`orderly_plugin_add_metadata()`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_add_metadata.md)

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

# See orderly_plugin_context in context here:
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
