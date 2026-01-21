# Creating plugins

**You may not need to read this: the intended readers are authors of
`orderly` plugins, not users of such plugins.**

In order to make `orderly` more extensible without bloating the core, we
have designed a simple plugin interface. Our first use case for this is
shifting all of `orderly1`’s database functionality out of the main
package, but other uses are possible!

This vignette is intended to primarily serve as a design document, and
will be of interest to the small number of people who might want to
write a new plugin, or to edit an existing one.

## The basic idea

A plugin is provided by a package, possibly it will be the only thing
that a package provides. The plugin name must (currently) be the same as
the package name. The only functions that the package needs to call are
`orderly::orderly_plugin` and
[`orderly::orderly_plugin_register`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_register.md)
which create and register the plugin, respectively.

To make a plugin available for an `orderly` project, two new bits of
configuration may be present in `orderly_config.json` - one declares the
plugin will be used, the other configures the plugin.

To use a plugin for an individual report, functions from the plugin
should be used, which configure and use the plugin.

Finally, we can save information back into the final `orderly` metadata
about what the plugin did.

With the yaml-less design of `orderly` (see
[`vignette("migrating")`](https://mrc-ide.github.io/orderly/articles/migrating.md)
if you are familiar with `orderly1`), the line between a plugin and just
package code is fairly blurred, but reasons for writing a plugin are
typically that you want to make something easier in reports, and you
want that action reflected in the `orderly` metadata.

## An example

As an example, we’ll implement a stripped down version of the database
plugin that inspired this work (see
[\`orderly.db](https://github.com/mrc-ide/orderly.db) for a fuller
implementation). To make this work we need functions:

- …that process additional fields in `orderly_config.json` that describe
  where to find the database
- …that can be called from an `orderly` file that access the database
- …that can add metadata to the final `orderly` metadata about what was
  done

We’ll start with the report side of things, describing what we want to
happen, then work on the implementation.

Here is the directory structure of our minimal project

    ## .
    ## ├── orderly_config.json
    ## └── src
    ##     └── example
    ##         └── example.R

The `orderly_config.json` file contains the information shared by all
possible uses of the plugin - in the case the connection information for
the database:

``` json
{
  "minimum_orderly_version": "1.99.90",
  "plugins": {
    "example.db": {
      "path": "/tmp/Rtmpnp8cRc/file2104208f9970"
    }
  }
}
```

Our plugin is called `example.db` and is listed within the `plugins`
section, along with its configuration; in this case indicating the path
where the SQLite file can be loaded from.

The `example.R` file contains information about use of the database for
this specific report; in this case, making the results of the query
`SELECT * from mtcars WHERE cyl == 4` against the database available as
some R object `dat`

``` r
dat <- example.db::query("SELECT * FROM mtcars WHERE cyl == 4")
orderly::orderly_artefact(description = "Summary of data", "data.rds")

saveRDS(summary(dat), "data.rds")
```

Normally, we imagine some calculation here but this is kept minimal for
the purpose of demonstration.

To implement this we need to:

1.  create a package
2.  write a function to handle the configuration in
    `orderly_config.json`
3.  write a function `query()` used in `example.R` to do the query
    itself

### Create a tiny package

There are lots of package skeleton tools out there, and if you do not
have a favourite, `usethis::create_package()` will probably do a
reasonable job. The only thing your package needs to do is to contain
`Imports: orderly` in its `DESCRIPTION` field.

A simple package may have a structure like

    ## .
    ## ├── DESCRIPTION
    ## ├── NAMESPACE
    ## └── R
    ##     └── plugin.R

Here, our `DESCRIPTION` file contains:

``` plain
Package: example.db
Version: 0.0.1
License: CC0
Title: Orderly Database Example Plugin
Description: Simple example of an orderly plugin.
Authors@R: person('Orderly Authors', role = c('aut', 'cre'),
    email = 'email@example.com')
Imports: orderly
```

and the `NAMESPACE` and `R/plugin.R` files are shown below.

### Handle the configuration

The only required function that a plugin needs to provide is one to
process the data from `orderly_config.json`. This is probably primarily
concerned with validation so can be fairly simple at first, later we’ll
expand this to report errors nicely:

``` r
db_config <- function(data, filename) {
  data
}
```

The arguments here are

- `data`: the deserialised section of the `orderly_config.json` specific
  to this plugin
- `filename`: the full path to `orderly_config.json`

The return value here should be the `data` argument with any auxiliary
data added after validation.

### Evaluate the query

Finally, for our minimal example, we need the function that actually
does the query; in our example above this is `example.db::query`:

``` r
query <- function(sql) {
  ctx <- orderly::orderly_plugin_context("example.db")
  dbname <- ctx$config$path
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}
```

The arguments here are whatever you want the user to provide – nothing
here is special to `orderly`. The important function here to call is
[`orderly::orderly_plugin_context`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_context.md)
which returns information that you can use to make the plugin work. This
is explained in
[`?orderly::orderly_plugin_context`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_context.md),
but in this example we use just one element, `config`, the configuration
for this plugin (i.e., the return value from our function `db_config`);
see
[`orderly::orderly_plugin_context`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_context.md)
for other context that can be accessed here.

The last bit of package code is to register the plugin, we do this by
calling
[`orderly::orderly_plugin_register`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_register.md)
within `.onLoad()` which is a special R function called when a package
is loaded. This means that whenever your packages is loaded (regardless
of whether it is attached) it will register the plugin.

``` r
.onLoad <- function(...) {
  orderly::orderly_plugin_register(
    name = "example.db",
    config = db_config)
}
```

(It is important that the `name` argument here matches your package
name, as `orderly` will trigger loading the package based on this name
in the configuration; we may support multiple plugins within one package
later.)

Note that our `query` function here does not appear within this
registration, just the function to read and process the configuration.

Our final (minimal) package code is:

``` r
db_config <- function(data, filename) {
  data
}

query <- function(sql) {
  ctx <- orderly::orderly_plugin_context("example.db")
  dbname <- ctx$config$path
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}

.onLoad <- function(...) {
  orderly::orderly_plugin_register(
    name = "example.db",
    config = db_config)
}
```

and the `NAMESPACE` file contains

``` plain
export(query)
```

### Trying it out

In order to test your package, it needs to be loaded. You can do this by
either installing the package or by using
[`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
(you may find doing so with `pkgload::load_all(export_all = FALSE)`
gives the most reliable experience.

``` r
pkgload::load_all()
```

    ## ℹ Loading example.db

Now, we can run the report:

``` r
library(orderly)
orderly_run("example", root = path_root)
## ℹ Starting packet 'example' `20260121-095958-2ecae11a` at 2026-01-21 09:59:58.188772
## > dat <- example.db::query("SELECT * FROM mtcars WHERE cyl == 4")
## > orderly::orderly_artefact(description = "Summary of data", "data.rds")
## > saveRDS(summary(dat), "data.rds")
## ✔ Finished running example.R
## ℹ Finished 20260121-095958-2ecae11a at 2026-01-21 09:59:58.281895 (0.09312296 secs)
## [1] "20260121-095958-2ecae11a"
```

## Making the plugin more robust

The plugin above is fairly fragile because it does not do any validation
on the input data from `orderly_config.json`. This is fairly annoying to
do in practice, but a little effort will make the experience for a user
better because they will be able to debug incorrect configuration more
effectively.

In our case, we expect a single key-value pair in `orderly_config.json`
with the key being `path` and the value being the path to a SQLite
database. We can easily expand our configuration function to report
better back to the user when they misconfigure the plugin:

``` r
db_config <- function(data, filename) {
  if (!is.list(data) || is.null(names(data)) || length(data) == 0) {
    stop("Expected a JSON object for orderly_config.json:example.db")
  }
  if (length(data$path) != 1 || !is.character(data$path)) {
    stop("Expected a string for orderly_config.json:example.db:path")
  }
  if (!file.exists(data$path)) {
    stop(sprintf(
      "The database '%s' does not exist (orderly_config:example.db:path)",
      data$path))
  }
  data
}
```

This should do an acceptable job of preventing poor input while
suggesting to the user where they might look within the configuration to
fix it. Note that we return the configuration data here, and you can
augment (or otherwise change) this data as you need.

## Saving metadata about what the plugin did

Nothing about what the plugin does is saved into the report metadata
unless you save it. Partly this is because the `orderly` source file,
which is saved into the final directory, serves as some sort of record.
However, you probably want to know something about the data that you
returned here. For example we might want to save

- the query string so that later we can query it without having to read
  and process the `orderly` source file
- some statistics about the size of the data (e.g., the number of rows
  returned, or the columns)
- perhaps some summary of the content such as a hash so that we can see
  if the content has changed between different versions of a report

To save metadata, use the function
[`orderly::orderly_plugin_add_metadata`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_add_metadata.md);
this takes as arguments your plugin name, any string you like to
structure the saved metadata (here we’ll use `query`) and whatever data
you want to save:

``` r
query <- function(sql) {
  ctx <- orderly::orderly_plugin_context("example.db")
  dbname <- ctx$config$path
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
  on.exit(DBI::dbDisconnect(con))
  d <- DBI::dbGetQuery(con, sql)
  info <- list(sql = sql, rows = nrow(d), cols = names(d))
  orderly::orderly_plugin_add_metadata("example.db", "query", info)
  d
}
```

This function is otherwise the same as the minimal version above.

We also need to provide a serialisation function to ensure that the
metadata is saved as expected. Because we saved our metadata under the
key `query`, we will get a list back with an element `query` and then an
unnamed list with as many elements as there were `query` calls in a
given report.

``` r
db_serialise <- function(data) {
  for (i in seq_along(data$query)) {
    # Always save cols as a vector, even if length 1:
    data$query[[i]]$cols <- I(data$query[[i]]$cols)
  }
  jsonlite::toJSON(data$query, auto_unbox = TRUE)
}
```

Here, we ensure that everything except `cols` that is length 1 (which
will be everything) gets turned into a scalar (so `1` not `[1]`) and
then serialise with
[`jsonlite::toJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
with `auto_unbox` as `TRUE`.

Taking this a step further, we can also specify a
[schema](https://json-schema.org/) that this metadata will conform to

``` json
{
    "$schema": "http://json-schema.org/draft-07/schema#",

    "type": "array",
    "items": {
        "type": "object",
        "properties": {
            "sql": {
                "type": "string"
            },
            "rows": {
                "type": "number"
            },
            "cols": {
                "type": "array",
                "items": {
                    "type": "string"
                }
            }
        },
        "required": ["sql", "rows", "cols"],
        "additionalProperties": false
    }
}
```

We save this file as `inst/schema.json` within the package (any path
within `inst` is fine, just adapt the schema path in the call to
`orderly_plugin_register` below).

Finally, we can also add a deserialisation hook to convert the loaded
metadata into a nice `data.frame`:

Now, when we register the plugin, we provide the path to this schema,
along with the serialisation and deserialisation functions:

``` r
.onLoad <- function(...) {
  orderly::orderly_plugin_register(
    name = "example.db",
    config = db_config,
    serialise = db_serialise,
    deserialise = db_deserialise,
    schema = "schema.json")
}
```

Now, when the `orderly` metadata is saved (just before running the
script part of a report) we will validate output that was passed into
[`orderly::orderly_plugin_add_metadata`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_add_metadata.md)
against the schema, if `jsonvalidate` is installed and if the R option
`orderly.schema_validate` is set to `TRUE` (e.g., by running
`options(orderly.schema_validate = TRUE)`, see
[`vignette("details")`](https://mrc-ide.github.io/orderly/articles/details.md)).

Our final package has structure:

    ## .
    ## ├── archive
    ## │   └── example
    ## │       └── 20260121-095958-2ecae11a
    ## │           ├── data.rds
    ## │           └── example.R
    ## ├── draft
    ## │   └── example
    ## ├── orderly_config.json
    ## └── src
    ##     └── example
    ##         └── example.R

The `DESCRIPTION` file and `NAMESPACE` are unchanged from above, and the
schema is shown just above.

The `plugin.R` file contains the code collected from above:

``` r
db_config <- function(data, filename) {
  if (!is.list(data) || is.null(names(data)) || length(data) == 0) {
    stop("Expected a JSON object for orderly_config.json:example.db")
  }
  if (length(data$path) != 1 || !is.character(data$path)) {
    stop("Expected a string for orderly_config.json:example.db:path")
  }
  if (!file.exists(data$path)) {
    stop(sprintf(
      "The database '%s' does not exist (orderly_config:example.db:path)",
      data$path))
  }
  data
}

query <- function(sql) {
  ctx <- orderly::orderly_plugin_context("example.db")
  dbname <- ctx$config$path
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
  on.exit(DBI::dbDisconnect(con))
  d <- DBI::dbGetQuery(con, sql)
  info <- list(sql = sql, rows = nrow(d), cols = names(d))
  orderly::orderly_plugin_add_metadata("example.db", "query", info)
  d
}

.onLoad <- function(...) {
  orderly::orderly_plugin_register(
    name = "example.db",
    config = db_config,
    serialise = db_serialise,
    deserialise = db_deserialise,
    schema = "schema.json")
}
```

(this code could be in any .R file in the package, or across several).

``` r
id <- orderly_run("example", root = path_root)
## ℹ Starting packet 'example' `20260121-095959-25d29c15` at 2026-01-21 09:59:59.151943
## > dat <- example.db::query("SELECT * FROM mtcars WHERE cyl == 4")
## > orderly::orderly_artefact(description = "Summary of data", "data.rds")
## > saveRDS(summary(dat), "data.rds")
## ✔ Finished running example.R
## ℹ Finished 20260121-095959-25d29c15 at 2026-01-21 09:59:59.190716 (0.0387733 secs)
meta <- orderly_metadata(id, root = path_root)
meta$custom$example.db
##                                   sql rows         cols
## 1 SELECT * FROM mtcars WHERE cyl == 4   11 mpg, cyl....
```

## Potential uses

Our need for this functionality is similar to this example - pulling out
the database functionality from the original version of `orderly` into
something that is more independent, as it turns out to be useful only in
a fraction of `orderly` use-cases. We can imagine other potential uses
though, such as:

- Non-DBI-based database data extraction, or customised routines for
  pulling data from a database
- Download files from some shared location just before use (e.g.,
  SharePoint, OneDrive, AWS). The `orderly_config.json` would contain
  account connection details and the `orderly` source file would contain
  mapping between the remote data/files and local files. Rather than
  writing to the environment as we do above, use the `path` argument to
  copy files into the correct place.
- Pull data from some web API just before running

These all follow the same basic pattern of requiring some configuration
in order to be able to connect to the resource service, some
specification of what resources are to be fetched, and some action to
actually fetch the resource and put it into place.
