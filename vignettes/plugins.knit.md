In order to make orderly/orderly2/orderly3 more extensible without bloating the core, we have designed a simple plugin interface. Our first use case for this is shifting all of orderly 1.0's database functionality out of the main package, but other uses are possible!

This vignette is intended to primarily serve as a design document, and will be of interest to the small number of people who might want to write a new plugin, or to edit an existing one; it is **not** intended for users of plugins.

## The basic idea

A plugin is provided by a package, probably it will be the only thing that a package provides.  The plugin name will be the same as the package name.

To make a plugin available for an orderly project, two new bits of configuration may be present in `orderly_config.yml` - one declares the plugin will be used (this must be present), the other configures the plugin (this may not be needed, depending on the plugin).

To use a plugin for an individual report, the user adds any plugin-provided functions within their `orderly.R` script.

Finally, we can save information back into the final orderly3 metadata about what the plugin did.

From a design point of view, consider which external resources any use of your plugin might want to use (databases, external sources of files etc); these will typically be the configuration options that you would have users specify in `orderly_config.yml` and would be available across all uses of your plugin (not all reports would use them).

Then write your package functions so that they can work well when provided with information about the configuration. You can use the working directory as a way of establishing a "scope", if you need to share resources across multiple calls.  Typically a plugin will write files into the packet directory while it runs, or it will assign objects into the environment (or both).

Finally, when done, we typically want to save information about what was done in a form that can be pulled from the outpack metadata store.  To do this we need to acculate metadata, to serialise it into a single json object (or array) and then provide a schema so that the metadata may be validated.

## An example

As an example, we'll implement a stripped down version of the database plugin that inspired this work. To make this work:

* we need to add to `orderly_config.yml` some plugin configuration describing where we can find the database
* we need to add to `orderly.R` some information about queries that we want to make, resulting in data available to the report

We'll start with the report side of things, describing what we want to happen, then work on the implementation.



Here is the directory structure of our minimal project


```
#> .
#> ├── orderly_config.yml
#> └── src
#>     └── example
#>         └── orderly.R
```

The `orderly_config.yml` file contains the information shared by all possible uses of the plugin - in the case the connection information for the database:

```yaml
plugins:
  example.db:
    path: /tmp/Rtmp20kHJy/file176163a8b7aa4
```

The `orderly.R` script contains information to run the report, including extracting data from the database:

```r
example.db::query("dat", "SELECT * from mtcars WHERE cyl == 4")
orderly3::orderly_artefact("summary of data", "data.rds")
saveRDS(summary(dat), "data.rds")
```

The function `example.db::query` is one we'll need to make available from our plugin package; in this example we're making the results of the query `SELECT * from mtcars WHERE cyl == 4` against the database available as some R object `dat`. We've kept the actual calculation within this script trivial here for simplicity.

To implement this we need to write three functions:

(1) A "config" function to process the data from `orderly_config.yml`, primarily this is concerned with validation so can be fairly simple at first, later we'll expand this to report errors nicely.


```r
db_config <- function(data, filename) {
  data
}
```

The arguments here are

* `data`: the deserialised section of the `orderly_config.yml` specific to this plugin
* `filename`: the full path to `orderly_config.yml`

The return value here should be the `data` argument with any auxiliary data added after validation. For now, we assume that the configuration is correct.

(2) The function `example.db::query` that we referenced from the code above:

```r
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
```

Here, the first line which calls `orderly3::orderly_plugin_context` fetches the running context; this includes information about the report working directory, and the environment; see the reference docs for information. Typically any call to a plugin function will call this function first.

After that we proceed in a fairly straightforward way:

* Create a connection to the db with `DBI::dbConnect`, using configuration information available as part of the context (`ctx$config`; see below)
* Arrange to close this connection even on error using `DBI::dbDisconnect` from within `on.exit`
* Make a query against the database using `DBI::dbGetQuery`

The whole point of this process is to affect the running of the report so at some point we need to assign objects into the environment or write files to disk; here we use `ctx$env` to assign into the packet environment - after completion of this function the data will be available for use.

Then, we collect some metadata about the process. This will be stored within the outpack metadata and available for later querying. Here, we save the name of the created object, the query used, the number of rows retrieved and the names of the columns.  We first collect this as `info` then use `orderly3::orderly_plugin_add_metadata` to save it within the currently running packet.

Finally, return nothing (this is not enforced, but will be compatible with future versions of `orderly3` which may assume this).

(3) A function to serialise the metadata collected above:

```r
db_serialise <- function(data) {
  lapply(data$query, function(x) {
    list(query = jsonlite::unbox(x$query),
         as = jsonlite::unbox(x$as),
         rows = jsonlite::unbox(x$rows),
         cols = x$cols)
  })
}
```

Each time a user calls `example.db::query` we accumulate metadata within the `query` field of our metadata, one per call, and we want to convert this into an array of objects. This function does this while making sure that all values we know are scalars are serialised correctly (it's not obvious to `jsonlite` if `1` represents a vector of length 1 or a number, and `orderly3` and `outpack` avoid doing any guessing, so all scalars must be marked explicitly).

Note that this does not actually convert to JSON, but we could. If you do, be sure to use `jsonlite::toJSON` (with whatever options you prefer) so that the resulting string ends up with the class `json` so that it is interpolated correctly into the final structure.

Finally, we need to register the plugin; this is done via the `.onLoad` function:

```r
.onLoad <- function(...) {
  orderly3::orderly_plugin_register("example.db", db_config, db_serialise)
}
```

This code is all placed in a package, with a `DESCRIPTION` file containing:

```plain
Package: example.db
Title: Example Database Access from 'orderly3'
Version: 0.0.1
Authors@R: c(person("Rich", "FitzJohn", role = c("aut", "cre"),
                    email = "rich.fitzjohn@gmail.com"),
             person("Robert", "Ashton", role = "aut"),
             person("Alex", "Hill", role = "aut"),
             person("Imperial College of Science, Technology and Medicine",
                    role = "cph"))
Description: Shows how to create a simple package that provides an
    'orderly3' plugin.  This plugin is really a stripped down version of
    the plugin available at <https://github.com/mrc-ide/orderly3.db>
License: CC0
Imports:
    DBI,
    RSQLite,
    orderly3
```

and `NAMESPACE`:

```plain
export(query)
```

With all this in place, we can run the report once the package is installed




```r
id <- orderly3::orderly_run("example", root = path_root)
```

# Making the plugin more robust

The plugin above is fairly fragile because it does not do any validation on the input data in `orderly_config.yml`.

In our case, we expect a single key-value pair in `orderly_config.yml` with the key being `path` and the value being the path to a SQLite database. We can easily expand our configuration function to report better back to the user when they misconfigure the plugin:

```r
db_config <- function(data, filename) {
  if (!is.list(data) || is.null(names(data)) || length(data) == 0) {
    stop(sprintf("Expected a named list for %s:example.db", filename))
  }
  if (length(data$path) != 1 || !is.character(data$path)) {
    stop(sprintf("Expected a string for %s:example.db:path", filename))
  }
  if (!file.exists(data$path)) {
    stop(sprintf(
      "The database '%s' does not exist (%s:example.db:path)",
      filename, data$path))
  }
  data
}
```

This should do an acceptable job of preventing poor input while suggesting to the user where they might look within the configuration to fix it. Note that we return the configuration data here, and you can augment (or otherwise change) this data as you need.

Expanding the validation within the query function is easy, if tedious, as this is just an ordinary R function.

# Specifying a schema for metadata

Above, we push back metadata with `orderly3::orderly_plugin_add_metadata` and serialise it with `db_serialise` but we never check the metadata created conforms to any standard.

In the example, above, the generated metadata contains:

```json
"plugins": {
    "example.db": [
        {
            "query": "SELECT * from mtcars WHERE cyl == 4",
            "as": "dat",
            "rows": 11,
            "cols": [
                "mpg",
                "cyl",
                "disp",
                "hp",
                "drat",
                "wt",
                "qsec",
                "vs",
                "am",
                "gear",
                "carb"
            ]
        }
    ]
}
```

We can formalise that all metadata "looks" like this by adding a  [schema](https://json-schema.org/):

```json
{
    "$schema": "http://json-schema.org/draft-07/schema#",

    "type": "array",
    "items": {
        "type": "object",
        "properties": {
            "query": {
                "type": "string"
            },
            "as": {
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
        "required": ["query", "as", "rows", "cols"],
        "additionalProperties": false
    }
}
```

and then using this schema when registering the plugin:

```r
.onLoad <- function(...) {
  schema <- system.file("schema.json", package = "example.db", mustWork = TRUE)
  orderly3::orderly_plugin_register("example.db", db_config, db_serialise,
                                    schema)
}
```

Now, when the orderly metadata is saved (at the final stage of running a packet) we will validate metadata saved by `orderly3::orderly_plugin_add_metadata` and serialised by `db_serialise`  that `db_run` generates against the schema, if `jsonvalidate` is installed (currently this requires our development version) and if the R option `outpack.schema_validate` is set to `TRUE` (e.g., by running `options(outpack.schema_validate = TRUE)`).

# The final package

The final package structure looks like this:


```
#> example.db
#> ├── DESCRIPTION
#> ├── NAMESPACE
#> ├── R
#> │   ├── query.R
#> │   ├── read.R
#> │   ├── serialise.R
#> │   └── zzz.R
#> └── inst
#>     └── schema.json
```

and the files are all included in your copy of `orderly3`; run `system.file("example/example.db", package = "orderly3")` to see it locally. Alternatively, browse the [source on GitHub](https://github.com/mrc-ide/orderly3/blob/plugin/inst/example/example.db)

# Potential uses

Our need for this functionality are similar to this example - pulling out the database functionality from the original version of orderly into something that is more independent, as it turns out to be useful only in a fraction of orderly use-cases. We can imagine other potential uses though, such as:

* Non-DBI-based database data extraction, or customised routines for pulling data from a database
* Download files from some shared location just before use (e.g., SharePoint, OneDrive, AWS). The `orderly_config.yml` would contain account connection details and `orderly.yml` would contain mapping between the remote data/files and local files. Rather than writing to the environment as we do above, use the `path` argument to copy files into the correct place.
* Pull data from some web API just before running

These all follow the same basic pattern of requiring some configuration in order to be able to connect to the resource service, some specification of what resources are to be fetched, and some action to actually fetch the resource and put it into place.
