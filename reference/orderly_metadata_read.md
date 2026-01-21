# Read outpack metadata json file

Low-level function for reading metadata and deserialising it. This
function can be used to directly read a metadata json file without
reference to a root which contains it. It may be useful in the context
of reading a metadata file written out as part of a failed run.

## Usage

``` r
orderly_metadata_read(path, plugins = TRUE)
```

## Arguments

- path:

  Path to the json file

- plugins:

  Try and deserialise data from all loaded plugins (see Details).

## Value

A list of outpack metadata; see the schema for details. In contrast to
reading the json file directly with
[`jsonlite::fromJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html),
this function will take care to convert scalar and length-one vectors
into the expected types.

## Details

Custom metadata saved by plugins may not be deserialised as expected
when called with this function, as it is designed to operate separately
from a valid orderly root (i.e., it will load data from any file
regardless of where it came from). If `plugins` is `TRUE` (the default)
then we will deserialise all data that matches any loaded plugin. This
means that the behaviour of this function depends on if you have loaded
the plugin packages. You can force this by running
[`orderly_config()`](https://mrc-ide.github.io/orderly/reference/orderly_config.md)
within any orderly directory, which will load any declared plugins.

## Examples

``` r
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9cee6460d'
id <- orderly_run("data", root = path)
#> ℹ Starting packet 'data' `20260121-095927-9bb466ba` at 2026-01-21 09:59:27.612265
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095927-9bb466ba at 2026-01-21 09:59:27.636783 (0.02451825 secs)
meta <- orderly_metadata_read(file.path(path, ".outpack", "metadata", id))
identical(meta, orderly_metadata(id, root = path))
#> [1] TRUE
```
