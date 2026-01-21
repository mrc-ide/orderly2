# Declare a dependency

Declare a dependency on another packet

## Usage

``` r
orderly_dependency(name, query, files)
```

## Arguments

- name:

  The name of the packet to depend on

- query:

  The query to search for; often this will simply be the string
  `latest`, indicating the most recent version. You may want a more
  complex query here though.

- files:

  Files to copy from the other packet, as a character vector. If the
  character vector is unnamed, the files listed are copied over without
  changing their names. If the vector is named however, the names will
  be used as the destination name for the files.

  In either case, if you want to import a directory of files from a
  packet, you must refer to the source with a trailing slash (e.g.,
  `c(here = "there/")`), which will create the local directory
  `here/...` with files from the upstream packet directory `there/`. If
  you omit the slash then an error will be thrown suggesting that you
  add a slash if this is what you intended.

  You can use a limited form of string interpolation in the names of
  this argument; using `${variable}` will pick up values from `envir`
  and substitute them into your string. This is similar to the
  interpolation you might be familiar with from
  [`glue::glue`](https://glue.tidyverse.org/reference/glue.html) or
  similar, but much simpler with no concatenation or other fancy
  features supported.

  Note that there is an unfortunate, but (to us) avoidable inconsistency
  here; interpolation of values from your environment in the query is
  done by using `environment:x` and in the destination filename by doing
  `${x}`.

  If you want to copy *all* files from the packet, use `./` (read this
  as the directory of the packet). The trailing slash is required in
  order to be consistent with the rules above.

## Value

Undefined

## Details

See
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md)
for some details about how search options are used to select which
locations packets are found from, and if any data is fetched over the
network. If you are running interactively, this will obviously not work,
so you should use
[`orderly_interactive_set_search_options()`](https://mrc-ide.github.io/orderly/reference/orderly_interactive_set_search_options.md)
to set the options that this function will respond to.

## Examples

``` r
orderly_example_show("depends")
#> 
#> ── src/depends/depends.R ───────────────────────────────────────────────────────
#> # Depend on 'data.rds' from 'data' as 'xy.rds', using the most recent
#> # packet:
#> orderly_dependency("data", "latest", c("xy.rds" = "data.rds"))
#>  
#> # When the report runs, or when you run through interactively, the
#> # file 'xy.rds' will be present and you can ready it as normal
#> xy <- readRDS("xy.rds")
#>  
#> # The rest of the analysis would then proceed as usual
#> orderly_artefact("plot.png", description = "A plot of data")
#> png("plot.png")
#> plot(xy)
#> dev.off()
```
