# Read outpack metadata

Read metadata for a particular id. You may want to use
[`orderly_search()`](https://mrc-ide.github.io/orderly/reference/orderly_search.md)
to find an id corresponding to a particular query.

## Usage

``` r
orderly_metadata(id, root = NULL)
```

## Arguments

- id:

  The id to fetch metadata for. An error will be thrown if this id is
  not known

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function does not require
  that the directory is configured for orderly, and can be any `outpack`
  root (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

A list of metadata. See the outpack schema for details
(<https://github.com/mrc-ide/outpack>)

## Examples

``` r
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c52536ed7'
id <- orderly_run("data", root = path)
#> ℹ Starting packet 'data' `20260121-095925-848bbb1d` at 2026-01-21 09:59:25.521863
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095925-848bbb1d at 2026-01-21 09:59:25.547264 (0.02540088 secs)

# Read metadata for this packet:
meta <- orderly_metadata(id, root = path)
names(meta)
#> [1] "schema_version" "name"           "id"             "time"          
#> [5] "parameters"     "files"          "depends"        "git"           
#> [9] "custom"        

# Information on files produced by this packet:
meta$files
#>       path size
#> 1   data.R  227
#> 2 data.rds  598
#>                                                                      hash
#> 1 sha256:64df52ebf4cfe4a1cf989f51280b22c3a6aa9aa06096afcf0a52135c704cc374
#> 2 sha256:39f8ea9fe552202421f1abca875cfec52afa3f29171c236339e0f9e037e6ce7c
```
