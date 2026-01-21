# Copy shared resources into a packet directory

Copy shared resources into a packet directory. You can use this to share
common resources (data or code) between multiple packets. Additional
metadata will be added to keep track of where the files came from. Using
this function requires the shared resources directory `shared/` exists
at the orderly root; an error will be raised if this is not configured
when we attempt to fetch files.

## Usage

``` r
orderly_shared_resource(...)
```

## Arguments

- ...:

  The shared resources to copy. If arguments are named, the name will be
  the destination file while the value is the filename within the shared
  resource directory.

  You can use a limited form of string interpolation in the names of
  this argument; using `${variable}` will pick up values from `envir`
  and substitute them into your string. This is similar to the
  interpolation you might be familiar with from
  [`glue::glue`](https://glue.tidyverse.org/reference/glue.html) or
  similar, but much simpler with no concatenation or other fancy
  features supported.

## Value

Invisibly, a data.frame with columns `here` (the filenames as as copied
into the running packet) and `there` (the filenames within `shared/`).
Do not rely on the ordering where directory expansion was performed.

## Examples

``` r
# An example in context within the orderly examples:
orderly_example_show("shared")
#> 
#> ── src/shared/shared.R ─────────────────────────────────────────────────────────
#> # Pull in the file 'shared/palette.R' as 'cols.R'
#> orderly_shared_resource(cols.R = "palette.R")
#>  
#> # Then source it, as usual
#> source("cols.R")
#>  
#> # And use the function 'palette()' found within
#> png("volcano.png")
#> image(volcano, col = palette())
#> dev.off()

# Here's the directory structure for this example:
path <- orderly_example(names = "shared")
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c65a8972f'
fs::dir_tree(path)
#> /tmp/RtmpoLRnek/orderly_ex_1b9c65a8972f
#> ├── orderly_config.json
#> ├── shared
#> │   └── palette.R
#> └── src
#>     └── shared
#>         └── shared.R

# We can run this packet:
orderly_run("shared", root = path)
#> ℹ Starting packet 'shared' `20260121-095935-cb4901fe` at 2026-01-21 09:59:35.798205
#> > # Pull in the file 'shared/palette.R' as 'cols.R'
#> > orderly_shared_resource(cols.R = "palette.R")
#> > # Then source it, as usual
#> > source("cols.R")
#> > # And use the function 'palette()' found within
#> > png("volcano.png")
#> > image(volcano, col = palette())
#> > dev.off()
#> agg_record_2080293431 
#>                     2 
#> ✔ Finished running shared.R
#> ℹ Finished 20260121-095935-cb4901fe at 2026-01-21 09:59:35.840971 (0.04276562 secs)
#> [1] "20260121-095935-cb4901fe"

# In the final archive version of the packet, 'cols.R' is copied
# over from `shared/`, so we have a copy of the version of the code
# that was used in the analysis
fs::dir_tree(path)
#> /tmp/RtmpoLRnek/orderly_ex_1b9c65a8972f
#> ├── archive
#> │   └── shared
#> │       └── 20260121-095935-cb4901fe
#> │           ├── cols.R
#> │           ├── shared.R
#> │           └── volcano.png
#> ├── draft
#> │   └── shared
#> ├── orderly_config.json
#> ├── shared
#> │   └── palette.R
#> └── src
#>     └── shared
#>         └── shared.R
```
