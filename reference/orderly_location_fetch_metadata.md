# Fetch metadata from a location

Fetch metadata from a location, updating the index. This should always
be relatively quick as it updates only small files that contain
information about what can be found in remote packets.

## Usage

``` r
orderly_location_fetch_metadata(location = NULL, root = NULL)
```

## Arguments

- location:

  The name of a location to pull from (see
  [`orderly_location_list()`](https://mrc-ide.github.io/orderly/reference/orderly_location_list.md)
  for possible values). If not given, pulls from all locations. The
  "local" and "orphan" locations are always up to date and pulling
  metadata from them does nothing.

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function does not require
  that the directory is configured for orderly, and can be any `outpack`
  root (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

Nothing

## Examples

``` r
# Two roots, one local and one representing some remote orderly location:
local <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9cdefa9d4'
remote <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c295c828e'

# We create a packet in the remote root:
orderly_run("data", root = remote)
#> ℹ Starting packet 'data' `20260121-095923-cb512f28` at 2026-01-21 09:59:23.798455
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095923-cb512f28 at 2026-01-21 09:59:23.82316 (0.02470541 secs)
#> [1] "20260121-095923-cb512f28"

# Add the remote as a path location to the local root:
orderly_location_add_path("remote", remote, root = local)
#> ℹ Testing location
#> ✔ Location configured successfully
#> ✔ Added location 'remote' (path)

# Pull metadata from 'remote' into our local version
orderly_location_fetch_metadata(root = local)
#> ℹ Fetching metadata from 1 location: 'remote'
#> ✔ Found 1 packet at 'remote', of which 1 is new
```
