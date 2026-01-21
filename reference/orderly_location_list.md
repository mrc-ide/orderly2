# List known pack locations

List known locations. The special name `local` will always be present
within the output from this function (this is packets known at the
current root), though you will typically be interested in *other*
locations.

## Usage

``` r
orderly_location_list(verbose = FALSE, root = NULL)
```

## Arguments

- verbose:

  Logical, indicating if we should return a data.frame that includes
  more information about the location.

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function does not require
  that the directory is configured for orderly, and can be any `outpack`
  root (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

Depending on the value of `verbose`:

- `verbose = FALSE`: A character vector of location names. This is the
  default behaviour.

- `verbose = TRUE`: A data.frame with columns `name`, `type` and `args`.
  The `args` column is a list column, with each element being the
  key-value pair arguments to the location.

## See also

[`orderly_location_fetch_metadata()`](https://mrc-ide.github.io/orderly/reference/orderly_location_fetch_metadata.md),
which can update your outpack index with metadata from any of the
locations listed here.

## Examples

``` r
# Two roots, one local and one representing some remote orderly location:
local <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c26e21790'
remote <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c6dc57956'

# No locations at first
orderly_location_list(root = local)
#> [1] "local"

# Add a location
orderly_location_add_path("remote", remote, root = local)
#> ℹ Testing location
#> ✔ Location configured successfully
#> ✔ Added location 'remote' (path)

# Here it is!
orderly_location_list(root = local)
#> [1] "local"  "remote"

# Add vebose = TRUE to find more about the location
orderly_location_list(root = local)
#> [1] "local"  "remote"
```
