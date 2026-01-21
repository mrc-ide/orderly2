# Remove a location

Remove an existing location. Any packets from this location and not
known elsewhere will now be associated with the 'orphan' location
instead.

## Usage

``` r
orderly_location_remove(name, root = NULL)
```

## Arguments

- name:

  The short name of the location. Cannot remove `local` or `orphan`

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
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c2958e0a4'
remote <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c40ce7f25'
orderly_location_add_path("remote", remote, root = local)
#> ℹ Testing location
#> ✔ Location configured successfully
#> ✔ Added location 'remote' (path)

orderly_location_list(root = local)
#> [1] "local"  "remote"

# Remove the remote location:
orderly_location_remove("remote", root = local)
orderly_location_list(root = local)
#> [1] "local"
```
