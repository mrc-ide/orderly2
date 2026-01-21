# Rename a location

Rename an existing location

## Usage

``` r
orderly_location_rename(old, new, root = NULL)
```

## Arguments

- old:

  The current short name of the location. Cannot rename `local` or
  `orphan`

- new:

  The desired short name of the location. Cannot be one of `local` or
  `orphan`

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
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c4c767d21'
remote <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c5e76169e'
orderly_location_add_path("remote", remote, root = local)
#> ℹ Testing location
#> ✔ Location configured successfully
#> ✔ Added location 'remote' (path)

orderly_location_list(root = local, verbose = TRUE)
#>     name  type         args
#> 1  local local             
#> 2 remote  path /tmp/Rtm....

# Rename the remote location:
orderly_location_rename("remote", "bob", root = local)
orderly_location_list(root = local, verbose = TRUE)
#>    name  type         args
#> 1 local local             
#> 2   bob  path /tmp/Rtm....
```
