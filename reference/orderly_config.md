# Read configuration

Read the current orderly configuration, stored within the outpack root,
along with any orderly-specific extensions.

## Usage

``` r
orderly_config(root = NULL)
```

## Arguments

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function does not require
  that the directory is configured for orderly, and can be any `outpack`
  root (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

A list of configuration options:

- `core`: The most important options about the outpack store,
  containing:

  - `path_archive`: The path to the human-readable packet archive, or
    `NULL` if disabled (set in
    [`orderly_config_set()`](https://mrc-ide.github.io/orderly/reference/orderly_config_set.md)
    as `core.path_archive`)

  - `use_file_store`: Indicates if a content-addressable file store is
    enabled (`core.use_file_store`)

  - `require_complete_tree`: Indicates if this outpack store requires
    all dependencies to be fully available
    (`core.require_complete_tree`)

  - `hash_algorithm`: The hash algorithm used (currently not modifiable)

- `location`: Information about locations; see
  [`orderly_location_add()`](https://mrc-ide.github.io/orderly/reference/orderly_location_add.md),
  [`orderly_location_rename()`](https://mrc-ide.github.io/orderly/reference/orderly_location_rename.md)
  and
  [`orderly_location_remove()`](https://mrc-ide.github.io/orderly/reference/orderly_location_remove.md)
  to interact with this configuration, or
  [`orderly_location_list()`](https://mrc-ide.github.io/orderly/reference/orderly_location_list.md)
  to more simply list available locations. Returns as a
  [data.frame](https://rdrr.io/r/base/data.frame.html) with columns
  `name`, `id`, `priority`, `type` and `args`, with `args` being a list
  column.

- `orderly`: A list of orderly-specific configuration; this is just the
  minimum required version (as `minimum_orderly_version`).

## Examples

``` r
# A default configuration in a new temporary directory
path <- withr::local_tempdir()
orderly_init(path)
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/file1b9c2b429df3'
orderly_config(path)
#> $core
#> $core$path_archive
#> [1] "archive"
#> 
#> $core$use_file_store
#> [1] FALSE
#> 
#> $core$require_complete_tree
#> [1] FALSE
#> 
#> $core$hash_algorithm
#> [1] "sha256"
#> 
#> 
#> $location
#>    name  type args
#> 1 local local     
#> 
#> $orderly
#> $orderly$minimum_orderly_version
#> [1] ‘2.0.0’
#> 
#> attr(,"filename")
#> [1] "/tmp/RtmpoLRnek/file1b9c2b429df3/orderly_config.json"
#> 
```
