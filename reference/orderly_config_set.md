# Set configuration options

Set configuration options. Not all can currently be set; this will be
expanded over time. See Details.

## Usage

``` r
orderly_config_set(..., options = list(...), root = NULL)
```

## Arguments

- ...:

  Named options to set (e.g., pass the argument
  `core.require_complete_tree = TRUE`)

- options:

  As an alternative to `...`, you can pass a list of named options here
  (e.g., `list(core.require_complete_tree = TRUE)`). This interface is
  typically easier to program against.

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function does not require
  that the directory is configured for orderly, and can be any `outpack`
  root (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

Nothing

## Details

Options are set in the order that they are provided. Currently, if
setting one option fails, no further options will be processed but
previous ones will be (do not rely on this behaviour, it may change).

Currently you can set:

- `core.require_complete_tree`

See
[`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
for description of these options.

## See also

orderly_config

## Examples

``` r
# The default configuration does not include a file store, and
# saves output within the "archive" directory:
path <- withr::local_tempdir()
orderly_init(path)
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/file1b9c31b50ccb'
fs::dir_tree(path, all = TRUE)
#> /tmp/RtmpoLRnek/file1b9c31b50ccb
#> ├── .outpack
#> │   ├── config.json
#> │   ├── location
#> │   └── metadata
#> └── orderly_config.json

# Change this after the fact:
orderly_config_set(core.use_file_store = TRUE,
                   core.path_archive = NULL,
                   root = path)
fs::dir_tree(path, all = TRUE)
#> /tmp/RtmpoLRnek/file1b9c31b50ccb
#> ├── .outpack
#> │   ├── config.json
#> │   ├── files
#> │   ├── index
#> │   │   └── outpack.rds
#> │   ├── location
#> │   └── metadata
#> └── orderly_config.json
```
