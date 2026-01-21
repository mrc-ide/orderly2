# Initialise an orderly repository

Initialise an empty orderly repository, or initialise a source copy of
an orderly repository (see Details). An orderly repository is defined by
the presence of a file `orderly_config.json` (or `orderly_config.yml`)
at its root, along with a directory `.outpack/` at the same level.

## Usage

``` r
orderly_init(
  root = ".",
  path_archive = "archive",
  use_file_store = FALSE,
  require_complete_tree = FALSE,
  force = FALSE
)
```

## Arguments

- root:

  The path to initialise the repository root at. If the repository is
  already initialised, this operation checks that the options passed in
  are the same as those set in the repository (erroring if not), but
  otherwise does nothing. The default path is the current working
  directory.

- path_archive:

  Path to the archive directory, used to store human-readable copies of
  packets. If `NULL`, no such copy is made, and `file_store` must be
  `TRUE`

- use_file_store:

  Logical, indicating if we should use a content-addressable file-store
  as the source of truth for packets. If `archive` is non-`NULL`, the
  file-store will be used as the source of truth and the duplicated
  files in archive exist only for convenience.

- require_complete_tree:

  Logical, indicating if we require a complete tree of packets. This
  currently affects
  [`orderly_location_pull()`](https://mrc-ide.github.io/orderly/reference/orderly_location_pull.md),
  by requiring that it always operates in recursive mode. This is
  `FALSE` by default, but set to `TRUE` if you want your archive to
  behave well as a location; if `TRUE` you will always have all the
  packets that you hold metadata about.

- force:

  Logical, indicating if we should initialise orderly even if the
  directory is not empty.

## Value

The full, normalised, path to the root, invisibly. Typically this is
called only for its side effect.

## Details

It is expected that `orderly_config.json` will be saved in version
control, but that `.outpack` will be excluded from version control; this
means that for every clone of your project you will need to call
`orderly_init()` to initialise the `.outpack` directory. If you forget
to do this, an error will be thrown reminding you of what you need to
do.

You can safely call `orderly_init()` on an already-initialised
directory, however, any arguments passed through must exactly match the
configuration of the current root, otherwise an error will be thrown.
Please use
[`orderly_config_set()`](https://mrc-ide.github.io/orderly/reference/orderly_config_set.md)
to change the configuration within `.outpack`, as this ensures that the
change in configuration is possible. If configuration options are given
but match those that the directory already uses, then nothing happens.
You can safely edit `orderly_config.json` yourself, at least for now.

If the repository that you call `orderly_init()` on is already
initialised with an `.outpack` directory but not an
`orderly_config.json` file, then we will write that file too.

## Examples

``` r
# We'll use an automatically cleaned-up directory for the root:
path <- withr::local_tempdir()

# Initialise a new repository, setting an option:
orderly_init(path, use_file_store = TRUE)
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/file1b9c1e266b99'

fs::dir_tree(path, all = TRUE)
#> /tmp/RtmpoLRnek/file1b9c1e266b99
#> ├── .outpack
#> │   ├── config.json
#> │   ├── files
#> │   ├── location
#> │   └── metadata
#> └── orderly_config.json
```
