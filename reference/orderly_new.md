# Create a new report

Create a new empty report.

## Usage

``` r
orderly_new(name, template = NULL, force = FALSE, root = NULL)
```

## Arguments

- name:

  The name of the report

- template:

  The template to use. The only acceptable values for now are `NULL`
  (uses the built-in default) and `FALSE` which suppresses any default
  content. We may support customisable templates in future - let us know
  if this would be useful.

- force:

  Create an orderly file - `<name>.R` within an existing directory
  `src/<name>`; this may be useful if you have already created the
  directory and some files first but want help creating the orderly
  file.

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function **does** require
  that the directory is configured for orderly, and not just outpack
  (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

Nothing, called for its side effects only

## Examples

``` r
path <- withr::local_tempdir()

# Initialise a new repository, setting an option:
orderly_init(path)
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/file1b9c3eec823'

# Create a new report 'myreport' in this root:
orderly_new("myreport", root = path)
#> ✔ Created 'src/myreport/myreport.R'

# We now see:
fs::dir_tree(path, all = TRUE)
#> /tmp/RtmpoLRnek/file1b9c3eec823
#> ├── .outpack
#> │   ├── config.json
#> │   ├── location
#> │   └── metadata
#> ├── orderly_config.json
#> └── src
#>     └── myreport
#>         └── myreport.R

# By default, the new path will contain some hints, you can
# customise this by writing a template
cli::cli_code(readLines(file.path(path, "src", "myreport", "myreport.R")))
#> # This is an orderly script - edit it to suit your needs. You might include
#> #
#> # * orderly::orderly_parameters():
#> #       declare parameters that your report accepts
#> # * orderly::orderly_description():
#> #       describe your report with friendly names, descriptions and metadata
#> # * orderly::orderly_resource():
#> #       declare files in your source tree that are inputs
#> # * orderly::orderly_shared_resource():
#> #       use files from the root directory's 'shared/' directory
#> # * orderly::orderly_dependency():
#> #       use files from a previously-run packet
#> # * orderly::orderly_artefact():
#> #       declare files that you promise to produce, and describe them
#> # * orderly::orderly_strict_mode():
#> #       enable some optional checks
#> #
#> # See the docs for more information:
#> #     https://mrc-ide.github.io/orderly/reference/
#> #
#> # To generate templates without this header, pass template = FALSE to
#> # orderly_new(); this header can be safely deleted if you don't need it.
```
