# List source reports

List source reports - that is, directories within `src/` that look
suitable for running with orderly; these will be directories that
contain an entrypoint file - a `.R` file with the same name as the
directory (e.g., `src/data/data.R` corresponds to `data`).

## Usage

``` r
orderly_list_src(root = NULL)
```

## Arguments

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function **does** require
  that the directory is configured for orderly, and not just outpack
  (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

A character vector of names of source reports, suitable for passing to
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md)

## See also

[`orderly_metadata_extract()`](https://mrc-ide.github.io/orderly/reference/orderly_metadata_extract.md)
for listing packets that have completed

## Examples

``` r
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c3f3f5734'
orderly_list_src(root = path)
#> [1] "data"       "depends"    "parameters" "run_info"   "shared"    
#> [6] "strict"    
```
