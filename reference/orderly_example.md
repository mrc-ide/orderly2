# Copy a simple orderly example

Copy a simple orderly example for use in the docs. This function should
not form part of your workflow!

## Usage

``` r
orderly_example(..., names = NULL, example = "demo", dest = NULL)
```

## Arguments

- ...:

  Arguments passed through to
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)

- names:

  Optionally, names of the reports to copy. The default is to copy all
  reports.

- example:

  The name of the example to copy. Currently only "simple" and "demo"
  are supported.

- dest:

  The destination. By default we use
  [`withr::local_tempdir()`](https://withr.r-lib.org/reference/with_tempfile.html)
  which will create a temporary directory that will clean itself up.
  This is suitable for use from the orderly examples, but you may prefer
  to provide your own path, in which case the path must not already
  exist.

## Value

Invisibly, the path to the example.

## Examples

``` r
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c5a7fd283'
orderly_list_src(root = path)
#> [1] "data"       "depends"    "parameters" "run_info"   "shared"    
#> [6] "strict"    
```
