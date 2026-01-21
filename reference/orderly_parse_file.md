# Parse the orderly entrypoint script

For expert use only.

## Usage

``` r
orderly_parse_file(path)

orderly_parse_expr(exprs, filename)
```

## Arguments

- path:

  Path to `orderly_*` script

- exprs:

  Parsed AST from `orderly_*` script

- filename:

  Name of `orderly_*` file to include in metadata

## Value

Parsed orderly entrypoint script

## Details

Parses details of any calls to the orderly\_ in-script functions into
intermediate representation for downstream use. Also validates that any
calls to `orderly_*` in-script functions are well-formed.

## Examples

``` r
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c27b192bd'
# About the simplest case
orderly_parse_file(file.path(path, "src", "data", "data.R"))
#> $entrypoint_filename
#> [1] "data.R"
#> 
#> $strict
#> $strict$enabled
#> [1] FALSE
#> 
#> 
#> $artefacts
#> $artefacts[[1]]
#> $artefacts[[1]]$description
#> [1] "A synthetic dataset"
#> 
#> $artefacts[[1]]$files
#> [1] "data.rds"
#> 
#> 
#> 

# Find out about parameters
orderly_parse_file(file.path(path, "src", "parameters", "parameters.R"))
#> $entrypoint_filename
#> [1] "parameters.R"
#> 
#> $strict
#> $strict$enabled
#> [1] FALSE
#> 
#> 
#> $parameters
#> $parameters$max_cyl
#> NULL
#> 
#> 
#> $parameters_target
#> [1] "pars"
#> 
#> $artefacts
#> $artefacts[[1]]
#> $artefacts[[1]]$description
#> [1] "Final data"
#> 
#> $artefacts[[1]]$files
#> [1] "data.rds"
#> 
#> 
#> 

# Find out about dependencies:
orderly_parse_file(file.path(path, "src", "depends", "depends.R"))
#> $entrypoint_filename
#> [1] "depends.R"
#> 
#> $strict
#> $strict$enabled
#> [1] FALSE
#> 
#> 
#> $artefacts
#> $artefacts[[1]]
#> $artefacts[[1]]$description
#> [1] "A plot of data"
#> 
#> $artefacts[[1]]$files
#> [1] "plot.png"
#> 
#> 
#> 
#> $dependency
#> $dependency[[1]]
#> $dependency[[1]]$name
#> [1] "data"
#> 
#> $dependency[[1]]$query
#> [1] "latest"
#> 
#> $dependency[[1]]$files
#>     xy.rds 
#> "data.rds" 
#> 
#> 
#> 
```
