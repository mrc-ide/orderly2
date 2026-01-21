# Declare orderly resources

Declare that a file, or group of files, are an orderly resource. By
explicitly declaring files as resources orderly will mark the files as
immutable inputs and validate that your analysis does not modify them
when run with
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md)

## Usage

``` r
orderly_resource(files)
```

## Arguments

- files:

  Any number of names of files or directories. If you list a directory
  it is expanded recursively to include all subdirectories and files.

## Value

Invisibly, a character vector of resources included by the call. Don't
rely on the order of these files if they are expanded from directories,
as this is likely platform dependent. If a path was not found, then we
throw an error.

## Examples

``` r
# An example in context within the orderly examples:
orderly_example_show("strict")
#> 
#> ── src/strict/strict.R ─────────────────────────────────────────────────────────
#> ## An example showing how strict mode works
#>  
#> ## Enable strict mode here:
#> orderly_strict_mode()
#>  
#> ## We declare that 'data.csv' is a resource; it will be copied over
#> ## into the working directory when this packet is run.
#> orderly_resource("data.csv")
#>  
#> ## If the line above is omitted then the run will fail here where we
#> ## try to read the data:
#> d <- read.csv("data.csv")
#>  
#> orderly_artefact("plot.png", description = "A plot of data")
#> png("plot.png")
#> plot(d)
#> dev.off()
```
