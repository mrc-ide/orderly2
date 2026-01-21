# Declare orderly artefacts

Declare an artefact. By doing this you turn on a number of orderly
features; see Details below. You can have multiple calls to this
function within your orderly script.

## Usage

``` r
orderly_artefact(description = NULL, files)
```

## Arguments

- description:

  The name of the artefact

- files:

  The files within this artefact

## Value

Undefined

## Details

\(1\) files matching this will *not* be copied over from the src
directory to the draft directory unless they are also listed as a
resource with
[`orderly_resource()`](https://mrc-ide.github.io/orderly/reference/orderly_resource.md).
This feature is only enabled if you call this function from the top
level of the orderly script and if it contains only string literals (no
variables).

\(2\) if your script fails to produce these files, then
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md)
will fail, guaranteeing that your task does really produce the things
you need it to.

\(3\) within the final metadata, your artefacts will have additional
metadata; the description that you provide and a grouping

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
