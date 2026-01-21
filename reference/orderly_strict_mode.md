# Enable orderly strict mode

Put orderly into "strict mode", which is closer to the defaults in
orderly 1.0.0; in this mode only explicitly included files (via
[`orderly_resource()`](https://mrc-ide.github.io/orderly/reference/orderly_resource.md)
and
[`orderly_shared_resource()`](https://mrc-ide.github.io/orderly/reference/orderly_shared_resource.md))
are copied when running a packet, and we warn about any unexpected files
at the end of the run. Using strict mode allows orderly to be more
aggressive in how it deletes files within the source directory, more
accurate in what it reports to you, and faster to start packets after
developing them interactively.

## Usage

``` r
orderly_strict_mode()
```

## Value

Undefined

## Details

In future, we may extend strict mode to allow requiring that no
computation occurs within orderly functions (i.e., that the requirements
to run a packet are fully known before actually running it). Most likely
this will *not* be the default behaviour and `orderly_strict_mode()`
will gain an argument.

We will allow server processes to either override this value (enabling
it even when it is not explicitly given) and/or require it.

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
