# Describe the current packet

Describe the current packet

## Usage

``` r
orderly_description(display = NULL, long = NULL, custom = NULL)
```

## Arguments

- display:

  A friendly name for the report; this will be displayed in some
  locations of the web interface, packit. If given, it must be a scalar
  character.

- long:

  A longer description of the report. If given, it must be a scalar
  character.

- custom:

  Any additional metadata. If given, it must be a named list, with all
  elements being scalar atomics (character, number, logical).

## Value

Undefined

## Examples

``` r
# An example in context within the orderly examples:
orderly_example_show("data")
#> 
#> ── src/data/data.R ─────────────────────────────────────────────────────────────
#> orderly_description(
#>   display = "A demo data set")
#>  
#> x <- jitter(1:30)
#> y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> d <- data.frame(x, y)
#>  
#> orderly_artefact("data.rds", description = "A synthetic dataset")
#> saveRDS(d, "data.rds")
```
