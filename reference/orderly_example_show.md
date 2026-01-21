# Show an example file

Show a file from within one of the examples. This function exists for
use within orderly help files, vignettes and tutorials and is not meant
to form part of your workflows, unless you are doing something very
peculiar.

## Usage

``` r
orderly_example_show(name, file = NULL, example = "demo")
```

## Arguments

- name:

  The name of the report within the example.

- file:

  The name of the file within the report. The default is to show the
  main orderly file (i.e., `<name>.R`)

- example:

  The name of the example to look in. The default `demo` is a sprawling
  set of source designed to show off different orderly features.

## Value

Nothing, called for its side effects only.

## Details

All orderly examples here are runnable, though some will naturally have
some pre-requisites (e.g., using a dependency will require that the
dependency has been run first).

## Examples

``` r
# We use constructions like this in the help, to show off features
# of orderly:
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

# You can run this example:
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c69855db9'
orderly_run("data", root = path)
#> ℹ Starting packet 'data' `20260121-095921-b45458cd` at 2026-01-21 09:59:21.70857
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095921-b45458cd at 2026-01-21 09:59:21.733058 (0.02448821 secs)
#> [1] "20260121-095921-b45458cd"
```
