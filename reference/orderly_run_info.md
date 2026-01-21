# Information about currently running report

Fetch information about the actively running report. This allows you to
reflect information about your report back as part of the report, for
example embedding the current report id, or information about computed
dependencies. This information is in a slightly different format to
orderly version 1.x and does not (currently) include information about
dependencies when run outside of
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md),
but this was never reliable previously.

## Usage

``` r
orderly_run_info()
```

## Value

A list with elements

- `name`: The name of the current report

- `id`: The id of the current report, `NA` if running interactively

- `root`: The orderly root path

- `depends`: A data frame with information about the dependencies (not
  available interactively)

  - `index`: an integer sequence along calls to
    [`orderly_dependency()`](https://mrc-ide.github.io/orderly/reference/orderly_dependency.md)

  - `name`: the name of the dependency

  - `query`: the query used to find the dependency

  - `id`: the computed id of the included packet

  - `filename`: the file used from the packet

  - `as`: the filename used locally

## Examples

``` r
# An example from the orderly examples
orderly_example_show("run_info")
#> 
#> ── src/run_info/run_info.R ─────────────────────────────────────────────────────
#> orderly_dependency("data", "latest", c("xy.rds" = "data.rds"))
#> xy <- readRDS("xy.rds")
#>  
#> info <- orderly_run_info()
#> print(info)
#>  
#> orderly_artefact("plot.png", description = "A plot of data")
#> png("plot.png")
#> plot(xy)
#> dev.off()

# Prepare to run
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c45fe53d7'
orderly_run("data", root = path, echo = FALSE)
#> ℹ Starting packet 'data' `20260121-095934-a39092eb` at 2026-01-21 09:59:34.64316
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095934-a39092eb at 2026-01-21 09:59:34.667723 (0.02456307 secs)
#> [1] "20260121-095934-a39092eb"

# Here, see the printed information from a real running report
orderly_run("run_info", root = path)
#> ℹ Starting packet 'run_info' `20260121-095934-b0113ced` at 2026-01-21 09:59:34.691859
#> > orderly_dependency("data", "latest", c("xy.rds" = "data.rds"))
#> ℹ Depending on data @ `20260121-095934-a39092eb` (via latest(name == "data"))
#> > xy <- readRDS("xy.rds")
#> > info <- orderly_run_info()
#> > print(info)
#> $name
#> [1] "run_info"
#> 
#> $id
#> [1] "20260121-095934-b0113ced"
#> 
#> $root
#> [1] "/tmp/RtmpoLRnek/orderly_ex_1b9c45fe53d7"
#> 
#> $depends
#>   index name                  query                       id    there   here
#> 1     1 data latest(name == "data") 20260121-095934-a39092eb data.rds xy.rds
#> 
#> > orderly_artefact("plot.png", description = "A plot of data")
#> > png("plot.png")
#> > plot(xy)
#> > dev.off()
#> agg_record_1411383854 
#>                     2 
#> ✔ Finished running run_info.R
#> ℹ Finished 20260121-095934-b0113ced at 2026-01-21 09:59:34.767721 (0.07586193 secs)
#> [1] "20260121-095934-b0113ced"
```
