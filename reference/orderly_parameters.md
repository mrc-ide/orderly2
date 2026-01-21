# Declare orderly parameters

Declare orderly parameters. You should only have one call to this within
your file! Typically you'd put the call to this function very close to
the top of the file. Parameters are scalar atomic values (e.g. a string,
number or boolean) and defaults must be present literally (i.e., they
may not come from a variable itself). Provide `NULL` if you do not have
a default, in which case this parameter will be required.

## Usage

``` r
orderly_parameters(...)
```

## Arguments

- ...:

  Any number of parameters. All arguments must be named.

## Value

A list of parameters. This list is "strict" so accessing elements that
are not present will throw an error rather than returning `NULL`.

## Parameters and variables

Prior to orderly 1.99.61, parameters are always available as variables
in the execution environment. In order to harmonise the R and Python
versions of orderly, we are moving away from this, at least by default.
The recommended way of using parameters is to assign it to a variable,
for example:

    pars <- orderly_parameters(debug = FALSE, replicates = NULL)

This defines two parameters, `debug` (with a default) and `replicates`
(without a default). In the running report, you can access these by
subsetting the `pars` object (e.g., `pars$debug` or
`pars[["replicates"]]`).

To get the old behaviour, do not assign to a variable:

    orderly_parameters(debug = FALSE, replicates = NULL)

This will create two bindings in the environment (`debug` and
`replicates`) but will also generate a deprecation warning and we will
remove support in a release of orderly 2.x. If you really want the old
behaviour, you can achieve it by writing:

    pars <- orderly_parameters(debug = FALSE, replicates = NULL)
    list2env(pars, environment())

## Behaviour in interactive sessions

We want you to be able to run through an orderly report interactively,
e.g. via [`source()`](https://rdrr.io/r/base/source.html), by copy/paste
or via the "Run" or "Source" button in RStudio. This is not very
compatible with use of orderly parameters, because normally you'd
provide these to
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md),
so we need a mechanism to get the parameters from you.

The behaviour differs if you have assigned the result of
`orderly_parameters` to a variable or are using the (deprecated)
behaviour of exporting parameters as variables.

### New behaviour

Suppose that you are assigning to `pars`. The first time we run though
your code we won't see a value of `pars` and we'll prompt for values for
each parameter. Those that have default values in your list will offer
these values to make selection of parameters faster.

On subsequent calls, `pars` will be present with the values you used
previously; these will be reused. If you want to be re-prompted, delete
`pars` (i.e., `rm("pars")`) or assign `NULL` (i.e., `pars <- NULL`).

### Old behaviour

This is now deprecated, and you should update your code.

When running interactively (i.e., via
[`source()`](https://rdrr.io/r/base/source.html) or running an orderly
file session by copy/paste or in RStudio), the `orderly_parameters()`
function has different behaviour, and this behaviour depends on whether
parameters will be exported to the environment or not.

First, we look in the current environment (most likely the global
environment) for values of your parameters - that is, variables bound to
the names of your parameters. For any parameters that are not found we
will look at the default values and use these if possible, but if not
possible then we will either error or prompt based on the global option
`orderly.interactive_parameters_missing_error`. If this is `FALSE`, then
we will ask you to enter a value for the parameters (strings will need
to be entered with quotes).

## Examples

``` r
# An example in context within the orderly examples, using the
# recommended new-style syntax:
orderly_example_show("parameters")
#> 
#> ── src/parameters/parameters.R ─────────────────────────────────────────────────
#> # This declares that this orderly report accepts one parameter
#> # 'max_cyl' with no default (i.e., it is required).
#> pars <- orderly_parameters(max_cyl = NULL)
#> orderly_artefact("data.rds", description = "Final data")
#>  
#> # We can use the parameter by subsetting 'pars'; unlike regular R
#> # lists you will get an error if you try and access a non-existent
#> # element.
#> data <- mtcars[mtcars$cyl <= pars$max_cyl, ]
#> saveRDS(data, "data.rds")
```
