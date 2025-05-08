# orderly2

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R build status](https://github.com/mrc-ide/orderly2/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/orderly2/actions)
[![CodeFactor](https://www.codefactor.io/repository/github/mrc-ide/orderly2/badge)](https://www.codefactor.io/repository/github/mrc-ide/orderly2)
[![codecov.io](https://codecov.io/github/mrc-ide/orderly2/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/orderly2?branch=main)
<!-- badges: end -->

> 1. an attendant in a hospital responsible for the non-medical care of patients and the maintenance of order and cleanliness.
> 2. a soldier who carries orders or performs minor tasks for an officer.

`orderly2` is a package designed to help make analysis more reproducible.  Its principal aim is to automate a series of basic steps in the process of writing analyses, making it easy to:

* track all inputs into an analysis (packages, code, and data resources)
* store multiple versions of an analysis where it is repeated
* track outputs of an analysis
* create analyses that depend on the outputs of previous analyses

## Get started

Two vignettes provide an overview of the package, depending on your tastes:

* `vignette("orderly2")` describes the problem that `orderly2` tries to solve ([read on package website](https://mrc-ide.github.io/orderly2/articles/orderly2.html))
* `vignette("introduction")` describes `orderly2` by example, walking through the basic features ([read on package website](https://mrc-ide.github.io/orderly2/articles/introduction.html))

## Roadmap

This package is a ground-up rewrite of our [`orderly`](https://vaccineimpact.org/orderly) package, which is now ready for use on new projects or by adventurous users of the original package.

Over the next few months `orderly` will be renamed `orderly1`, and we will finalise the API for this package.  Once stable, we will rename it back from `orderly2` to `orderly` and release to CRAN to replace the existing package. A migration path for existing users is described in `vignette("migrating")` and will be updated based on our experience migrating people.

The web framework [`OrderlyWeb`](https://github.com/vimc/orderly-web) is being rewritten in parallel (to [`packit`](https://github.com/mrc-ide/packit)) and we will provide a migration path here too.

## Installation

Please install from our [r-universe](https://mrc-ide.r-universe.dev/):

```r
install.packages(
  "orderly2",
  repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
```

If you prefer, you can install from GitHub with `remotes`:

```r
remotes::install_github("mrc-ide/orderly2")
```

## Testing

To run all tests, you need to have [`outpack_server`](https://github.com/mrc-ide/outpack_server) available on your system path. One way to achieve this is to run

```
cargo install --git https://github.com/mrc-ide/outpack_server
```

For all tests we also require development versions of [`gert`](https://github.com/r-lib/gert/) and [`jsonvalidate`](https://github.com/ropensci/jsonvalidate/). These can be installed from the [rOpenSci r-universe](https://ropensci.r-universe.dev), with:

```r
install.packages(
  c("gert", "jsonvalidate"),
  repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org"))
```

Without these, some tests will be skipped, but the suite will still run.

If you want to force validation of schemas during testing, set the R option `outpack.schema_validate` to `TRUE`.  This will automatically be set on CI (as detected by the `CI` environment variable) and will be enabled if `jsonvalidate` is installed.  Set the option `outpack.schema_validate` as `FALSE` to disable checking.

## Licence

MIT © Imperial College of Science, Technology and Medicine
