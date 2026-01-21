# orderly

> 1.  an attendant in a hospital responsible for the non-medical care of
>     patients and the maintenance of order and cleanliness.
> 2.  a soldier who carries orders or performs minor tasks for an
>     officer.

`orderly` is a package designed to help make analysis more reproducible.
Its principal aim is to automate a series of basic steps in the process
of writing analyses, making it easy to:

- track all inputs into an analysis (packages, code, and data resources)
- store multiple versions of an analysis where it is repeated
- track outputs of an analysis
- create analyses that depend on the outputs of previous analyses

You can [watch a short
talk](https://www.youtube.com/watch?v=lkPgihFQbrk) that describes the
ideas in the package, and why it differs to other approaches to
reproducibility and workflows.

## Get started

Two vignettes provide an overview of the package, depending on your
tastes:

- [`vignette("orderly")`](https://mrc-ide.github.io/orderly/articles/orderly.md)
  describes the problem that `orderly` tries to solve ([read on package
  website](https://mrc-ide.github.io/orderly/articles/orderly.html))
- [`vignette("introduction")`](https://mrc-ide.github.io/orderly/articles/introduction.md)
  describes `orderly` by example, walking through the basic features
  ([read on package
  website](https://mrc-ide.github.io/orderly/articles/introduction.html))

There is also an [orderly
tutorial](https://mrc-ide.github.io/orderly-tutorial/) from a 3-hour
interactive session.

## Installation

Install `orderly` from CRAN with

    install.packages("orderly")

To get the current development version, please install from our
[r-universe](https://mrc-ide.r-universe.dev/):

``` r
install.packages(
  "orderly",
  repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
```

If you prefer, you can install from GitHub with `remotes`:

``` r
remotes::install_github("mrc-ide/orderly", upgrade = FALSE)
```

## History

This package is a ground-up rewrite of our original [`orderly` version
1](https://vimc.github.io/orderly1/) package, which is now ready for use
on all projects, including migrating projects created with the original
package.

The original `orderly` package has now been renamed
[`orderly1`](https://vimc.github.io/orderly1/), and will be preserved
and installable for use with historical projects without the need to
migrate, though this version of the package is no longer available from
CRAN.

A migration path for existing users is described in
[`vignette("migrating")`](https://mrc-ide.github.io/orderly/articles/migrating.md).

The web framework [`OrderlyWeb`](https://github.com/vimc/orderly-web)
has been rewritten as [`packit`](https://github.com/mrc-ide/packit).

## Testing

To run all tests, you need to have
[`outpack_server`](https://github.com/mrc-ide/outpack_server) available
on your system path. One way to achieve this is to run

    cargo install --git https://github.com/mrc-ide/outpack_server

If you want to force validation of schemas during testing, set the R
option `orderly.schema_validate` to `TRUE`. This will automatically be
set on CI (as detected by the `CI` environment variable) and will be
enabled if `jsonvalidate` is installed. Set the option
`orderly.schema_validate` as `FALSE` to disable checking.

## Licence

MIT © Imperial College of Science, Technology and Medicine
