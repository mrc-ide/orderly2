# Migrating from orderly (1.x)

The new version of `orderly` (version 1.99.x, known as `orderly2` during
development) is very different to the previously released version on
CRAN (orderly 1.4.3; September 2021) or the last development version of
the 1.x line (orderly 1.6.x; June 2023; now archived as
[`orderly1`](https://github.com/vimc/orderly1)). These changes
constitute a ground-up rewrite in order to bring out the best features
we found that `orderly` enabled within workflows, while removing some
features we felt have outlived their usefulness. This is disruptive
change, but we hope that it will be worth it.

This vignette is divided into two parts; one covers the conceptual
differences between `orderly1` and `orderly` while the second covers the
mechanical process of migrating from an existing `orderly` source tree
and archive to take advantage of the new features.

If you have never used version 1.x of `orderly`, you should not read
this document unless you are curious about the history of design
decisions. Instead you should read the introductory vignette
([`vignette("orderly")`](https://mrc-ide.github.io/orderly/articles/orderly.md)).

## Summary of changes

### So long YAML and thanks for all the whitespace errors

The most obvious user-facing change is that there is (almost) no
[YAML](https://en.wikipedia.org/wiki/YAML), with the definition of
inputs and outputs for a report now defined within an `orderly` file,
`<reportname>.R`. So an `orderly` report that previously had an
`orderly.yml` file that looked like

``` yaml
parameters:
  n_min:
    default: 10
script: script.R
source:
  - functions.R
resources:
  - metadata.csv
depends:
  raw_data:
    id: latest
    use:
      raw_data.csv: data.csv
artefacts:
  data:
    description: Processed data
    filenames: data.rds
```

would end up within an `orderly` file that looks like:

``` r
pars <- orderly_parameters(n_min = 10)
orderly_dependency("raw_data", "latest",
                            files = c("raw_data.csv" = "data.csv"))
orderly_resource("metadata.csv")
orderly_artefact(description = "Processed data", "data.rds")
source("functions.R")
```

We think this is much clearer, and comes with documentation and
autocomplete support in most IDEs.

In fact, for simple reports, **no** special functions are required,
though you’ll find that some will be useful (see
[`vignette("orderly")`](https://mrc-ide.github.io/orderly/articles/orderly.md))

Some specific changes:

- you no longer need to list packages with `packages:`, instead just use
  [`library()`](https://rdrr.io/r/base/library.html) as in an ordinary
  script. We will record the state of the session regardless so you will
  get a record of what was used
- you no longer need to use `sources:` to list scripts you want to
  source, instead use [`source()`](https://rdrr.io/r/base/source.html)
  as normal
- `global_resources` has become `orderly_shared_resource` (these aren’t
  really global so much as shared). Note that the directory they are in
  is now always `shared/` at the `orderly` root, you may not configure
  it.

#### Implications

This change has widespread implications:

- you can program against things like dependencies, creating a `for`
  loop over a series of parameter values, or conditionally depending on
  other reports
- any R script can be the basis of an `orderly` report, and you can add
  `orderly` functions annotating inputs and outputs progressively

### Database support has been moved into a plugin

In version 1, we had built-in support for accessing data from SQL
databases; this has moved within the
[`orderly.db`](https://github.com/mrc-ide/orderly.db) plugin. All major
features are supported.

### No more commit

`orderly` no longer requires a separate `orderly_commit()` call after
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md);
we no longer make a distinction between local draft and archive packets.
Instead, we have added finer-grained control over where dependencies are
resolved from (locally, or from some subset of your servers), which
generalises the way that draft/archive was used in practice. See
[`?orderly_run`](https://mrc-ide.github.io/orderly/reference/orderly_run.md)
for more details on how dependencies are resolved.

This has implications for deleting things; the draft directory was
always an easy target for deletion, but now after deletion you will need
to tell `orderly` that you have deleted things. See
[`vignette("introduction")`](https://mrc-ide.github.io/orderly/articles/introduction.md)
for details on this (section “Deleting things from the archive”).

### No more testing or development mode

We have had two different, but unsatisfactory, mechanisms for developing
an `orderly` report:

- `orderly_test_start` (up to version 1.1.0)
- `orderly_develop_start` (from 1.1.0 onwards)

These worked by doing the initial setup, and copying of dependencies etc
to a location you could work (a new draft for `orderly_test_start` and
the source directory for `orderly_develop_start`). From `orderly` you
can just work directly within the source directory, and so long as your
working directory is set to `src/<report-name>`, all `orderly` commands
will work as expected.

As with `orderly1`, you will need to be careful not to commit (to git)
results of running your analysis, and we encourage per-report
`.gitignore` files to help with this.

### New, language-agnostic, backend

The biggest change, but perhaps the least visible, is that `orderly` is
now built on an open spec [outpack](https://github.com/mrc-ide/outpack)
which can be implemented for any language. We will develop a Python
implementation of this, and possibly other languages.

This takes control of all the metadata. As such there is a split between
“`orderly_`” and “`outpack_`” functions in this package, for more
information see the last section of
[`vignette("introduction")`](https://mrc-ide.github.io/orderly/articles/introduction.md)
and also `vignette("outpack")`.

### Other, smaller changes

**“Global” resources have become “shared” resources**, and always live
in `shared/` at the `orderly` root (i.e., this is no longer
configurable). The reason for this is that we want reports to be able to
be run fairly independently of the `orderly` configuration (the only
exception to this are plugins). In practice people did not really vary
this.

**You can depend on anything**; if a packet has produced a file you can
depend on it, regardless of if it was an artefact, a resource or simply
just present.

## What is missing compared with `orderly1`

- the changelog feature (we may implement something to support this)
- extraction of secrets from vault (we may support this)
- automatic handling of README files (we may implement support for this)

## How to migrate

There are two parts to a migration: updating the canonical copy of your
`orderly` archive (ideally you only have one of these) and updating your
source tree. These steps should be done via the
[`outpack.orderly`](https://github.com/mrc-ide/outpack.orderly) package.

You should migrate your archive first. Do this for every archive that
you want to retain (you might have archives stored locally, on
production servers and on staging servers). Archive migration happens
*out of place*; that is, we do not modify anything in the original
location. If your archive is old and has been used with very old
versions of `orderly1` it is possible that this process will have a few
hiccups. Please let us know if that is the case. The result of this
process is that you will end up with a new directory that contains a new
archive conforming to the `outpack` spec and containing `orderly`
metadata.

Next, migrate your source tree. This will be done *in place* so should
be done on a fresh clone of your source git repository. For each report,
we will examine your `orderly.yml` files and your script files (often
`script.R`), delete these, and then write out a new `orderly` file that
will adapt your report to work for `orderly`. It is possible that this
will not be perfect and might need some minor tweaking but hopefully it
will be reasonable. One thing that is not preserved (and we probably
cannot do so) is the comments from the `yaml` but as these often refer
to `yaml` formatting or `orderly1` features hopefully this is not too
much of a problem. You will probably want to manually tweak the
generated code anyway, to take advantage of some of the new `orderly`
features such as being able to compute dependencies.

If you are using OrderlyWeb, you probably need to pause before
migrating, as the replacement is not yet ready.

## What about the packages?

We have renamed the original version of the package as `orderly1` so
that you can use it with existing projects without migrating; this can
be safely installed alongside this new `orderly` package.

## What were the problems in version 1

- The YAML format is inflexible, error prone for users, and leads to
  duplication
- It was too focussed around our initial needs with the [Vaccine Impact
  Modelling Consortium](https://www.vaccineimpact.org/)
- It was fairly easy to get your archive and SQLite database into an
  inconsistent state (e.g., by deleting or moving files from the
  archive)
- The SQLite database behaved poorly on shared file systems

## Working with old versions without migrating

Sometimes, you will want to use the old version of `orderly` without
migrating any source, for example to rerun something as it was run in
2020. You can do this by installing `orderly1`, which can be safely
installed alongside `orderly`. You should, however, avoid loading both
packages at once (i.e., with
[`library()`](https://rdrr.io/r/base/library.html)) as this will cause
ambiguity in a number of functions, notably
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md)

Install `orderly1` with:

    install.packages(
      "orderly1",
      repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))

and you can work with the sources or outputs as you wish. You will need
to replace any namespaced calls `orderly::fn` with `orderly1::fn` (these
might appear in your documentation, helper scripts, or similar).

If you try to use `orderly` (\>= 1.99.82) with an `orderly1` project,
then you will get an error message that will direct you to this document
to decide the best way forward.

## Users of `orderly2`

Users of the `orderly2` package have a little work to do; that package
was the precursor to this one (`orderly2` version 1.99.81 and `orderly`
1.99.82 are functionally the same but with references to `orderly2`
replaced with `orderly`). You will likely have references to `orderly2`
remaining in your code that will not necessarily work with the new
version without a little work.

First, you will want to upgrade from `orderly2` to the most recent
version (2.0.3) of `orderly` by uninstalling `orderly2`:

``` r
remove.packages("orderly2")
```

and installing `orderly`:

``` r
install.packages(
  "orderly",
  repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
```

As always, please do this in a fresh session.

In the simplest cases there is nothing more to do!

Most users will have references to `orderly2` in their code though, for
example within an `orderly` source report you might have something like

``` r
orderly2::orderly_parameter(...)
```

which needs updating to use `orderly`. A few alternative solutions are
available:

- migrate your source code
- use the built-in compatibility support
- install the `orderly2` compatibility package

### Migrate your source code

The best solution to use if you are happy modifying your source code. We
require a clean clone of your repository (`git status` should report no
modified or unknown files). You can then run:

``` r
orderly_migrate_source()
```

which will rewrite some simple references to `orderly2` for you. In
particular it will replace

- `orderly2::fn` with `orderly::fn`
- [`library(orderly2)`](https://rdrr.io/r/base/library.html) with
  [`library(orderly)`](https://github.com/mrc-ide/orderly)

It will also update the minimum required version in the
`orderly_config.json` to at least 2.0.0 (or migrate `orderly_config.yml`
to `orderly_config.json` and update that file).

Review the changes that this function has made by running `git diff`,
and if happy commit them.

You might like to check for other missed occurrences of `orderly2`, by
running something like

    grep --exclude-dir .git -r orderly2 .

which will show you remaining occurrences in files that we do not scan
or change. Run this in the shell, not from R!

### Use the built-in compatibility support

Sometimes you will not be able to migrate the source code (e.g., working
with an old project, or if you are working on a feature branch ahead of
doing the migration). In this case, so long as you have the new version
of `orderly` installed along with [`pkgload`](https://pkgload.r-lib.org)
(which is a dependency of `devtools`) then we will automatically load a
compatibility package that will interpret all references to `orderly2`
as references to `orderly` on the fly.

We will retain this support within `orderly` for a few versions, but
will add a warning where it is used soon to encourage you to migrate.

### Install the `orderly2` compatibility package

Run

``` r
install.packages(
  "orderly2",
  repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
```

to install a package that will interpret references to `orderly2` as
references to `orderly` on the fly. This solution will be available for
the foreseeable future.
