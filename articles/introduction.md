# Introduction to orderly

This vignette provides a how-to style introduction to `orderly`, an
overview of key ingredients to writing `orderly` reports, and a summary
of key features and ideas. It may be useful to look at
[`vignette("orderly")`](https://mrc-ide.github.io/orderly/articles/orderly.md)
for a more roundabout discussion of what `orderly` is trying to achieve,
or
[`vignette("migrating")`](https://mrc-ide.github.io/orderly/articles/migrating.md)
if you are familiar with version 1 of `orderly` as this explains
concepts in terms of differences from the previous version.

You might also prefer the [`orderly`
tutorial](https://mrc-ide.github.io/orderly-tutorial/) which works
through similar material in slide form, or [watch a short
talk](https://www.youtube.com/watch?v=lkPgihFQbrk) that describes the
ideas in the package, and why it differs to other approaches to
reproducibility and workflows.

``` r
library(orderly)
```

## Installation

If you don’t already have `orderly` installed, you can install it from
CRAN with

``` r
install.packages("orderly")
```

or a potentially more recent version from our R-universe:

``` r
install.packages(
  "orderly",
  repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
```

## Creating an empty `orderly` repository

The first step is to initialise an empty `orderly` repository. An
`orderly` repository is a directory with the file `orderly_config.json`
within it, and since version 2 also a directory `.outpack/`. Files
within the `.outpack/` directory should never be directly modified by
users and this directory should be excluded from version control (see
`orderly_gitignore_update`).

Create an `orderly` repository by calling
[`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md):

``` r
path <- tempfile() # we'll use a temporary directory here - see note below
orderly_init(path)
## ✔ Created orderly root at '/tmp/RtmpP14Y55/file20173ed65adb'
```

which creates a few files:

    ## .
    ## ├── .outpack
    ## │   ├── config.json
    ## │   ├── location
    ## │   └── metadata
    ## └── orderly_config.json

This step should be performed on a completely empty directory, otherwise
an error will be thrown. Later, you will re-initialise an `orderly`
repository when cloning to a new machine, such as when working with
others; this is discussed in
[`vignette("collaboration")`](https://mrc-ide.github.io/orderly/articles/collaboration.md).

The `orderly_config.json` file contains very little by default:

``` json
{"minimum_orderly_version": "2.0.0"}
```

For this vignette, the created `orderly` root is in R’s per-session
temporary directory, which will be deleted once R exits. If you want to
use a directory that will persist across restarting R (which you would
certainly want when using `orderly` on a real project!) you should
replace this with a path within your home directory, or other location
that you control.

For the rest of the vignette we will evaluate commands from within this
directory, by changing the directory to the path we’ve created:

``` r
setwd(path)
```

## Creating your first `orderly` report

An `orderly` report is a directory `src/<name>` containing an `orderly`
file `<name>.R`. That file may have special commands in it, but for now
we’ll create one that is as simple as possible; we’ll create some random
data and save it to disk. This seems silly, but imagine this standing in
for something like:

- downloading file from some external site or resource
- running a simulation and saving output
- fitting a model to data
- merging some set of files together to create a final data set

Our directory structure (ignoring the hidden `.outpack` directory) looks
like:

    ## .
    ## ├── orderly_config.json
    ## └── src
    ##     └── incoming_data
    ##         ├── data.csv
    ##         └── incoming_data.R

and `src/incoming_data/incoming_data.R` contains:

``` r
d <- read.csv("data.csv")
d$z <- resid(lm(y ~ x, d))
saveRDS(d, "data.rds")
```

To run the report and create a new “**packet**”, use
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md):

``` r
id <- orderly_run("incoming_data")
## ℹ Starting packet 'incoming_data' `20260121-095948-25f51f24` at 2026-01-21 09:59:48.15446
## > d <- read.csv("data.csv")
## > d$z <- resid(lm(y ~ x, d))
## > saveRDS(d, "data.rds")
## ✔ Finished running incoming_data.R
## ℹ Finished 20260121-095948-25f51f24 at 2026-01-21 09:59:48.214346 (0.05988574 secs)
id
## [1] "20260121-095948-25f51f24"
```

The `id` that is created is a new identifier for the packet that will be
both unique among all packets (within reason) and chronologically
sortable. A packet that has an id that sorts after another packet’s id
was started before that packet.

Having run the report, our directory structure looks like:

    ## .
    ## ├── archive
    ## │   └── incoming_data
    ## │       └── 20260121-095948-25f51f24
    ## │           ├── data.csv
    ## │           ├── data.rds
    ## │           └── incoming_data.R
    ## ├── draft
    ## │   └── incoming_data
    ## ├── orderly_config.json
    ## └── src
    ##     └── incoming_data
    ##         ├── data.csv
    ##         └── incoming_data.R

A few things have changed here:

- we have a directory archive/incoming_data/20260121-095948-25f51f24;
  this directory contains
  - the file that was created when we ran the report (`data.rds`; see
    the script above)
  - a log of what happened when the report was run and the packet was
    created
  - `incoming_data.R` and `data.csv`, the original input that have come
    from our source tree
- there is an empty directory `draft/incoming_data` which was created
  when `orderly` ran the report in the first place; you can ignore (or
  delete) this directory

In addition, quite a few files have changed within the `.outpack`
directory, but these are not covered here.

That’s it! Notice that the initial script is just a plain R script, and
you can develop it interactively from within the `src/incoming_data`
directory. Note however, that any paths referred to within will be
relative to `src/incoming_data` and **not** the `orderly` repository
root. This is important as all reports only see the world relative to
the directory containing their `<name>.R` file (here,
`incoming_data.R`).

Once created, you can then refer to this report by id and pull its files
wherever you need them, both in the context of another `orderly` report
or just to copy to your desktop to email someone. For example, to copy
the file `data.rds` that we created to some location outside of
`orderly`’s control you could do

``` r
dest <- tempfile()
fs::dir_create(dest)
orderly_copy_files(id, files = c("final.rds" = "data.rds"),
                   dest = dest)
```

which copies `data.rds` to some new temporary directory `dest` with name
`final.rds`.

## Depending on packets from another report

Creating a new dataset is mostly useful if someone else can use it. To
do this we introduce the first of the special `orderly` commands that
you can use from an `orderly` file

The `src/` directory now looks like:

    ## src
    ## ├── analysis
    ## │   └── analysis.R
    ## └── incoming_data
    ##     ├── data.csv
    ##     └── incoming_data.R

and `src/analysis/analysis.R` contains:

``` r
orderly_dependency("incoming_data", "latest()",
                   c("incoming.rds" = "data.rds"))
d <- readRDS("incoming.rds")
png("analysis.png")
plot(y ~ x, d)
dev.off()
```

Here, we’ve used
[`orderly_dependency()`](https://mrc-ide.github.io/orderly/reference/orderly_dependency.md)
to pull in the file `data.rds` from the most recent version (`latest()`)
of the `data` packet with the filename `incoming.rds`, then we’ve used
that file as normal to make a plot, which we’ve saved as `analysis.png`.

We can run this just as before, using
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md):

``` r
id <- orderly_run("analysis")
## ℹ Starting packet 'analysis' `20260121-095948-9d467b4d` at 2026-01-21 09:59:48.618852
## > orderly_dependency("incoming_data", "latest()",
## +                    c("incoming.rds" = "data.rds"))
## ℹ Depending on incoming_data @ `20260121-095948-25f51f24` (via latest(name == "incoming_data"))
## > d <- readRDS("incoming.rds")
## > png("analysis.png")
## > plot(y ~ x, d)
## > dev.off()
## agg_png 
##       2
## ✔ Finished running analysis.R
## ℹ Finished 20260121-095948-9d467b4d at 2026-01-21 09:59:48.704893 (0.08604121 secs)
```

See how (from the logs) `orderly` has found the `data` packet that we
created before and arranged to copy the files from one place to another
on demand. When it does this it also records metadata about this
relationship, which we can query later.

For more information on dependencies, see
[`vignette("dependencies")`](https://mrc-ide.github.io/orderly/articles/dependencies.md).

## Available in-report `orderly` commands

The function
[`orderly_dependency()`](https://mrc-ide.github.io/orderly/reference/orderly_dependency.md)
is designed to operate while the packet runs. These functions all act by
adding metadata to the final packet, and perhaps by copying files into
the directory.

- [`orderly_description()`](https://mrc-ide.github.io/orderly/reference/orderly_description.md):
  Provide a longer name and description for your report; this can be
  reflected in tooling that uses `orderly` metadata to be much more
  informative than your short name.
- [`orderly_parameters()`](https://mrc-ide.github.io/orderly/reference/orderly_parameters.md):
  Declares parameters that can be passed in to control the behaviour of
  the report, returning a list of the parameters. Parameters are
  key-value pairs of simple data (booleans, numbers, strings) which your
  report can respond to. They can also be used in queries to
  [`orderly_dependency()`](https://mrc-ide.github.io/orderly/reference/orderly_dependency.md)
  to find packets that satisfy some criteria.
- [`orderly_resource()`](https://mrc-ide.github.io/orderly/reference/orderly_resource.md):
  Declares that a file is a *resource*; a file that is an input to the
  the report, and which comes from this source directory. By default,
  `orderly` treats all files in the directory as a resource, but it can
  be useful to mark these explicitly, and necessary to do so in “strict
  mode” (see below). Files that have been marked as a resource are
  **immutable** and may not be deleted or modified.
- [`orderly_shared_resource()`](https://mrc-ide.github.io/orderly/reference/orderly_shared_resource.md):
  Copies a file from the “shared resources” directory `shared/`, which
  can be data files or source code located at the root of the `orderly`
  repository. This can be a reasonable way of sharing data or commonly
  used code among several reports.
- [`orderly_artefact()`](https://mrc-ide.github.io/orderly/reference/orderly_artefact.md):
  Declares that a file (or set of files) will be created by this report,
  before it is even run. Doing this makes it easier to check that the
  report behaves as expected and can allow reasoning about what a
  related set of reports will do without running them. By declaring
  something as an artefact (especially in conjunction with “strict
  mode”) it is also easier to clean up `src` directories that have been
  used in interactive development (see below).
- [`orderly_dependency()`](https://mrc-ide.github.io/orderly/reference/orderly_dependency.md):
  Copy files from one packet into this packet as it runs, as seen above.
- [`orderly_strict_mode()`](https://mrc-ide.github.io/orderly/reference/orderly_strict_mode.md):
  Declares that this report will be run in “strict mode” (see below).

In addition, there is also a function
[`orderly_run_info()`](https://mrc-ide.github.io/orderly/reference/orderly_run_info.md)
that can be used while running a report that returns information about
the currently running report (its id, resolved dependencies etc).

Let’s add some additional annotations to the previous reports:

``` r
orderly_strict_mode()
orderly_resource("data.csv")
orderly_artefact(description = "Processed data", "data.rds")

d <- read.csv("data.csv")
d$z <- resid(lm(y ~ x, d))
saveRDS(d, "data.rds")
```

Here, we’ve added a block of special `orderly` commands; these could go
anywhere, for example above the files that they refer to. If strict mode
is enabled (see below) then
[`orderly_resource()`](https://mrc-ide.github.io/orderly/reference/orderly_resource.md)
calls must go before the files are used as they will only be made
available at that point (see below).

``` r
id <- orderly_run("incoming_data")
## ℹ Starting packet 'incoming_data' `20260121-095948-e43889df` at 2026-01-21 09:59:48.895108
## > orderly_strict_mode()
## > orderly_resource("data.csv")
## > orderly_artefact(description = "Processed data", "data.rds")
## > d <- read.csv("data.csv")
## > d$z <- resid(lm(y ~ x, d))
## > saveRDS(d, "data.rds")
## ✔ Finished running incoming_data.R
## ℹ Finished 20260121-095948-e43889df at 2026-01-21 09:59:48.922952 (0.02784371 secs)
```

This has no impact on the data that is produced, but provides an easy
way to associate extra metadata into the produced packet, and allows us
to start building guarantees about what parts of the graph will produce.

## Parameterised reports

Much of the flexibility that comes from the `orderly` graph comes from
using parameterised reports; these are reports that take a set of
parameters and then change behaviour based on these parameters.
Downstream reports can depend on a parameterised report and filter based
on suitable parameters.

For example, consider a simple report where we generate samples based on
some parameter:

``` r
pars <- orderly_parameters(n_samples = 10)
x <- seq_len(pars$n_samples)
d <- data.frame(x = x, y = x + rnorm(pars$n_samples))
saveRDS(d, "data.rds")
```

This creates a report that has a single parameter `n_samples` with a
default value of 10. We could have used

``` r
pars <- orderly_parameters(n_samples = NULL)
```

to define a parameter with no default, or defined multiple parameters
with

``` r
pars <- orderly_parameters(n_samples = 10, distribution = "normal")
```

You can do anything in your report that switches on the value of a
parameter:

- You might read different URLs to fetch different underlying data
- You might fit a different analysis
- You might read different shared resources (see below)
- You might depend on different dependencies
- You might produce different artefacts

However, you should see parameters as relatively heavyweight things and
try to have a consistent set over all packets created from a report. In
this report we use it to control the size of the generated data set.

``` r
id <- orderly_run("random", list(n_samples = 15))
## ℹ Starting packet 'random' `20260121-095949-1cf55387` at 2026-01-21 09:59:49.117375
## ℹ Parameters:
## • n_samples: 15
## > pars <- orderly_parameters(n_samples = 10)
## > x <- seq_len(pars$n_samples)
## > d <- data.frame(x = x, y = x + rnorm(pars$n_samples))
## > saveRDS(d, "data.rds")
## ✔ Finished running random.R
## ℹ Finished 20260121-095949-1cf55387 at 2026-01-21 09:59:49.147345 (0.02996993 secs)
```

Our resulting file has 15 rows, as the parameter we passed in affected
the report:

``` r
orderly_copy_files(id, files = c("random.rds" = "data.rds"),
                   dest = dest)
readRDS(file.path(dest, "random.rds"))
##     x          y
## 1   1  0.4463006
## 2   2  2.6289820
## 3   3  5.0650249
## 4   4  2.3690106
## 5   5  5.5124269
## 6   6  4.1369885
## 7   7  6.4779875
## 8   8  7.9473981
## 9   9  9.5429963
## 10 10  9.0859252
## 11 11 11.4681544
## 12 12 12.3629513
## 13 13 11.6954565
## 14 14 14.7377763
## 15 15 16.8885049
```

You can use these parameters in `orderly`’s search functions. For
example we can find the most recent version of a packet by running:

``` r
orderly_search('latest(name == "random")')
## [1] "20260121-095949-1cf55387"
```

But we can also pass in parameter queries here:

``` r
orderly_search('latest(name == "random" && parameter:n_samples > 10)')
## [1] "20260121-095949-1cf55387"
```

These can be used within
[`orderly_dependency()`](https://mrc-ide.github.io/orderly/reference/orderly_dependency.md)
(the `name == "random"` part is implied by the first `name` argument),
for example

``` r
orderly_dependency("random", "latest(parameter:n_samples > 10)",
                   c("random.rds" = "data.rds"))
```

In this case if the report that you are querying *from* also has
parameters you can use these within the query, using the `this` prefix.
So suppose our downstream report simply uses `n` for the number of
samples we might write:

``` r
orderly_dependency("random", "latest(parameter:n_samples == this:n)",
                   c("randm.rds" = "data.rds"))
```

to depend on the most recent packet called `random` where it has a
parameter `n_samples` which has the same value as the current report’s
parameter `n`.

See
[`vignette("query")`](https://mrc-ide.github.io/orderly/articles/query.md)
for much more detail on this.

## Shared resources

Sometimes it is useful to share data between different reports, for
example some common source utilities that don’t warrant their own
package, or some common data.

To do this, create a directory `shared` at the `orderly` root and put in
it any files or directories you might want to share.

Suppose our shared directory contains a file `data.csv`:

    ## .
    ## ├── archive
    ## │   ├── analysis
    ## │   │   └── 20260121-095948-9d467b4d
    ## │   │       ├── analysis.R
    ## │   │       ├── analysis.png
    ## │   │       └── incoming.rds
    ## │   ├── incoming_data
    ## │   │   ├── 20260121-095948-25f51f24
    ## │   │   │   ├── data.csv
    ## │   │   │   ├── data.rds
    ## │   │   │   └── incoming_data.R
    ## │   │   └── 20260121-095948-e43889df
    ## │   │       ├── data.csv
    ## │   │       ├── data.rds
    ## │   │       └── incoming_data.R
    ## │   └── random
    ## │       └── 20260121-095949-1cf55387
    ## │           ├── data.rds
    ## │           └── random.R
    ## ├── draft
    ## │   ├── analysis
    ## │   ├── incoming_data
    ## │   └── random
    ## ├── orderly_config.json
    ## ├── shared
    ## │   └── data.csv
    ## └── src
    ##     ├── analysis
    ##     │   └── analysis.R
    ##     ├── incoming_data
    ##     │   ├── data.csv
    ##     │   └── incoming_data.R
    ##     └── random
    ##         └── random.R

We can then write an `orderly` report `use_shared` that uses this shared
file, with its `use_shared.R` containing:

``` r
orderly_shared_resource("data.csv")
orderly_artefact(description = "analysis", "analysis.png")

d <- read.csv("data.csv")
png("analysis.png")
plot(y ~ x, d)
dev.off()
```

We can run this:

``` r
id <- orderly_run("use_shared")
## ℹ Starting packet 'use_shared' `20260121-095949-903e1b71` at 2026-01-21 09:59:49.567713
## > orderly_shared_resource("data.csv")
## > orderly_artefact(description = "analysis", "analysis.png")
## > d <- read.csv("data.csv")
## > png("analysis.png")
## > plot(y ~ x, d)
## > dev.off()
## agg_png 
##       2
## ✔ Finished running use_shared.R
## ℹ Finished 20260121-095949-903e1b71 at 2026-01-21 09:59:49.606262 (0.03854918 secs)
```

In the resulting archive, the file that was used from the shared
directory is present:

    ## archive/use_shared
    ## └── 20260121-095949-903e1b71
    ##     ├── analysis.png
    ##     ├── data.csv
    ##     └── use_shared.R

This is a general property of `orderly`: it tries to save all the inputs
alongside the final results of the analysis, so that later on you can
check to see what went into an analysis and what might have changed
between versions.

The boundaries between source code under version control, shared
resources and dependencies are blurry, and we expect teams will find
ways of working that suit them; one group’s solution may not please
another.

## Strict mode

The previous version of `orderly` (`orderly1`; see
[`vignette("migrating")`](https://mrc-ide.github.io/orderly/articles/migrating.md))
was very fussy about all input being strictly declared before a report
could be run, so that it was clear what was really required in order to
run something. From version 2 this is relaxed by default, but you can
opt into most of the old behaviours and checks by adding

``` r
orderly_strict_mode()
```

anywhere within your `orderly` file (conventionally at the top). We may
make this more granular in future, but by adding this we:

- only copy files from the source directory (`src/<reportname>/`) to the
  draft directory where the report runs
  (`draft/<reportname>/<packet-id>`) that were declared with
  [`orderly_resource()`](https://mrc-ide.github.io/orderly/reference/orderly_resource.md);
  this leaves behind any extra files left over in development
- warn at the end of running a packet if any files are found that are
  not part of an artefact

Using strict mode also helps `orderly` clean up the `src/<reportname>`
directory more effectively after interactive development (see next
section).

## Interactive development

Set your working directory to `src/<reportname>` and any `orderly`
script should be fully executable (e.g., source with Rstudio’s `Source`
button, or R’s [`source()`](https://rdrr.io/r/base/source.html)
function). Dependencies will be copied over as needed.

After doing this, you will have a mix of files within your source
directory. We recommend a per-source-directory `.gitignore` which will
keep these files out of version control (see below).

For example, suppose that we have interactively run our
`incoming_data/incoming_data.R` script, we would leave behind generated
files. We can report on this with
[`orderly_cleanup_status()`](https://mrc-ide.github.io/orderly/reference/orderly_cleanup.md):

``` r
orderly_cleanup_status("incoming_data")
## ✖ incoming_data is not clean:
## ℹ 1 file can be deleted by running 'orderly_cleanup("incoming_data")':
##   • data.rds
```

If you have files here that are unknown to `orderly` it will tell you
about them and prompt you to tell it about them explicitly.

You can clean up generated files by running (as suggested in the
message):

``` r
orderly_cleanup("incoming_data")
## ℹ Deleting 1 file from 'incoming_data':
## • data.rds
```

There is a `dry_run = TRUE` argument you can pass if you want to see
what would be deleted without using the status function.

You can also keep these files out of git by using the
[`orderly_gitignore_update()`](https://mrc-ide.github.io/orderly/reference/orderly_gitignore_update.md)
function:

``` r
orderly_gitignore_update("incoming_data")
## ✔ Wrote 'src/incoming_data/.gitignore'
```

This creates (or updates) a `.gitignore` file within the report so that
generated files will not be included by git. If you have already
accidentally committed them then the gitignore has no real effect and
you should do some git surgery, see the git manuals or this [handy, if
profane, guide](https://ohshitgit.com/).

## Deleting things from the archive

If you delete packets from your `archive/` directory then this puts
`orderly` into an inconsistent state with its metadata store. Sometimes
this does not matter (e.g., if you delete old copies that would never be
candidates for inclusion with
[`orderly_dependency()`](https://mrc-ide.github.io/orderly/reference/orderly_dependency.md)
you will never notice). However, if you delete the most recent copy of a
packet and then try and depend on it, you will get an error.

At the moment, we have two copies of the `incoming_data` task:

``` r
orderly_metadata_extract(
  name = "incoming_data",
  extract = c(time = "time.start"))
##                         id                time
## 1 20260121-095948-25f51f24 2026-01-21 09:59:48
## 2 20260121-095948-e43889df 2026-01-21 09:59:48
```

When we run the `analysis` task, it will pull in the most recent version
(`20260121-095948-e43889df`). However, if you had deleted this manually
(e.g., to save space or accidentally) or corrupted it (e.g., by opening
some output in Excel and letting it save changes) it will not be able to
be included, and running `analysis` will fail:

``` r
orderly_run("analysis")
## ℹ Starting packet 'analysis' `20260121-095950-1eb59963` at 2026-01-21 09:59:50.124247
## > orderly_dependency("incoming_data", "latest()",
## +                    c("incoming.rds" = "data.rds"))
## ✖ Error running analysis.R
## ℹ Finished 20260121-095950-1eb59963 at 2026-01-21 09:59:50.203725 (0.07947779 secs)
## Error in `orderly_run()`:
## ! Failed to run report
## Caused by error in `orderly_copy_files()`:
## ! Unable to copy files, due to deleted packet 20260121-095948-e43889df
## ℹ Consider 'orderly_validate_archive("20260121-095948-e43889df", action =
##   "orphan")' to remove this packet from consideration
## Caused by error:
## ! File not found in archive
## ✖ data.rds
```

The error here tries to be fairly informative, telling us that we failed
because when copying files from `20260121-095948-e43889df` we found that
the packet was corrupt, because the file `data.rds` was not found in the
archive. It also suggests a fix; we can tell `orderly` that
`20260121-095948-e43889df` is “orphaned” and should not be considered
for inclusion when we look for dependencies.

We can carry out the suggestion and just validate this packet by running

``` r
orderly_validate_archive("20260121-095948-e43889df", action = "orphan")
```

or we can validate *all* the packets we have:

``` r
orderly_validate_archive(action = "orphan")
## ✔ 20260121-095948-25f51f24 (incoming_data) is valid
## ✔ 20260121-095948-9d467b4d (analysis) is valid
## ✖ 20260121-095948-e43889df (incoming_data) is invalid due to its files
## ✔ 20260121-095949-1cf55387 (random) is valid
## ✔ 20260121-095949-903e1b71 (use_shared) is valid
```

If we had the option `core.require_complete_tree` enabled, then this
process would also look for any packets that used our now-deleted packet
and orphan those too, as we no longer have a complete tree that includes
them.

If you want to remove references to the orphaned packets, you can use
[`orderly_prune_orphans()`](https://mrc-ide.github.io/orderly/reference/orderly_prune_orphans.md)
to remove them entirely:

``` r
orderly_prune_orphans()
## ℹ Pruning 1 orphan packet
```

## Interaction with version control

Some guidelines:

Make sure to exclude some files from `git` by listing them in
`.gitignore`:

- `.outpack/` - nothing in here is suitable for version control
- `archive/` - if you have `core.archive_path` set to a non-null value,
  this should be excluded. The default is `archive`
- `draft/` - the temporary draft directory
- `orderly_envir.yml` - used for setting machine-specific configuration

You absolutely should version control some files:

- `src/` the main source of your analyses
- `orderly_config.json` - this high level configuration is suitable for
  sharing
- The shared resource directory (`shared/`) should probably be version
  controlled

Your source repository will end up in multiple people’s machines, each
of which are configured differently. The configuration option set via
`orderly_config_set` are designed to be (potentially) different for
different users, so this configuration needs to be not version
controlled. It also means that reports/packets can’t directly refer to
values set here. This includes the directory used to save archive
packets at (if enabled) and the names of locations (equivalent to git
remotes).

You may find it useful to include scripts that help users set up common
locations, but like with git, different users may use different names
for the same remote (e.g., one user may have a location called `data`
while for another it is called `data-incoming`, depending on their
perspective about the use of the location).

`orderly` will always try and save information about the current state
of the git source repository alongside the packet metadata. This
includes the current branch, commit (sha) and remote url. This is to try
and create links between the final version of the packet and the
upstream source repository.

## Interaction with the outpack store

As alluded to above, the `.outpack` directory contains lots of
information about packets that have been run, but is typically “out of
bounds” for normal use. This is effectively the “database” of
information about packets that have been run. Understanding how this
directory is structured is not required for using `orderly`, but is
included here for the avoidance of mystery!

After all the work above, our directory structure looks like:

    ## .outpack
    ## ├── config.json
    ## ├── index
    ## │   └── outpack.rds
    ## ├── location
    ## │   ├── local
    ## │   │   ├── 20260121-095948-25f51f24
    ## │   │   ├── 20260121-095948-9d467b4d
    ## │   │   ├── 20260121-095949-1cf55387
    ## │   │   └── 20260121-095949-903e1b71
    ## │   └── orphan
    ## └── metadata
    ##     ├── 20260121-095948-25f51f24
    ##     ├── 20260121-095948-9d467b4d
    ##     ├── 20260121-095949-1cf55387
    ##     └── 20260121-095949-903e1b71

As can be perhaps inferred from the filenames, the files
`.outpack/metadata/<packet-id>` are the metadata for each packet as it
has been run. The files `.outpack/location/<location-id>/<packet-id>`
holds information about when the packet was first known about by a
location (here the location is the special “local” location).

The default `orderly` configuration is to store the final files in a
directory called `archive/`, but alternatively (or additionally) you can
use a [content-
addressable](https://en.wikipedia.org/wiki/Content-addressable_storage)
file store. With this enabled, the `.outpack` directory looks like:

    ## .outpack
    ## ├── config.json
    ## ├── files
    ## │   └── sha256
    ## │       ├── 20
    ## │       │   └── 0ce535b6ea0f2d875519de60d87ab10ade8092e4ad537cc7294502a4bb4ba3
    ## │       ├── 49
    ## │       │   └── 13986171d5b996e52ac50f9e3ba2a9cc3f261bab82c82e27930664a2ea814c
    ## │       ├── 5f
    ## │       │   └── 96f49230c2791c05706f24cb2335cd0fad5d3625dc6bca124c44a51857f3f8
    ## │       ├── a6
    ## │       │   └── 80ab7c65a52327a3d9c5499d114f513f18eabe7f63a98f9fc308c2b3744c82
    ## │       ├── aa
    ## │       │   └── 994dbde68580e1df76dbcc9e32157902c498fe9582e4784b40a437b9cb0cdd
    ## │       ├── b0
    ## │       │   └── bbd0c75a47435b74298ecb3ebdb3ceb77b00a373063092b1d4f716daff7477
    ## │       ├── b3
    ## │       │   └── 69412c2748c9c7762534c66ac8edb904cca5cc33126f72222d9a16e7a6b985
    ## │       ├── ba
    ## │       │   └── aa052008cfa7a30c9d83a4105f1dfb1b5632cde45373829e0bc63ef0d48f34
    ## │       ├── d5
    ## │       │   └── 0f20991ac416a9577edf0d3b5695f81d8d5daf91d3fb5bd5882361187d5b59
    ## │       └── ec
    ## │           └── b53285781a4d36c65168c80ee14f2af2c885423c6166b9425f40c3c6cd8297
    ## ├── index
    ## │   └── outpack.rds
    ## ├── location
    ## │   ├── local
    ## │   │   ├── 20260121-095948-25f51f24
    ## │   │   ├── 20260121-095948-9d467b4d
    ## │   │   ├── 20260121-095949-1cf55387
    ## │   │   └── 20260121-095949-903e1b71
    ## │   └── orphan
    ## └── metadata
    ##     ├── 20260121-095948-25f51f24
    ##     ├── 20260121-095948-9d467b4d
    ##     ├── 20260121-095949-1cf55387
    ##     └── 20260121-095949-903e1b71

The files under `.outpack/files/` should never be modified or deleted.
This approach to storage naturally deduplicates the file archive, so
that a large file used in many places is only ever stored once.

## Relationship between `orderly` and `outpack`

The `orderly` package is built on a metadata and file storage system
called `outpack`; we have implemented support for working with these
metadata archives in other languages (see
[`outpack_server`](https://github.com/mrc-ide/outpack_server) for our
server implementation in Rust and
[`pyorderly`](https://github.com/mrc-ide/pyorderly) in Python). The
metadata is discussed in more detail in
[`vignette("metadata")`](https://mrc-ide.github.io/orderly/articles/metadata.md)
and we will document the general ideas more fully at
[`mrc-ide/outpack`](https://github.com/mrc-ide/outpack).
