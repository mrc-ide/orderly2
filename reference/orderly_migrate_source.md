# Migrate orderly source code

Migrate source code for an orderly project. Periodically, we may make
changes to how orderly works that require you to update your source code
sooner or later. This function can be used to automate (or at least
accelerate) that process by trying to rewrite the R code within your
project. See below for details of migrations and triggers for them.

## Usage

``` r
orderly_migrate_source(path = ".", dry_run = FALSE, from = NULL, to = NULL)
```

## Arguments

- path:

  Path to the repository to migrate

- dry_run:

  Logical, indicating if no changes would be made, but just print
  information about the changes that would be made. If `TRUE`, you can
  run this function against a repository that is not under version
  control.

- from:

  Optional minimum version to migrate from. If `NULL`, we migrate from
  the version indicated in your orderly configuration and assume that
  all older migrations have been applied. You can specify a lower
  version here if you want to force migrations that would otherwise be
  skipped because they are assumed to be applied. Pass `"0"` (as a
  string) to match all previous versions.

- to:

  Optional maximum version to migrate to. If `NULL` we apply all
  possible migrations. With `dry_run = TRUE` you may not want to use
  this, because we do not write any files, therefore each migration does
  not see the results of applying the previous migration.

## Value

Primarily called for side effects, but returns (invisibly) `TRUE` if any
changes were made (or would be made if `dry_run` was `TRUE`) and `FALSE`
otherwise.

## Details

This function acts as an interface for rewriting the source code that
will be used to create new packets, it does not migrate any data from
packets that have been run. The idea here is that if we make changes to
how orderly works that require some repetitive and relatively simple
changes to your code, we can write a script that will do a reasonable
(if not perfect) job of this, and you can run this over your code, check
the results and if you like it commit the changes to your repository,
rather than you having to go through and change everything by hand.

The version of orderly that you support is indicated by the version
specified in `orderly_version.yml`; we will change some warnings to
errors once you update this, in order to help you keep your code up to
date.

## Migrations

A summary of migrations. The version number indicates the minimum
version that this would increase your source repository to.

Currently, we do not *enforce* these changes must be present in a
repository that declares it uses a recent orderly version, but this may
happen at any time, without further warning!

### 1.99.82

Removes references to `orderly2`, replacing them with `orderly`. This
affects namespaced calls (e.g., `orderly2::orderly_parameter()`) and
calls to `library` (e.g.,
[`library(orderly2)`](https://rdrr.io/r/base/library.html))

### 1.99.88

Renames `<name>/orderly.R` files to `<name>/<name>.R`, a change that we
introduced in early 2024 (version 1.99.13).

### Future migrations

We have some old changes to enable here:

- enforcing named arguments to `orderly_artefact`

We would like to enforce changes to `orderly_parameter` but have not
worked out a general best practice way of doing this.

## Migration process

This function requires a clean git status before it is run, and will
typically be best to run against a fresh clone of a repository (though
this is not enforced). After running, review changes (if any) with
`git diff` and then commit. You cannot run this function against source
code that is not version controlled with git.

We will refuse to migrate sources if we find the directories `archive/`,
`draft/` or `.outpack/` to avoid any chance of modifying files in
packets that have been previously run. You should make a fresh clone,
migrate that, push back up to GitHub (or wherever you store your
sources) and pull back down into your working directory.

## Migration of very old sources

If you have old yaml-based orderly sources, you should consult
[`vignette("migrating")`](https://mrc-ide.github.io/orderly/articles/migrating.md)
as the migration path is not automatic and a bit more involved. You will
need to install the helper package `outpack.orderly` and migrate your
source and your archive separately.

## Examples

``` r
# If a project already has made the migration from orderly2 to
# orderly, then the migration does nothing:
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c1117ba63'
orderly_migrate_source(path, dry_run = TRUE)
#> ✔ No migrations to apply
```
