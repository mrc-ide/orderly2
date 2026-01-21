# Prune orphan packet metadata

Prune orphan packets from your metadata store. This function can be used
to remove references to packets that are no longer reachable; this could
have happened because you deleted a packet manually from the archive and
ran
[`orderly_validate_archive()`](https://mrc-ide.github.io/orderly/reference/orderly_validate_archive.md)
or because you removed a location.

## Usage

``` r
orderly_prune_orphans(root = NULL)
```

## Arguments

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function does not require
  that the directory is configured for orderly, and can be any `outpack`
  root (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

Invisibly, a character vector of orphaned packet ids

## Details

If an orphan packet is not used anywhere, then we can easily drop it -
it's as if it never existed. If it is referenced by metadata that you
know about from elsewhere but not locally, then that is a problem for
the upstream location (and one that should not happen). If you have
referenced it in a packet that you have run locally, the the metadata is
not deleted.

We expose this function mostly for users who want to expunge permanently
any reference to previously run packets. We hope that there should never
need to really be a reason to run it.

## Examples

``` r
# The same example as orderly_validate_archive; a corrupted
# archive due to the local deletion of a file
# Start with an archive containing 4 simple packets
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c621c18fd'
ids <- vapply(1:4, function(i) orderly_run("data", root = path), "")
#> ℹ Starting packet 'data' `20260121-095932-691126ed` at 2026-01-21 09:59:32.414618
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095932-691126ed at 2026-01-21 09:59:32.439899 (0.02528071 secs)
#> ℹ Starting packet 'data' `20260121-095932-758c8c69` at 2026-01-21 09:59:32.463396
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095932-758c8c69 at 2026-01-21 09:59:32.488205 (0.02480888 secs)
#> ℹ Starting packet 'data' `20260121-095932-829aabcb` at 2026-01-21 09:59:32.51418
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095932-829aabcb at 2026-01-21 09:59:32.538524 (0.02434468 secs)
#> ℹ Starting packet 'data' `20260121-095932-93738cf9` at 2026-01-21 09:59:32.580104
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095932-93738cf9 at 2026-01-21 09:59:32.605181 (0.02507639 secs)
fs::file_delete(file.path(path, "archive", "data", ids[[3]], "data.rds"))

# Validate the archive ands orphan corrupt packets:
orderly_validate_archive(action = "orphan", root = path)
#> ✔ 20260121-095932-691126ed (data) is valid
#> ✔ 20260121-095932-758c8c69 (data) is valid
#> ✖ 20260121-095932-829aabcb (data) is invalid due to its files
#> ✔ 20260121-095932-93738cf9 (data) is valid

# Prune our orphans:
orderly_prune_orphans(root = path)
#> ℹ Pruning 1 orphan packet
```
