# Validate unpacked packets.

Validate unpacked packets. Over time, expect this function to become
more fully featured, validating more.

## Usage

``` r
orderly_validate_archive(
  expr = NULL,
  name = NULL,
  action = "inform",
  root = NULL
)
```

## Arguments

- expr:

  The query expression. A `NULL` expression matches everything.

- name:

  Optionally, the name of the packet to scope the query on. This will be
  intersected with `scope` arg and is a shorthand way of running
  `scope = list(name = "name")`

- action:

  The action to take on finding an invalid packet. See Details.

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function does not require
  that the directory is configured for orderly, and can be any `outpack`
  root (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

Invisibly, a character vector of repaired (or invalid) packets.

## Details

The actions that we can take on finding an invalid packet are:

- `inform` (the default): just print information about the problem

- `orphan`: mark the packet as orphaned within the metadata, but do not
  touch the files in your archive (by default the directory
  `archive/`) - this is a safe option and will leave you in a consistent
  state without deleting anything.

- `delete`: in addition to marking the packet as an orphan, also delete
  the files from your archive.

Later, we will add a "repair" option to try and fix broken packets.

The validation interacts with the option `core.require_complete_tree`;
if this option is `TRUE`, then a packet is only valid if all its
(recursive) dependencies are also valid, so the action will apply to
packets that have also had their upstream dependencies invalidated. This
validation will happen even if the query implied by `...` does not
include these packets if a complete tree is required.

The validation will also interact with `core.use_file_store` once repair
is supported, as this becomes trivial.

## Examples

``` r
# Start with an archive containing 4 simple packets
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c42d4e943'
ids <- vapply(1:4, function(i) orderly_run("data", root = path), "")
#> ℹ Starting packet 'data' `20260121-095936-429c3359` at 2026-01-21 09:59:36.264211
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095936-429c3359 at 2026-01-21 09:59:36.28928 (0.02506876 secs)
#> ℹ Starting packet 'data' `20260121-095936-4f0293e6` at 2026-01-21 09:59:36.312728
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095936-4f0293e6 at 2026-01-21 09:59:36.33821 (0.0254817 secs)
#> ℹ Starting packet 'data' `20260121-095936-5c1c5733` at 2026-01-21 09:59:36.363858
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095936-5c1c5733 at 2026-01-21 09:59:36.387699 (0.02384043 secs)
#> ℹ Starting packet 'data' `20260121-095936-6cb8e854` at 2026-01-21 09:59:36.428811
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095936-6cb8e854 at 2026-01-21 09:59:36.45315 (0.02433872 secs)

# Suppose someone corrupts a packet by deleting a file:
fs::file_delete(file.path(path, "archive", "data", ids[[3]], "data.rds"))

# We can check all packets, and report on validity
orderly_validate_archive(root = path)
#> ✔ 20260121-095936-429c3359 (data) is valid
#> ✔ 20260121-095936-4f0293e6 (data) is valid
#> ✖ 20260121-095936-5c1c5733 (data) is invalid due to its files
#> ✔ 20260121-095936-6cb8e854 (data) is valid

# Alternatively, we can take action and orphan the invalid packet:
orderly_validate_archive(action = "orphan", root = path)
#> ✔ 20260121-095936-429c3359 (data) is valid
#> ✔ 20260121-095936-4f0293e6 (data) is valid
#> ✖ 20260121-095936-5c1c5733 (data) is invalid due to its files
#> ✔ 20260121-095936-6cb8e854 (data) is valid

# At which point the validation will not find this packet anymore
orderly_validate_archive(root = path)
#> ✔ 20260121-095936-429c3359 (data) is valid
#> ✔ 20260121-095936-4f0293e6 (data) is valid
#> ✔ 20260121-095936-6cb8e854 (data) is valid

# The orphaned packet will no longer be found in most operations:
orderly_search(root = path)
#> [1] "20260121-095936-429c3359" "20260121-095936-4f0293e6"
#> [3] "20260121-095936-6cb8e854"
```
