# Copy files from a packet

Copy files from a packet to anywhere. Similar to
[`orderly_dependency()`](https://mrc-ide.github.io/orderly/reference/orderly_dependency.md)
except that this is not used in an active packet context. You can use
this function to pull files from an outpack root to a directory outside
of the control of outpack, for example. Note that all arguments need
must be provided by name, not position, with the exception of the id or
query.

## Usage

``` r
orderly_copy_files(
  expr,
  files,
  dest,
  overwrite = TRUE,
  name = NULL,
  location = NULL,
  allow_remote = NULL,
  fetch_metadata = FALSE,
  parameters = NULL,
  options = NULL,
  envir = parent.frame(),
  root = NULL
)
```

## Arguments

- expr:

  The query expression. A `NULL` expression matches everything.

- files:

  Files to copy from the other packet, as a character vector. If the
  character vector is unnamed, the files listed are copied over without
  changing their names. If the vector is named however, the names will
  be used as the destination name for the files.

  In either case, if you want to import a directory of files from a
  packet, you must refer to the source with a trailing slash (e.g.,
  `c(here = "there/")`), which will create the local directory
  `here/...` with files from the upstream packet directory `there/`. If
  you omit the slash then an error will be thrown suggesting that you
  add a slash if this is what you intended.

  You can use a limited form of string interpolation in the names of
  this argument; using `${variable}` will pick up values from `envir`
  and substitute them into your string. This is similar to the
  interpolation you might be familiar with from
  [`glue::glue`](https://glue.tidyverse.org/reference/glue.html) or
  similar, but much simpler with no concatenation or other fancy
  features supported.

  Note that there is an unfortunate, but (to us) avoidable inconsistency
  here; interpolation of values from your environment in the query is
  done by using `environment:x` and in the destination filename by doing
  `${x}`.

  If you want to copy *all* files from the packet, use `./` (read this
  as the directory of the packet). The trailing slash is required in
  order to be consistent with the rules above.

- dest:

  The directory to copy into

- overwrite:

  Overwrite files at the destination; this is typically what you want,
  but set to `FALSE` if you would prefer that an error be thrown if the
  destination file already exists.

- name:

  Optionally, the name of the packet to scope the query on. This will be
  intersected with `scope` arg and is a shorthand way of running
  `scope = list(name = "name")`

- location:

  Optional vector of locations to pull from. We might in future expand
  this to allow wildcards or exceptions.

- allow_remote:

  Logical, indicating if we should allow packets to be found that are
  not currently unpacked (i.e., are known only to a location that we
  have metadata from). If this is `TRUE`, then in conjunction with
  [`orderly_dependency()`](https://mrc-ide.github.io/orderly/reference/orderly_dependency.md)
  you might pull a large quantity of data. The default is `NULL`. This
  is `TRUE` if remote locations are listed explicitly as a character
  vector in the `location` argument, or if you have specified
  `fetch_metadata = TRUE`, otherwise `FALSE`.

- fetch_metadata:

  Logical, indicating if we should pull metadata immediately before the
  search. If `location` is given, then we will pass this through to
  [`orderly_location_fetch_metadata()`](https://mrc-ide.github.io/orderly/reference/orderly_location_fetch_metadata.md)
  to filter locations to update. If pulling many packets in sequence,
  you *will* want to update this option to `FALSE` after the first pull,
  otherwise it will update the metadata between every packet, which will
  be needlessly slow.

- parameters:

  Optionally, a named list of parameters to substitute into the query
  (using the `this:` prefix)

- options:

  **DEPRECATED**. Please don't use this any more, and instead use the
  arguments `location`, `allow_remote` and `fetch_metadata` directly.

- envir:

  Optionally, an environment to substitute into the query (using the
  `environment:` prefix). The default here is to use the calling
  environment, but you can explicitly pass this in if you want to
  control where this lookup happens.

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function does not require
  that the directory is configured for orderly, and can be any `outpack`
  root (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

Primarily called for its side effect of copying files from a packet into
the directory `dest`. Also returns a list with information about the
copy, containing elements:

- `id`: The resolved id of the packet

- `name`: The name of the packet

- `files`: a [data.frame](https://rdrr.io/r/base/data.frame.html) of
  filenames with columns `here` (the name of the file in `dest`) and
  `there` (the name of the file in the packet)

## Details

You can call this function with an id as a string, in which case we do
not search for the packet and proceed regardless of whether or not this
id is present. If called with any other arguments (e.g., a string that
does not match the id format, or a named argument `name`, `subquery` or
`parameters`) then we interpret the arguments as a query and
[`orderly_search()`](https://mrc-ide.github.io/orderly/reference/orderly_search.md)
to find the id. It is an error if this query does not return exactly one
packet id, so you probably want to use `latest()`.

There are different ways that this might fail (or recover from failure):

- if `id` is not known in the metadata store (not known because it's not
  unpacked but also not known to be present in some other remote) then
  this will fail because it's impossible to resolve the files. Consider
  refreshing the metadata with
  [`orderly_location_fetch_metadata()`](https://mrc-ide.github.io/orderly/reference/orderly_location_fetch_metadata.md)
  to refresh this.

- if the `id` is not unpacked *and* no local copy of the files referred
  to can be found, we error by default (but see the next option).
  However, sometimes the file you refer to might also be present because
  you have downloaded a packet that depended on it, or because the
  content of the file is unchanged because from some other packet
  version you have locally.

- if the `id` is not unpacked, there is no local copy of the file and if
  `allow_remote` is `TRUE` we will try and request the file from
  whatever remote would be selected by
  [`orderly_location_pull()`](https://mrc-ide.github.io/orderly/reference/orderly_location_pull.md)
  for this packet.

Note that empty directories might be created on failure.

## Examples

``` r
root <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c6ddc11d5'
orderly_run("data", root = root)
#> ℹ Starting packet 'data' `20260121-095920-76a61e3b` at 2026-01-21 09:59:20.467759
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095920-76a61e3b at 2026-01-21 09:59:20.496407 (0.02864718 secs)
#> [1] "20260121-095920-76a61e3b"

dest <- withr::local_tempdir()
res <- orderly_copy_files("latest", name = "data", "data.rds",
                          dest = dest, root = root)

# We now have our data in the destination directory:
fs::dir_tree(dest)
#> /tmp/RtmpoLRnek/file1b9cf56dad4
#> └── data.rds

# Information about the copy:
res
#> $id
#> [1] "20260121-095920-76a61e3b"
#> 
#> $name
#> [1] "data"
#> 
#> $files
#>       here    there
#> 1 data.rds data.rds
#> 
```
