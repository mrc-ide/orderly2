# Pull one or more packets from a location

Pull one or more packets (including all their files) into this archive
from one or more of your locations. This will make files available for
use as dependencies (e.g., with
[`orderly_dependency()`](https://mrc-ide.github.io/orderly/reference/orderly_dependency.md)).

## Usage

``` r
orderly_location_pull(
  expr,
  name = NULL,
  location = NULL,
  fetch_metadata = FALSE,
  recursive = NULL,
  options = NULL,
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

- location:

  Optional vector of locations to pull from. We might in future expand
  this to allow wildcards or exceptions.

- fetch_metadata:

  Logical, indicating if we should pull metadata immediately before the
  search. If `location` is given, then we will pass this through to
  [`orderly_location_fetch_metadata()`](https://mrc-ide.github.io/orderly/reference/orderly_location_fetch_metadata.md)
  to filter locations to update. If pulling many packets in sequence,
  you *will* want to update this option to `FALSE` after the first pull,
  otherwise it will update the metadata between every packet, which will
  be needlessly slow.

- recursive:

  If non-NULL, a logical, indicating if we should recursively pull all
  packets that are referenced by the packets specified in `id`. This
  might copy a lot of data! If `NULL`, we default to the value given by
  the the configuration option `require_complete_tree`.

- options:

  **DEPRECATED**. Please don't use this any more, and instead use the
  arguments `location`, `allow_remote` and `fetch_metadata` directly.

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function does not require
  that the directory is configured for orderly, and can be any `outpack`
  root (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

Invisibly, the ids of packets that were pulled

## Details

It is possible that it will take a long time to pull packets, if you are
moving a lot of data or if you are operating over a slow connection.
Cancelling and resuming a pull should be fairly efficient, as we keep
track of files that are copied over even in the case of an interrupted
pull.

## Examples

``` r
# Two roots, one local and one representing some remote orderly location:
local <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9cd7ce35d'
remote <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c55798b73'

# We create a packet in the remote root:
orderly_run("data", root = remote)
#> ℹ Starting packet 'data' `20260121-095924-5663c3d6` at 2026-01-21 09:59:24.341715
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095924-5663c3d6 at 2026-01-21 09:59:24.367389 (0.02567387 secs)
#> [1] "20260121-095924-5663c3d6"

# Add the remote as a path location to the local root:
orderly_location_add_path("remote", remote, root = local)
#> ℹ Testing location
#> ✔ Location configured successfully
#> ✔ Added location 'remote' (path)

# Pull a packet into our local version
orderly_location_pull(quote(latest(name == "data")),
                      fetch_metadata = TRUE, root = local)
#> ℹ Fetching metadata from 1 location: 'remote'
#> ✔ Found 1 packet at 'remote', of which 1 is new
#> ℹ Pulling 1 packet: '20260121-095924-5663c3d6'
#> ℹ Looking for suitable files already on disk
#> ℹ Need to fetch 2 files (825 B) from 1 location
#> ✔ Unpacked 1 packet
```
