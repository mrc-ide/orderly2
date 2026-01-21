# Push tree to location

Push tree to location. This function works out what packets are not
known at the location and then what files are required to create them.
It then pushes all the files required to build all packets and then
pushes the missing metadata to the server. If the process is interrupted
it is safe to resume and will only transfer files and packets that were
missed on a previous call.

## Usage

``` r
orderly_location_push(
  expr,
  location,
  name = NULL,
  dry_run = FALSE,
  root = NULL
)
```

## Arguments

- expr:

  An expression to search for. Often this will be a vector of ids, but
  you can use a query here.

- location:

  The name of a location to push to (see
  [`orderly_location_list()`](https://mrc-ide.github.io/orderly/reference/orderly_location_list.md)
  for possible values).

- name:

  Optionally, the name of the packet to scope the query on. This will be
  intersected with `scope` arg and is a shorthand way of running
  `scope = list(name = "name")`

- dry_run:

  Logical, indicating if we should print a summary but not make any
  changes.

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function does not require
  that the directory is configured for orderly, and can be any `outpack`
  root (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

Invisibly, details on the information that was actually moved (which
might be more or less than what was requested, depending on the
dependencies of packets and what was already known on the other
location).

## Examples

``` r
# Two roots, one local and one representing some remote orderly
# location.  The remote location must use a file store at present.
local <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c5a34ef45'
remote <- orderly_example(use_file_store = TRUE)
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c4ed0b68'
orderly_location_add_path("remote", remote, root = local)
#> ℹ Testing location
#> ✔ Location configured successfully
#> ✔ Added location 'remote' (path)

# We create a packet in the local root:
id <- orderly_run("data", root = local)
#> ℹ Starting packet 'data' `20260121-095924-b9c97103` at 2026-01-21 09:59:24.730108
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095924-b9c97103 at 2026-01-21 09:59:24.760972 (0.030864 secs)

# Push a packet into our remote version
orderly_location_push(id, location = "remote", root = local)
#> ℹ Pushing 2 files for 1 packet
#> ⠙ Pushing file 1 / 2 (227 B)
#> ✔ Uploaded 2 files in 16ms
#> 
#> ℹ Fetching metadata from 1 location: 'remote'
#> ✔ Found 1 packet at 'remote', of which 0 are new
```
