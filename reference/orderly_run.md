# Run a report

Run a report. This will create a new directory in `drafts/<reportname>`,
copy your declared resources there, run your script and check that all
expected artefacts were created.

## Usage

``` r
orderly_run(
  name,
  parameters = NULL,
  envir = NULL,
  echo = TRUE,
  location = NULL,
  allow_remote = NULL,
  fetch_metadata = FALSE,
  search_options = NULL,
  root = NULL
)
```

## Arguments

- name:

  Name of the report to run. Any leading `./` `src/` or trailing `/`
  path parts will be removed (e.g., if added by autocomplete).

- parameters:

  Parameters passed to the report. A named list of parameters declared
  in the `orderly.yml`. Each parameter must be a scalar character,
  numeric, integer or logical.

- envir:

  The environment that will be used to evaluate the report script; by
  default we use the global environment, which may not always be what is
  wanted.

- echo:

  Optional logical to control printing output from
  [`source()`](https://rdrr.io/r/base/source.html) to the console.

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

- search_options:

  **DEPRECATED**. Please don't use this any more, and instead use the
  arguments `location`, `allow_remote` and `fetch_metadata` directly.

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function **does** require
  that the directory is configured for orderly, and not just outpack
  (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

The id of the created report (a string)

## Locations used in dependency resolution

If your packet depends on other packets, you will want to control the
locations that are used to find appropriate packets. The control for
this is passed through this function and *not* as an argument to
[`orderly_dependency()`](https://mrc-ide.github.io/orderly/reference/orderly_dependency.md)
because this is a property of the way that a packet is created and not
of a packet itself; importantly different users may have different names
for their locations so it makes little sense to encode the location name
into the source code. Alternatively, you want to use different locations
in different contexts, for example sometimes you might want to include
local copies of packets as possible dependencies, but at other times you
want to resolve dependencies only as they would be resolved on one of
your locations.

Similarly, you might want to include packets that are known by other
locations but are not currently downloaded onto this machine - pulling
these packets in could take anything from seconds to hours depending on
their size and the speed of your network connection (but *not* pulling
in the packets could mean that your packet fails to run).

To allow for control over this you can pass in an arguments to control
the names of the locations to use, whether metadata should be refreshed
before we pull anything and if packets that are not currently downloaded
should be considered candidates.

This has no effect when running interactively, in which case you can
specify the search options (root specific) with
[`orderly_interactive_set_search_options()`](https://mrc-ide.github.io/orderly/reference/orderly_interactive_set_search_options.md)

## Which packets might be selected from locations?

The arguments `location`, `allow_remote` and `fetch_metadata` control
where outpack searches for packets with the given query and if anything
might be moved over the network (or from one outpack archive to
another). By default everything is resolved locally only; that is we can
only depend on packets that are unpacked within our current archive. If
you pass `allow_remote = TRUE`, then packets that are known anywhere are
candidates for using as dependencies and *if needed* we will pull the
resolved files from a remote location. Note that even if the packet is
not locally present this might not be needed - if you have the same
content anywhere else in an unpacked packet we will reuse the same
content without re-fetching.

If `fetch_metadata = TRUE`, then we will refresh location metadata
before pulling, and the `location` argument controls which locations are
pulled from.

## Equivalence to the old `use_draft` option

The above location handling generalises the version 1.x of orderly's
previous `use_draft` option, in terms of the new `location` argument:

- `use_draft = TRUE` is `location = "local"`

- `use_draft = FALSE` is `location = c(...)` where you should provide
  all locations *except* local
  (`setdiff(orderly_location_list(), "local")`)

- `use_draft = "newer"` is `location = NULL`

(this last option was the one most people preferred so is the new
default behaviour). In addition, you could resolve dependencies as they
currently exist on production right now with the options:

    location = "production", fetch_metadata = TRUE

which updates your current metadata from production, then runs queries
against only packets known on that remote, then depends on them even if
you don't (yet) have them locally. This functionality was never
available in orderly version 1, though we had intended to support it.

## Running with a source tree separate from outpack root

Sometimes it is useful to run things from a different place on disk to
your outpack root. We know of two cases where this has come up:

- when running reports within a runner on a server, we make a clean
  clone of the source tree at a particular git reference into a new
  temporary directory and then run the report there, but have it insert
  into an orderly repo at a fixed and non-temporary location.

- we have a user for whom it is more convenient to run their report on a
  hard drive but store the archive and metadata on a (larger) shared
  drive.

In the first instance, we have a source path at `<src>` which contains
the file `orderly_config.json` and the directory `src/` with our source
reports, and a separate path `<root>` which contains the directory
`.outpack/` with all the metadata - it may also have an unpacked
archive, and a `.git/` directory depending on the configuration. (Later
this will make more sense once we support a "bare" outpack layout.)

## Manually setting report source directory

To manually set the report source directory, you will need to set the
path of the directory as the `ORDERLY_REPORT_SRC` environment variable.

## Examples

``` r
# Create a simple example:
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c7eca716f'

# Run the 'data' task:
orderly_run("data", root = path)
#> ℹ Starting packet 'data' `20260121-095934-153876ea` at 2026-01-21 09:59:34.08726
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095934-153876ea at 2026-01-21 09:59:34.112574 (0.02531362 secs)
#> [1] "20260121-095934-153876ea"

# After running, a finished packet appears in the archive:
fs::dir_tree(path)
#> /tmp/RtmpoLRnek/orderly_ex_1b9c7eca716f
#> ├── archive
#> │   └── data
#> │       └── 20260121-095934-153876ea
#> │           ├── data.R
#> │           └── data.rds
#> ├── draft
#> │   └── data
#> ├── orderly_config.json
#> ├── shared
#> │   └── palette.R
#> └── src
#>     ├── data
#>     │   └── data.R
#>     ├── depends
#>     │   └── depends.R
#>     ├── parameters
#>     │   └── parameters.R
#>     ├── run_info
#>     │   └── run_info.R
#>     ├── shared
#>     │   └── shared.R
#>     └── strict
#>         └── strict.R

# and we can query the metadata:
orderly_metadata_extract(name = "data", root = path)
#>                         id name parameters
#> 1 20260121-095934-153876ea data           
```
