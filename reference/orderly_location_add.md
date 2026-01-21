# Add a new location

Add a new location - a place where other packets might be found and
pulled into your local archive. Currently only file and http based
locations are supported, with limited support for custom locations. Note
that adding a location does *not* pull metadata from it, you need to
call
[`orderly_location_fetch_metadata()`](https://mrc-ide.github.io/orderly/reference/orderly_location_fetch_metadata.md)
first. The function `orderly_location_add` can add any sort of location,
but the other functions documented here (`orderly_location_add_path`,
etc) will typically be much easier to use in practice.

## Usage

``` r
orderly_location_add(name, type, args, verify = TRUE, root = NULL)

orderly_location_add_path(name, path, verify = TRUE, root = NULL)

orderly_location_add_http(name, url, verify = TRUE, root = NULL)

orderly_location_add_packit(
  name,
  url,
  token = NULL,
  save_token = NULL,
  verify = TRUE,
  root = NULL
)
```

## Arguments

- name:

  The short name of the location to use. Cannot be in use, and cannot be
  one of `local` or `orphan`

- type:

  The type of location to add. Currently supported values are `path` (a
  location that exists elsewhere on the filesystem), `http` (a location
  accessed over outpack's http API) and `packit` (a location accessed
  using the packit web app).

- args:

  Arguments to the location driver. The arguments here will vary
  depending on the type used, see Details.

- verify:

  Logical, indicating if we should verify that the location can be used
  before adding.

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function does not require
  that the directory is configured for orderly, and can be any `outpack`
  root (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

- path:

  The path to the other archive root. This can be a relative or absolute
  path, with different tradeoffs. If you use an absolute path, then this
  location will typically work well on this machine, but it may behave
  poorly when the location is found on a shared drive **and** when you
  use your orderly root from more than one system. This setup is common
  when using an HPC system. If you use a relative path, then we will
  interpret it **relative to your orderly root** and not the directory
  that you evaluate this command from. Typically your path should
  include leading dots (e.g. `../../somewhere/else`) as you should not
  nest orderly projects. This approach should work fine on shared
  filesystems.

- url:

  The location of the server, including protocol, for example
  `http://example.com:8080`

- token:

  The value for your your login token (currently this is a GitHub token
  with `read:org` scope). If `NULL`, orderly will perform an interactive
  authentication against GitHub to obtain one.

- save_token:

  If no token is provided and interactive authentication is used, this
  controls whether the GitHub token should be saved to disk. Defaults to
  `TRUE` if `NULL`.

## Value

Nothing, called for the side effect of modifying the orderly
configuration.

## Details

We currently support three types of locations - `path`, which points to
an outpack archive accessible by path (e.g., on the same computer or on
a mounted network share), `http`, which requires that an outpack server
is running at some url and uses an HTTP API to communicate, and
`packit`, which uses Packit as a web server. More types may be added
later, and more configuration options to these location types will
definitely be needed in future.

Configuration options for different location types are described in the
arguments to their higher-level functions.

**Path locations**:

Use `orderly_location_add_path`, which accepts a `path` argument.

**HTTP locations**:

Accessing outpack over HTTP requires that an outpack server is running.
The interface here is expected to change as we expand the API, but also
as we move to support things like TLS and authentication.

Use `orderly_location_add_http`, which accepts a `url` argument.

**Packit locations**:

Packit locations work over HTTPS, and include everything in an outpack
location but also provide authentication and later will have more
capabilities we think.

Use `orderly_location_add_packit`, which accepts `url`, `token` and
`save_token` arguments.

**Custom locations**:

All outpack implementations are expected to support path and http
locations, with the standard arguments above. But we expect that some
implementations will support custom locations, and that the argument
lists for these may vary between implementations. To allow this, you can
pass a location of type "custom" with a list of arguments. We expect an
argument 'driver' to be present among this list. For an example of this
in action, see the
[`orderly.sharedfile`](https://mrc-ide.github.io/orderly.sharedfile/)
package.

## Examples

``` r
# Two roots, one local and one representing some remote orderly location:
local <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c702370fa'
remote <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c10b72c0b'

# We create a packet in the remote root:
orderly_run("data", root = remote)
#> ℹ Starting packet 'data' `20260121-095923-5a40ed08` at 2026-01-21 09:59:23.356703
#> > orderly_description(
#> +   display = "A demo data set")
#> > x <- jitter(1:30)
#> > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
#> > d <- data.frame(x, y)
#> > orderly_artefact("data.rds", description = "A synthetic dataset")
#> > saveRDS(d, "data.rds")
#> ✔ Finished running data.R
#> ℹ Finished 20260121-095923-5a40ed08 at 2026-01-21 09:59:23.381821 (0.02511787 secs)
#> [1] "20260121-095923-5a40ed08"

# Add the remote as a path location to the local root:
orderly_location_add_path("remote", remote, root = local)
#> ℹ Testing location
#> ✔ Location configured successfully
#> ✔ Added location 'remote' (path)

# Pull metadata from 'remote' into our local version
orderly_location_fetch_metadata(root = local)
#> ℹ Fetching metadata from 1 location: 'remote'
#> ✔ Found 1 packet at 'remote', of which 1 is new

# Pull a packet into our local version
orderly_location_pull(quote(latest(name == "data")), root = local)
#> ℹ Pulling 1 packet: '20260121-095923-5a40ed08'
#> ℹ Looking for suitable files already on disk
#> ℹ Need to fetch 2 files (822 B) from 1 location
#> ⠙ Fetching file 1/2 (227 B) from 'remote' | ETA:  0s [3ms]
#> ✔ Fetched 2 files (822 B) from 'remote' in 25ms.
#> 
#> ✔ Unpacked 1 packet

# Drop the location
orderly_location_remove("remote", root = local)
```
