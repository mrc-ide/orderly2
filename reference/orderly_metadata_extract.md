# Extract metadata from orderly packets

Extract metadata from a group of packets. This is an **experimental**
high-level function for interacting with the metadata in a way that we
hope will be useful. We'll expand this a bit as time goes on, based on
feedback we get so let us know what you think. See Details for how to
use this.

## Usage

``` r
orderly_metadata_extract(
  expr = NULL,
  name = NULL,
  location = NULL,
  allow_remote = NULL,
  fetch_metadata = FALSE,
  extract = NULL,
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

- extract:

  A character vector of columns to extract, possibly named. See Details
  for the format.

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

A `data.frame`, the columns of which vary based on the names of
`extract`; see Details for more information.

## Details

Extracting data from outpack metadata is challenging to do in a way that
works in data structures familiar to R users, because it is naturally
tree structured, and because not all metadata may be present in all
packets (e.g., a packet that does not depend on another will not have a
dependency section, and one that was run in a context without git will
not have git metadata). If you just want the raw tree-structured data,
you can always use
[`orderly_metadata()`](https://mrc-ide.github.io/orderly/reference/orderly_metadata.md)
to load the full metadata for any packet (even one that is not currently
available on your computer, just known about it) and the structure of
the data will remain fairly constant across orderly versions.

However, sometimes we want to extract data in order to ask specific
questions like:

- what parameter combinations are available across a range of packets?

- when were a particular set of packets used?

- what files did these packets produce?

Later we'd like to ask even more complex questions like:

- at what version did the file `graph.png` change?

- what inputs changed between these versions?

...but being able to answer these questions requires a similar approach
to interrogating metadata across a range of packets.

The `orderly_metadata_extract` function aims to simplify the process of
pulling out bits of metadata and arranging it into a `data.frame` (of
sorts) for you. It has a little mini-language in the `extract` argument
for doing some simple rewriting of results, but you can always do this
yourself.

In order to use function you need to know what metadata are available;
we will expand the vignette with more worked examples here to make this
easier to understand. The function works on top-level keys, of which
there are:

- id: the packet id (this is always returned)

- name: the packet name

- parameters: a key-value pair of values, with string keys and atomic
  values. There is no guarantee about presence of keys between packets,
  or their types.

- time: a key-value pair of times, with string keys and time values (see
  [DateTimeClasses](https://rdrr.io/r/base/DateTimeClasses.html); these
  are stored as seconds since 1970 in the actual metadata). At present
  `start` and `end` are always present.

- files: files present in each packet. This is a `data.frame` (per
  packet), each with columns `path` (relative), `size` (in bytes) and
  `hash`.

- depends: dependencies used each packet. This is a `data.frame` (per
  packet), each with columns `packet` (id), `query` (string, used to
  find `packet`) and `files` (another `data.frame` with columns `there`
  and `here` corresponding to filenames upstream and in this packet,
  respectively)

- git: either metadata about the state of git or `null`. If given then
  `sha` and `branch` are strings, while `url` is an array of
  strings/character vector (can have zero, one or more elements).

- session: some information about the session that the packet was run in
  (this is unstandardised, and even the orderly version may change)

- custom: additional metadata added by its respective engine. For
  packets run by `orderly`, there will be an `orderly` field here, which
  is itself a list:

  - artefacts: A [data.frame](https://rdrr.io/r/base/data.frame.html)
    with artefact information, containing columns `description` (a
    string) and `paths` (a list column of paths).

  - shared: A [data.frame](https://rdrr.io/r/base/data.frame.html) of
    the copied shared resources with their original name (`there`) and
    name as copied into the packet (`here`).

  - role: A [data.frame](https://rdrr.io/r/base/data.frame.html) of
    identified roles of files, with columns `path` and `role`.

  - description: A list of information from
    [`orderly_description()`](https://mrc-ide.github.io/orderly/reference/orderly_description.md)
    with human-readable descriptions and tags.

  - session: A list of information about the session as run, with a list
    `platform` containing information about the platform (R version as
    `version`, operating system as `os` and system name as `system`) and
    `packages` containing columns `package` , `version` and `attached`.

The nesting here makes providing a universally useful data format
difficult; if considering files we have a `data.frame` with a `files`
column, which is a list of `data.frame`s; similar nestedness applies to
`depends` and the orderly custom data. However, you should be able to
fairly easily process the data into the format you need it in.

The simplest extraction uses names of top-level keys:

    extract = c("name", "parameters", "files")

This creates a data.frame with columns corresponding to these keys, one
row per packet. Because `name` is always a string, it will be a
character vector, but because `parameters` and `files` are more complex,
these will be list columns.

You must not provide `id`; it is always returned and always first as a
character vector column. If your extraction could possibly return data
from locations (i.e., you have `allow_remote = TRUE` or have given a
value for `location`) then we add a logical column `local` which
indicates if the packet is local to your archive, meaning that you have
all the files from it locally.

You can rename the columns by providing a name to entries within
`extract`, for example:

    extract = c("name", pars = "parameters", "files")

is the same as above, except that that the `parameters` column has been
renamed `pars`.

More interestingly, we can index into a structure like `parameters`;
suppose we want the value of the parameter `x`, we could write:

    extract = c(x = "parameters.x")

which is allowed because for *each packet* the `parameters` element is a
list.

However, we do not know what type `x` is (and it might vary between
packets). We can add that information ourselves though and write:

    extract = c(x = "parameters.x is number")

to create an numeric column. If any packet has a value of `x` that is
non-integer, your call to `orderly_metadata_extract` will fail with an
error, and if a packet lacks a value of `x`, a missing value of the
appropriate type will be added.

Note that this does not do any coercion to number, it will error if a
non-NULL non-numeric value is found. Valid types for use with
`is <type>` are `boolean`, `number` and `string` (note that these differ
slightly from R's names because we want to emphasise that these are
*scalar* quantities; also note that there is no `integer` here as this
may produce unexpected errors with integer-like numeric values). You can
also use `list` but this is the default. Things in the schema that are
known to be scalar atomics (such as `name`) will be automatically
simplified.

You can index into the array-valued elements (`files` and `depends`) in
the same way as for the object-valued elements:

    extract = c(file_path = "files.path", file_hash = "files.hash")

would get you a list column of file names per packet and another of
hashes, but this is probably less useful than the `data.frame` you'd get
from extracting just `files` because you no longer have the hash
information aligned.

You can index fairly deeply; it should be possible to get the orderly
"display name" with:

    extract = c(display = "custom.orderly.description.display is string")

If the path you need to extract has a dot in it (most likely a package
name for a plugin, such as `custom.orderly.db`) you need to escape the
dot with a backslash (so, `custom.orderly\.db`). You will probably need
two slashes or use a raw string (in recent versions of R).

## Custom 'orderly' metadata

Within `custom.orderly`, additional fields can be extracted. The format
of this is subject to change, both in the stored metadata and schema (in
the short term) and in the way we deserialise it. It is probably best
not to rely on this right now, and we will expand this section when you
can.

## Examples

``` r
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c38ca5be5'

# Generate a bunch of packets:
suppressMessages({
  orderly_run("data", echo = FALSE, root = path)
  for (n in c(2, 4, 6, 8)) {
    orderly_run("parameters", list(max_cyl = n), echo = FALSE, root = path)
  }
})

# Without a query, we get a summary over all packets; this will
# often be too much:
orderly_metadata_extract(root = path)
#>                         id       name parameters
#> 1 20260121-095925-c1cd7f19       data           
#> 2 20260121-095925-ce34f892 parameters          2
#> 3 20260121-095925-ddcc9ea3 parameters          4
#> 4 20260121-095925-ec2ba08a parameters          6
#> 5 20260121-095925-fb882b11 parameters          8

# Pass in a query to limit things:
meta <- orderly_metadata_extract(quote(name == "parameters"), root = path)
meta
#>                         id       name parameters
#> 1 20260121-095925-ce34f892 parameters          2
#> 2 20260121-095925-ddcc9ea3 parameters          4
#> 3 20260121-095925-ec2ba08a parameters          6
#> 4 20260121-095925-fb882b11 parameters          8

# The parameters are present as a list column:
meta$parameters
#> [[1]]
#> [[1]]$max_cyl
#> [1] 2
#> 
#> 
#> [[2]]
#> [[2]]$max_cyl
#> [1] 4
#> 
#> 
#> [[3]]
#> [[3]]$max_cyl
#> [1] 6
#> 
#> 
#> [[4]]
#> [[4]]$max_cyl
#> [1] 8
#> 
#> 

# You can also lift values from the parameters into columns of their own:
orderly_metadata_extract(
  quote(name == "parameters"),
  extract = c(max_cyl = "parameters.max_cyl is number"),
  root = path)
#>                         id max_cyl
#> 1 20260121-095925-ce34f892       2
#> 2 20260121-095925-ddcc9ea3       4
#> 3 20260121-095925-ec2ba08a       6
#> 4 20260121-095925-fb882b11       8
```
