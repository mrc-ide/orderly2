# Query orderly's database

Evaluate a query against the orderly database (within `.outpack/`),
returning a vector of matching packet ids. Note that by default this
only searches through packets that are unpacked and available for direct
use on this computer; to search within packets known to other locations
(and that we might know about via their metadata) you will need to use
the `location`, `allow_remote` and `fetch_metadata` arguments.

## Usage

``` r
orderly_search(
  expr,
  name = NULL,
  scope = NULL,
  subquery = NULL,
  parameters = NULL,
  envir = parent.frame(),
  location = NULL,
  allow_remote = NULL,
  fetch_metadata = FALSE,
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

- scope:

  Optionally, a scope query to limit the packets searched by `pars`

- subquery:

  Optionally, named list of subqueries which can be referenced by name
  from the `expr`.

- parameters:

  Optionally, a named list of parameters to substitute into the query
  (using the `this:` prefix)

- envir:

  Optionally, an environment to substitute into the query (using the
  `environment:` prefix). The default here is to use the calling
  environment, but you can explicitly pass this in if you want to
  control where this lookup happens.

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

A character vector of matching ids. In the case of no match from a query
returning a single value (e.g., `latest(...)` or `single(...)`) this
will be a character missing value (`NA_character_`)

## Examples

``` r
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c7e2cab27'

# Generate a bunch of packets:
suppressMessages({
  orderly_run("data", echo = FALSE, root = path)
  orderly_run("depends", echo = FALSE, root = path)
  for (n in c(2, 4, 6, 8)) {
    orderly_run("parameters", list(max_cyl = n), echo = FALSE, root = path)
  }
})

# By default, search returns everything, which is rarely what you want:
orderly_search(root = path)
#> [1] "20260121-095934-fb431479" "20260121-095935-073358f2"
#> [3] "20260121-095935-1a24ee3a" "20260121-095935-2cd1cc31"
#> [5] "20260121-095935-3ae2c7c9" "20260121-095935-4904e907"

# Restricting by name is common enough that there's a shortcut for
# it:
orderly_search(name = "data", root = path)
#> [1] "20260121-095934-fb431479"
orderly_search(name = "parameters", root = path)
#> [1] "20260121-095935-1a24ee3a" "20260121-095935-2cd1cc31"
#> [3] "20260121-095935-3ae2c7c9" "20260121-095935-4904e907"

# Restrict to a parameter value:
orderly_search(quote(parameter:max_cyl > 4), name = "parameters",
               root = path)
#> [1] "20260121-095935-3ae2c7c9" "20260121-095935-4904e907"
```
