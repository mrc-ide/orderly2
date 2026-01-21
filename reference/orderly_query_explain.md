# Explain a query

Explain how a query has or has not matched. This is experimental and the
output will change. At the moment, it can tell you why a query matches,
or if fails to match based on one of a number of `&&`-ed together
clauses.

## Usage

``` r
orderly_query_explain(
  expr,
  name = NULL,
  scope = NULL,
  subquery = NULL,
  parameters = NULL,
  envir = parent.frame(),
  location = NULL,
  allow_remote = NULL,
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

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function does not require
  that the directory is configured for orderly, and can be any `outpack`
  root (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

An object of class `orderly_query_explain`, which can be inspected
(contents subject to change) and which has a print method which will
show a user-friendly summary of the query result.

## Examples

``` r
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c6096a7e'
suppressMessages({
  orderly_run("data", echo = FALSE, root = path)
  orderly_run("depends", echo = FALSE, root = path)
  for (n in c(2, 4, 6, 8)) {
    orderly_run("parameters", list(max_cyl = n), echo = FALSE, root = path)
  }
})

# Explain why a query matches some packets:
orderly_query_explain("parameter:max_cyl > 2 && name == 'parameters'",
                      root = path)
#> Evaluated query: 'A && B' and found 3 packets
#> • A (parameter:max_cyl > 2): 3 packets
#> • B (name == "parameters"): 4 packets
#> • Pairwise combinations:
#>   • A && B: 3 packets

# Or misses:
orderly_query_explain("parameter:max_cyl > 2 && name == 'data'",
                      root = path)
#> Evaluated query: 'A && B' and found 0 packets
#> • A (parameter:max_cyl > 2): 3 packets
#> • B (name == "data"): 1 packet
#> • Pairwise combinations:
#>   • A && B: 0 packets
```
