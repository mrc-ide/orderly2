# Construct outpack query

Construct an outpack query, typically then passed through to
[`orderly_search()`](https://mrc-ide.github.io/orderly/reference/orderly_search.md)

## Usage

``` r
orderly_query(expr, name = NULL, scope = NULL, subquery = NULL)
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

## Value

An `orderly_query` object, which should not be modified, but which can
be passed to
[`orderly_search()`](https://mrc-ide.github.io/orderly/reference/orderly_search.md)

## See also

[`vignette("dependencies")`](https://mrc-ide.github.io/orderly/articles/dependencies.md)
and
[`vignette("query")`](https://mrc-ide.github.io/orderly/articles/query.md),
which discuss relationships between dependencies and the query DSL in
more detail.

## Examples

``` r
orderly_query(quote(latest(name == "data")))
#> <orderly_query>: `latest(name == "data")`
```
