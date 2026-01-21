# Packet search options

Options for controlling how packet searches are carried out, for example
via
[`orderly_search()`](https://mrc-ide.github.io/orderly/reference/orderly_search.md)
and
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md).
The details here are never included in the metadata alongside the query
(that is, they're not part of the query even though they affect it).
(**This function is deprecated, please see below.**)

## Usage

``` r
orderly_search_options(
  location = NULL,
  allow_remote = NULL,
  pull_metadata = FALSE
)
```

## Arguments

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

- pull_metadata:

  Logical, indicating if we should pull metadata immediately before the
  search. If `location` is given, then we will pass this through to
  [`orderly_location_fetch_metadata()`](https://mrc-ide.github.io/orderly/reference/orderly_location_fetch_metadata.md)
  to filter locations to update. If pulling many packets in sequence,
  you *will* want to update this option to `FALSE` after the first pull,
  otherwise it will update the metadata between every packet, which will
  be needlessly slow.

## Value

An object of class `orderly_search_options` which should not be modified
after creation (but see note about `fetch_metadata`)

## Details

**DEPRECATED**:
[`orderly_search()`](https://mrc-ide.github.io/orderly/reference/orderly_search.md)
and
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md)
now accept these arguments directly, which is much easier to reason
about and use. A deprecation warning will be thrown by those functions
if you pass `options` in.

## Examples

``` r
orderly_search_options()
#> Warning: Use of 'orderly_search_options' is deprecated
#> ℹ You should just pass these arguments directly into functions that previously
#>   accepted 'options'
#> ℹ Please note that 'pull_metadata' has become 'fetch_metadata'
#> This warning is displayed once every 8 hours.
#> $location
#> NULL
#> 
#> $allow_remote
#> [1] FALSE
#> 
#> $fetch_metadata
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "orderly_search_options"
```
