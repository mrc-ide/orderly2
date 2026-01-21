# Set search options for interactive use

Set search options for interactive use of orderly; see
[`orderly_dependency()`](https://mrc-ide.github.io/orderly/reference/orderly_dependency.md)
and
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md)
for details. This applies only for the current session, but applies to
all interactive uses of orderly functions that might have received a
copy of the search options (`location`, `allow_remote` and
`fetch_metadata`) via
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md).
Calling with no arguments resets to the defaults.

## Usage

``` r
orderly_interactive_set_search_options(
  location = NULL,
  allow_remote = NULL,
  fetch_metadata = FALSE
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

- fetch_metadata:

  Logical, indicating if we should pull metadata immediately before the
  search. If `location` is given, then we will pass this through to
  [`orderly_location_fetch_metadata()`](https://mrc-ide.github.io/orderly/reference/orderly_location_fetch_metadata.md)
  to filter locations to update. If pulling many packets in sequence,
  you *will* want to update this option to `FALSE` after the first pull,
  otherwise it will update the metadata between every packet, which will
  be needlessly slow.

## Value

Nothing, called for its side effects

## Examples

``` r
# enable fetching packets from remote locations in this session
orderly_interactive_set_search_options(allow_remote = TRUE)
# ... your interactive session
# reset to defaults
orderly_interactive_set_search_options()
```
