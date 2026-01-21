# Details

We use several options (set via R’s
[`options()`](https://rdrr.io/r/base/options.html) system) to control
some `orderly` behaviour.

To set an option for a session, you can use

``` r
options(orderly.whatever = TRUE)
```

If you want to make this permanent, you can add this line to your
`~/.Rprofile` file. The easiest way to edit that file is to run
`usethis::edit_r_profile()` which will find the correct file and open it
in your editor (e.g., RStudio, if you are using that).

### `orderly.quiet`

Control verbosity of some informative messages. If set to `TRUE`, then
we suppress many of the `cli`-based chatty messages.

The option is `FALSE` by default, but it is `TRUE` in `testthat` tests,
so that you should not need to suppress output manually.

### `orderly.index_progress`

Display a progress bar when building the `orderly` index (which can take
a while on large archives).

This option is `TRUE` by default, but also affected by `cli`’s options
`cli.progress_show_after` and `cli.progress_clear`.

### `orderly.schema_validate`

Validate all json produced by `orderly` using
[jsonvalidate](https://docs.ropensci.org/jsonvalidate/). This should be
set to `TRUE` for people developing `orderly` (or `orderly` plugins) but
`FALSE` otherwise. The hope is that enabling validation at development
will mean that all packets will conform to the schema, and then users
will never want or need to worry.

This option is `FALSE` by default.

### `orderly.disable_orderly2_compat`

Disable loading of the `orderly2` compatibility mini-package, designed
to help working with older sources that reference `orderly` as
`orderly2` (see
[`vignette("migrating")`](https://mrc-ide.github.io/orderly/articles/migrating.md)
for details).

This option is `FALSE` by default, but will become `TRUE` in a future
version as we deprecate the old format.

### `orderly.git_error_ignore`

Controls if we check the state of `orderly`’s files within git.
Typically, we want to keep `.outpack` out of version control, but if
this option is `TRUE` then the usual checks are suppressed.

This option is `FALSE` by default.

### `orderly.git_error_is_warning`

Controls if the error thrown for incorrectly version controlled
`.outpack` files should be converted into a warning.

This option is `FALSE` by default.

### `orderly.interactive_parameters_missing_error`

Control if we should prompt for missing old-style parameters. See
[`?orderly_parameters`](https://mrc-ide.github.io/orderly/reference/orderly_parameters.md);
this is now deprecated.

The default for this option is `FALSE`.

## Environment variables

Orderly responds to the environment variables `ORDERLY_ROOT` and
`ORDERLY_SRC_ROOT` which can control where the default option of
`root = NULL` to most `orderly` functions looks. This is semi-public; it
is probably best not to rely on it, but we do use this ourselves to make
the vignettes readable so it’s likely to persist for a while.
