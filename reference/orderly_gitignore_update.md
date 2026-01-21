# Update a gitignore file

Update a gitignore, which is useful to prevent accidentally committing
files to source control that are generated. This includes artefacts,
shared resources and dependencies (within a report directory) or at the
global level all the contents of the `.outpack` directory, the draft
folder and the archive directory.

## Usage

``` r
orderly_gitignore_update(name, root = NULL)
```

## Arguments

- name:

  The name of the gitignore file to update, or the string "(root)"

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function **does** require
  that the directory is configured for orderly, and not just outpack
  (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

## Value

Nothing, called for its side effects

## Details

If this function fails with a message
`Can't edit '.gitignore', markers are corrupted`, then look for the
special markers within the `.gitignore` file. It should look like

    # ---VVV--- added by orderly ---VVV----------------
    # Don't manually edit content between these markers
    ... patterns
    # ---^^^--- added by orderly ---^^^----------------

We can't edit the file if:

- any of these lines appears more than once in the file

- there is anything between the first two lines

- they are not in this order

If you get the error message, search and remove these lines and rerun.

## Examples

``` r
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c5d94705c'

# Update core orderly ignorables:
orderly_gitignore_update("(root)", root = path)
#> ✔ Wrote '.gitignore'
cli::cli_code(readLines(file.path(path, ".gitignore")))
#> # ---VVV--- added by orderly ---VVV----------------
#> # Don't manually edit content between these markers
#> .outpack
#> orderly_envir.yml
#> draft
#> archive
#> # ---^^^--- added by orderly ---^^^----------------

# Report-specific ignores:
orderly_gitignore_update("data", root = path)
#> ✔ Wrote 'src/data/.gitignore'
cli::cli_code(readLines(file.path(path, "src", "data", ".gitignore")))
#> # ---VVV--- added by orderly ---VVV----------------
#> # Don't manually edit content between these markers
#> data.rds
#> # ---^^^--- added by orderly ---^^^----------------
```
