# Compute a hash

Use orderly's hashing functions. This is intended for advanced users, in
particular those who want to create hashes that are consistent with
orderly from within plugins. The default behaviour is to use the same
algorithm as used in the orderly root (via the `root` argument, and the
usual root location approach). However, if a string is provided for
`algorithm` you can use an alternative algorithm.

## Usage

``` r
orderly_hash_file(path, algorithm = NULL, root = NULL)

orderly_hash_data(data, algorithm = NULL, root = NULL)
```

## Arguments

- path:

  The name of the file to hash

- algorithm:

  The name of the algorithm to use, overriding that in the orderly root.

- root:

  The path to the root directory, or `NULL` (the default) to search for
  one from the current working directory. This function does not require
  that the directory is configured for orderly, and can be any `outpack`
  root (see
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  for details).

- data:

  A string to hash

## Value

A string in the format `<algorithm>:<digest>`

## Examples

``` r
orderly_hash_data("hello", "md5")
#> [1] "md5:5d41402abc4b2a76b9719d911017c592"

# If you run this function from within the working directory of an
# orderly root, then you can omit the algorithm and it will use
# the algorithm used by orderly (which will be sha256):
path <- orderly_example()
#> ✔ Created orderly root at '/tmp/RtmpoLRnek/orderly_ex_1b9c69bdd926'
withr::with_dir(path, orderly_hash_data("hello"))
#> [1] "sha256:2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
```
