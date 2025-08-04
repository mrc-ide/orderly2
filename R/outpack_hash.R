##' Use orderly2's hashing functions.  This is intended for advanced
##' users, in particular those who want to create hashes that are
##' consistent with orderly2 from within plugins. The default
##' behaviour is to use the same algorithm as used in the orderly root
##' (via the `root` argument, and the usual root location
##' approach). However, if a string is provided for `algorithm` you
##' can use an alternative algorithm.
##'
##' @title Compute a hash
##'
##' @param path The name of the file to hash
##'
##' @param algorithm The name of the algorithm to use, overriding that
##'   in the orderly root.
##'
##' @inheritParams orderly_metadata
##'
##' @rdname orderly_hash
##'
##' @return A string in the format `<algorithm>:<digest>`
##' @export
##' @examples
##' orderly_hash_data("hello", "md5")
##'
##' # If you run this function from within the working directory of an
##' # orderly root, then you can omit the algorithm and it will use
##' # the algorithm used by orderly (which will be sha256):
##' path <- orderly_example()
##' withr::with_dir(path, orderly_hash_data("hello"))
orderly_hash_file <- function(path, algorithm = NULL, root = NULL) {
  if (is.null(algorithm)) {
    root <- root_open(root, require_orderly = FALSE)
    algorithm <- root$config$core$hash_algorithm
  }
  hash_file(path, algorithm, call = environment())
}


##' @param data A string to hash
##' @export
##' @rdname orderly_hash
orderly_hash_data <- function(data, algorithm = NULL, root = NULL) {
  if (is.null(algorithm)) {
    root <- root_open(root, require_orderly = FALSE)
    algorithm <- root$config$core$hash_algorithm
  }
  hash_data(data, algorithm, call = call)
}


hash_file <- function(path, algorithm = "sha256", call = NULL) {
  assert_file_exists(path, call = call)
  con <- file(path, open = "rb")
  on.exit(close(con))
  hash_data(con, algorithm, call)
}


hash_files <- function(paths, algorithm = "sha256", named = FALSE) {
  vcapply(paths, hash_file, algorithm, USE.NAMES = named)
}


hash_data <- function(data, algorithm = "sha256", call = NULL) {
  assert_scalar_character(algorithm, call = call)
  value <- openssl::multihash(data, algorithm)[[algorithm]]
  sprintf("%s:%s", algorithm, as.character(value))
}


hash_parse <- function(hash) {
  re <- "^([[:alnum:]]+):([[:xdigit:]]+)$"
  stopifnot(all(grepl(re, hash))) # TODO: better error
  list(algorithm = sub(re, "\\1", hash),
       value = sub(re, "\\2", hash))
}


hash_validate_file <- function(path, expected, body = NULL, call = NULL) {
  hash_validate(rehash(path, hash_file, expected), expected, squote(path),
                body, call)
}



hash_validate_data <- function(data, expected, name = deparse(substitute(x)),
                               body = NULL, call = NULL) {
  hash_validate(rehash(data, hash_data, expected), expected, name,
                body, call)
}


hash_validate <- function(found, expected, name, body, call) {
  if (found != expected) {
    ## These are hard to print well because the sha256 hash that we
    ## probably have consumes 71 characters, plus 2 for the bullet.
    ##
    ## 7         8
    ## 01234567890
    ## aaa0 given
    ## bcde want
    cli::cli_abort(c("Hash of {name} does not match!",
                     x = "{.strong {found}} found",
                     i = "{.strong {expected}} want",
                     body),
                   call = call)
  }
  invisible(found)
}


## hash function is (thing, algorithm) -> hash
rehash <- function(data, hash_function, expected) {
  algorithm <- hash_parse(expected)$algorithm
  hash_function(data, algorithm)
}
