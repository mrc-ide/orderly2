hash_file <- function(path, algorithm = "sha256") {
  assert_file_exists(path)
  con <- file(path, open = "rb")
  on.exit(close(con))
  hash_data(con, algorithm)
}


hash_files <- function(paths, algorithm = "sha256", named = FALSE) {
  vcapply(paths, hash_file, algorithm, USE.NAMES = named)
}


hash_data <- function(data, algorithm) {
  assert_scalar_character(algorithm)
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
