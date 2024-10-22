##' Copy a simple orderly example for use in the docs. This function
##' should not form part of your workflow!
##'
##' @title Copy a simple orderly example
##'
##' @param name The name of the example to copy. Currently only
##'   "default" is supported.
##'
##' @param dest The destination. By default we use
##'   `withr::local_tempfile()` which will create a temporary
##'   directory that will clean itself up. This is suitable for use
##'   from the orderly examples, but you may prefer to provide your
##'   own path. The path must not already exist.
##'
##' @param ... Arguments passed through to [orderly2::orderly_init()]
##'
##' @return Invisibly, the path to the example.
##' @export
##' @examples
##' path <- orderly2::orderly_example("default")
##' orderly2::orderly_list_src(root = path)
##'
##' fs::dir_delete(path)
orderly_example <- function(name, ..., dest = NULL) {
  match_value(name, "default", call = environment())
  if (is.null(dest)) {
    dest <- tempfile("orderly2_ex_")
  } else {
    assert_scalar_character(dest)
    if (file.exists(dest)) {
      cli::cli_abort("The path '{dest}' must not exist")
    }
  }
  orderly_init(..., root = dest)
  src <- orderly2_file("example")
  fs::dir_copy(file.path(src, "src"), dest)
  invisible(dest)
}
