##' Copy a simple orderly example for use in the docs. This function
##' should not form part of your workflow!
##'
##' @title Copy a simple orderly example
##'
##' @param names Optionally, names of the reports to copy.  The
##'   default is to copy all reports.
##'
##' @param example The name of the example to copy. Currently only
##'   "simple" and "demo" are supported.
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
##' path <- orderly2::orderly_example("demo")
##' orderly2::orderly_list_src(root = path)
##'
##' fs::dir_delete(path)
orderly_example <- function(..., names = NULL, example = "demo", dest = NULL) {
  if (is.null(dest)) {
    dest <- tempfile("orderly2_ex_")
  } else {
    assert_scalar_character(dest)
    if (file.exists(dest)) {
      cli::cli_abort("The path '{dest}' must not exist")
    }
  }
  src <- orderly_example_path(example)
  orderly_init(..., root = dest)
  path_src <- file.path(src, "src")
  if (is.null(names)) {
    fs::dir_copy(path_src, dest)
  } else {
    fs::dir_create(file.path(dest, "src"))
    fs::dir_copy(file.path(path_src, names), file.path(dest, "src"))
  }
  if (file.exists(file.path(src, "shared"))) {
    fs::dir_copy(file.path(src, "shared"), dest)
  }
  invisible(dest)
}


##' Show a file from within one of the examples.  This function exists
##' for use within orderly help files, vignettes and tutorials and is
##' not meant to form part of your workflows, unless you are doing
##' something very peculiar.
##'
##' All orderly examples here are runnable, though some will naturally
##' have some pre-requisites (e.g., using a dependency will require
##' that the dependency has been run first).
##'
##' @title Show an example file
##'
##' @param example The name of the example to look in.  The default
##'   `demo` is a sprawling set of source designed to show off
##'   different orderly features.
##'
##' @param name The name of the report within the example.
##'
##' @param file The name of the file within the report.  The default
##'   is to show the main orderly file (i.e., `<name>.R`)
##'
##' @param ... Arguments passed through to [orderly2::orderly_init()]
##'
##' @return Nothing, called for its side effects only.
##' @export
##' @examples
##' # We use constructions like this in the help, to show off features
##' # of orderly:
##' orderly_example_show("data")
##'
##' # You can run this example:
##' path <- orderly_example("demo")
##' orderly_run("data", root = path)
orderly_example_show <- function(name, file = NULL, example = "demo") {
  src <- orderly_example_path(example)
  valid <- dir(file.path(src, "src"))
  match_value(name, valid)
  filename <- file.path("src", name, file %||% paste0(name, ".R"))
  show_file(file.path(src, filename), filename)
  invisible()
}


orderly_example_path <- function(name, call = parent.frame()) {
  path <- orderly2_file("examples")
  valid <- dir(path)
  match_value(name, valid, call = call)
  file.path(path, name)
}
