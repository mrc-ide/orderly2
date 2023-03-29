##' Declare that a file, or group of files, are an orderly
##' resource. By explicitly declaring files as resources orderly will
##' mark the files as immutable inputs and validate that your analysis
##' does not modify them when run with [orderly_run()]
##'
##' @title Declare orderly resources
##'
##' @param files Any number of names of files
##'
##' @return Undefined
##'
##' @export
orderly_resource <- function(files) {
  assert_character(files)

  ## TODO: check that the files actually exist

  ## TODO: we need to register the active packet, or just pull it from
  ## outpack - either is likely ok really.

  p <- get_active_packet()
  if (!is.null(p)) {
    browser()
  }

  invisible()
}


static_orderly_resource <- function(files) {
  list(files = static_character_vector(files))
}


##' Declare an artefact. By doing this you turn on a number of orderly
##' features; see Details below. You can have multiple calls to this
##' function within your orderly script.
##'
##' (1) files matching this will *not* be copied over from the src
##' directory to the draft directory unless they are also listed as a
##' resource with [orderly_resource()]. This feature is only enabled
##' if you call this function from the top level of the orderly script
##' and if it contains only string literals (no variables).
##'
##' (2) if your script fails to produce these files, then
##' [orderly_run()] will fail, guaranteeing that your task does really
##' produce the things you need it to.
##'
##' (3) within the final metadata, your artefacts will have additional
##' metadata; the description that you provide and a grouping
##'
##' @title Declare orderly artefacts
##'
##' @param description The name of the artefact
##'
##' @param files The files within this artefact
##'
##' @return Undefined
##'
##' @export
orderly_artefact <- function(description, files) {
  assert_scalar_character(description)
  assert_character(files)

  p <- get_active_packet()
  if (!is.null(p)) {
    browser()
  }

  invisible()
}


static_orderly_artefact <- function(description, files) {
  list(description = static_character_vector(description),
       files = static_character_vector(files))
}


## TODO: orderly_ignore
## TODO: orderly_no_undeclared_artefacts
## TODO: orderly_strict_mode - turn on all the usual file checks and
##   old-style artefact behaviour (also enable globally)
## TODO: orderly_displayname } - possibly as orderly describe?
## TODO: orderly_description }

static_character_vector <- function(x) {
  if (is.character(x)) {
    x
  } else if (is_call(x, "c")) {
    x <- lapply(x, static_character_vector)
    x <- if (all(vlapply(x, is.character))) unlist(x, FALSE, FALSE) else NULL
  }
  x
}


static_eval <- function(fn, call) {
  expr <- match.call(fn, call)
  expr[[1]] <- fn
  eval(expr)
}
