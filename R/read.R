orderly_read <- function(path) {
  assert_file_exists("orderly.R", workdir = path)
  orderly_read_r(file.path(path, "orderly.R"))
}


orderly_read_r <- function(path) {
  exprs <- parse(file = path)

  inputs <- list()
  artefacts <- list()

  check <- list(orderly_parameters = static_orderly_parameters,
                orderly_resource = static_orderly_resource,
                orderly_artefact = static_orderly_artefact)
  dat <- set_names(rep(list(NULL), length(check)), names(check))

  for (e in exprs) {
    for (f in names(check)) {
      if (is_orderly_call(e, f)) {
        ## This will error a bit early really, so later we'll
        ## intercept this and throw a careful error with line numbers
        ## etc, or proceed without erroring in some situations.
        dat[[f]] <- c(dat[[f]], list(static_eval(check[[f]], e)))
      }
    }
  }

  names(dat) <- sub("^orderly_", "", names(dat))

  ret <- list()
  if (length(dat$parameters) > 0) {
    ## TODO: once things are working, check for no duplicate parameter
    ## names across multiple calls; leaving this assertion in for
    ## now. Reporting on that well suggests that we should be able to
    ## record the line numbers when we pass off through the file;
    ## that's not too hard to do really, I think we can do that with
    ## getSrcref or similar.
    ##
    ## TODO: we should disallow use of the parameter function in any
    ## deeper expression (at least in strict mode), worth searching
    ## for that and setting up that check here
    stopifnot(!anyDuplicated(unlist(lapply(dat$parameters, names))))
    ret$parameters <- unlist(dat$parameters, FALSE, TRUE)
  }
  if (length(dat$resource) > 0) {
    ret$resources <- unique(unlist(dat$resource, TRUE, FALSE))
  }
  if (length(dat$artefact) > 0) {
    ret$artefacts <- dat$artefact
  }

  ret
}


orderly_validate <- function(dat, path) {
}
