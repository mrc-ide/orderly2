orderly_read <- function(path) {
  assert_file_exists("orderly.R", workdir = path)
  orderly_read_r(file.path(path, "orderly.R"))
}


orderly_read_r <- function(path) {
  exprs <- parse(file = path)

  inputs <- list()
  artefacts <- list()

  check <- list(orderly_resource = static_orderly_resource,
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
