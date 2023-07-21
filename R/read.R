orderly_read <- function(path) {
  assert_file_exists("orderly.R", workdir = path)
  orderly_read_r(file.path(path, "orderly.R"))
}


orderly_read_r <- function(path) {
  exprs <- parse(file = path)

  inputs <- list()
  artefacts <- list()

  check <- list(orderly_strict_mode = static_orderly_strict_mode,
                orderly_parameters = static_orderly_parameters,
                orderly_resource = static_orderly_resource,
                orderly_description = static_orderly_description,
                orderly_global_resource = static_orderly_global_resource,
                orderly_artefact = static_orderly_artefact,
                orderly_dependency = static_orderly_dependency)

  dat <- set_names(rep(list(NULL), length(check)), names(check))

  single <- c("orderly_strict_mode", "orderly_description")
  top_level <- c("orderly_strict_mode", "orderly_parameters")

  for (e in exprs) {
    if (is_orderly_ns_call(e)) {
      nm <- deparse(e[[1]][[3]])
    } else if (is.recursive(e) && is.name(e[[1]])) {
      nm <- deparse(e[[1]])
    } else {
      nm <- ""
    }
    if (nm %in% names(check)) {
      dat[[nm]] <- c(dat[[nm]], list(static_eval(check[[nm]], e)))
    } else {
      vars <- all.vars(e, TRUE)
      ## TODO: As below, it is possible to return line numbers here,
      ## something when we come to tidy up for users.
      if (any(top_level %in% vars)) {
        stop(sprintf(
          "orderly function %s can only be used at the top level",
          paste(squote(intersect(top_level, vars)), collapse = ", ")))
      }
    }
  }

  for (nm in single) {
    if (length(dat[[nm]]) > 1) {
      stop(sprintf("Only one call to 'orderly2::%s' is allowed", nm))
    }
  }

  ## Rename to make things easier below:
  names(dat) <- sub("^orderly_", "", names(dat))

  ret <- list()
  if (length(dat$strict_mode) > 0) {
    ret$strict <- dat$strict_mode[[1]]
  } else {
    ret$strict <- list(enabled = FALSE)
  }

  if (length(dat$parameters) > 0) {
    ## TODO: once things are working, check for no duplicate parameter
    ## names across multiple calls; leaving this assertion in for
    ## now. Reporting on that well suggests that we should be able to
    ## record the line numbers when we pass off through the file;
    ## that's not too hard to do really, I think we can do that with
    ## getSrcref or similar.
    stopifnot(!anyDuplicated(unlist(lapply(dat$parameters, names))))
    ret$parameters <- unlist(dat$parameters, FALSE, TRUE)
  }

  ## TODO: probably some santisiation required here:
  ##
  ## * what do we do with directories here?
  ## * discourage people from listing orderly.R
  ## * discourage duplicates
  if (length(dat$resource) > 0) {
    ret$resources <- setdiff(unique(unlist(dat$resource, TRUE, FALSE)),
                             "orderly.R")
  }
  if (length(dat$artefact) > 0) {
    ret$artefacts <- dat$artefact
  }
  if (length(dat$dependency) > 0) {
    ret$dependency <- drop_null(dat$dependency, empty = NULL)
  }
  if (length(dat$global_resource) > 0) {
    ret$global_resource <- unlist(dat$global_resource, FALSE, TRUE)
  }

  ret
}


orderly_validate <- function(dat, path) {
}
