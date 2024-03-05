orderly_read <- function(path, call = NULL) {
  entrypoint_filename <- find_entrypoint_filename(path)
  orderly_parse_file(file.path(path, entrypoint_filename))
}


#' Parse the orderly entrypoint script
#'
#' For expert use only.
#'
#' Parses details of any calls to the orderly_ in-script functions
#' into intermediate representation for downstream use. Also validates
#' that any calls to `orderly_*` in-script functions are well-formed.
#'
#' @param path Path to `orderly_*` script
#'
#' @return Parsed orderly entrypoint script
#' @export
orderly_parse_file <- function(path) {
  assert_file_exists(path)
  exprs <- parse(file = path)
  orderly_parse_expr(exprs, basename(path))
}


#' @param exprs Parsed AST from `orderly_*` script
#' @param filename Name of `orderly_*` file to include in metadata
#'
#' @rdname orderly_parse_file
#' @export
orderly_parse_expr <- function(exprs, filename) {
  assert_is(exprs, "expression")

  inputs <- list()
  artefacts <- list()

  check <- list(orderly_strict_mode = static_orderly_strict_mode,
                orderly_parameters = static_orderly_parameters,
                orderly_resource = static_orderly_resource,
                orderly_description = static_orderly_description,
                orderly_shared_resource = static_orderly_shared_resource,
                orderly_artefact = static_orderly_artefact,
                orderly_dependency = static_orderly_dependency)
  dat <- set_names(rep(list(NULL), length(check)), names(check))

  single <- c("orderly_strict_mode", "orderly_description")
  top_level <- c("orderly_strict_mode", "orderly_parameters")

  for (e in exprs) {
    e <- orderly_read_expr(e, names(check))
    if (e$is_orderly) {
      nm <- e$name
      dat[[nm]] <- c(dat[[nm]], list(static_eval(check[[nm]], e$expr)))
    } else {
      vars <- all.vars(e$expr, TRUE)
      ## TODO: As below, it is possible to return line numbers here,
      ## something when we come to tidy up for users.
      if (any(top_level %in% vars)) {
        err <- intersect(top_level, vars)
        cli::cli_abort(
          "orderly function {squote(err)} can only be used at the top level",
          call = NULL)
      }
    }
  }

  for (nm in single) {
    if (length(dat[[nm]]) > 1) {
      cli::cli_abort("Only one call to 'orderly2::{nm}' is allowed",
                     call = NULL)
    }
  }

  ## Rename to make things easier below:
  names(dat) <- sub("^orderly_", "", names(dat))

  ret <- list(entrypoint_filename = filename)
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
  ## * discourage people from listing orderly files
  ## * discourage duplicates

  if (length(dat$resource) > 0) {
    ret$resources <- setdiff(unique(unlist(dat$resource, TRUE, FALSE)),
                             filename)
  }
  if (length(dat$artefact) > 0) {
    ret$artefacts <- dat$artefact
  }
  if (length(dat$dependency) > 0) {
    ret$dependency <- drop_null(dat$dependency, empty = NULL)
  }
  if (length(dat$shared_resource) > 0) {
    ret$shared_resource <- unlist(dat$shared_resource, FALSE, TRUE)
  }

  ret
}


orderly_read_expr <- function(e, nms) {
  ## We count the following things as top level:
  ##
  ## > orderly2::orderly_fn()
  ## > orderly_fn()
  ## > a <- orderly2::orderly_fn()
  ## > a <- orderly_fn()
  if (is_assignment(e)) {
    return(orderly_read_expr(e[[3]], nms))
  } else if (is_orderly_ns_call(e)) {
    args <- e[-1]
    nm <- deparse(e[[1]][[3]])
    if (nm %in% nms) {
      return(list(is_orderly = TRUE, name = nm, expr = e))
    }
  } else {
    if (is.recursive(e) && is.name(e[[1]])) {
      nm <- deparse(e[[1]])
      if (nm %in% nms) {
        return(list(is_orderly = TRUE, name = nm, expr = e))
      }
    }
  }
  list(is_orderly = FALSE, expr = e)
}


orderly_validate <- function(dat, path) {
}
