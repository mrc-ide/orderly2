##' Declare orderly parameters. You should only have one call to this
##' within your file, though this is not enforced! Typically you'd put
##' it very close to the top, though the order does not really matter.
##' Parameters are scalar atomic values (e.g. a string, number or
##' boolean) and defaults must be present literally (i.e., they may
##' not come from a variable itself). Provide `NULL` if you do not
##' have a default, in which case this parameter will be required.
##'
##' @title Declare orderly parameters
##'
##' @param ... Any number of parameters
##'
##' @return Undefined
##'
##' @export
orderly_parameters <- function(...) {
  p <- get_active_packet()
  if (is.null(p)) {
    ## Here, we might prompt for the presence of parameters in the
    ## global environment and prompt for it.
    pars <- static_orderly_parameters(list(...))
    env <- parent.frame()
    check_parameters_interactive(env, pars)
  }

  invisible()
}


static_orderly_parameters <- function(args) {
  if (length(args) == 0L) {
    return(NULL)
  }
  assert_named(args, unique = TRUE, name = "Arguments to 'orderly_parameters'")
  check_parameter_values(args, TRUE)

  args
}


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
  ## TODO: an error here needs to throw a condition that we can easily
  ## handle and or defer; that's not too hard to do though - convert
  ## the error into something with a special class, perhaps make it a
  ## warning in normal R and then register a handler for it in the
  ## main run.
  assert_character(files)
  assert_file_exists(files)

  p <- get_active_packet()
  if (!is.null(p) && length(files) > 0L) {
    outpack::outpack_packet_file_mark(files, "immutable", packet = p)
    p$orderly3$resources <- c(p$orderly3$resources, files)
  }

  invisible()
}


static_orderly_resource <- function(args) {
  list(files = static_character_vector(args$files))
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
  assert_character(files) # also check length >0 ?

  p <- get_active_packet()
  if (!is.null(p)) {
    artefact <- list(description = description, files = files)
    p$orderly3$artefacts <- c(p$orderly3$artefacts, list(artefact))
  }

  invisible()
}


static_orderly_artefact <- function(args) {
  list(description = static_character_vector(args$description),
       files = static_character_vector(args$files))
}


##' Declare a dependency on another packet
##'
##' @title Declare a dependency
##'
##' @param name The name of the packet to depend on
##'
##' @param query The query to search for; often this will simply be
##'   the string `latest`, indicating the most recent version. You may
##'   want a more complex query here though.
##'
##' @param use A named character vector of filenames to copy from the
##'   upstream packet. The name corresponds to the destination name,
##'   so c(here.csv = "there.csv") will take the upstream file
##'   `there.csv` and copy it over as `here.csv`.
##'
##' @return Undefined
##' @export
orderly_depends <- function(name, query, use) {
  assert_scalar_character(name)
  assert_scalar_character(query)

  assert_character(use)
  if (length(use) == 0) {
    stop("'use' must have length of at least 1")
  }
  if (is.null(names(use))) {
    names(use) <- use
  } else {
    is_unnamed <- names(use) == ""
    names(use)[is_unnamed] <- use[is_unnamed]
  }
  assert_named(use, unique = TRUE)

  p <- get_active_packet()
  if (is.null(p)) {
    path <- getwd()
    root <- detect_orderly_interactive_path(path)
    env <- parent.frame()
    id <- outpack::outpack_query(query, env, name = name,
                                 require_unpacked = TRUE,
                                 root = root$outpack)
    outpack::outpack_copy_files(id, use, path, root$outpack)
  } else {
    id <- outpack::outpack_query(query, p$parameters, name = name,
                                 require_unpacked = TRUE,
                                 root = p$root)
    outpack::outpack_packet_use_dependency(id, use, p)
  }

  invisible()
}


static_orderly_depends <- function(args) {
  name <- args$name
  query <- args$query
  use <- args$use

  name <- static_string(name)
  use <- static_character_vector(use)
  ## TODO: allow passing expressions directly in, that will be much
  ## nicer, but possibly needs some care as we do want a consistent
  ## approach to NSE here
  query <- static_string(query)
  if (is.null(name) || is.null(use) || is.null(query)) {
    return(NULL)
  }
  list(name = name, query = query, use = use)
}


static_string <- function(x) {
  if (is.character(x)) {
    x
  } else if (is_call(x, "c") && length(x) == 2 && is.character(x[[2]])) {
    x[[2]]
  } else {
    NULL
  }
}


static_character_vector <- function(x) {
  if (is.character(x)) {
    x
  } else if (is_call(x, "c")) {
    x <- lapply(x[-1], static_character_vector)
    x <- if (all(vlapply(x, is.character))) unlist(x, FALSE, FALSE) else NULL
  } else {
    x <- NULL
  }
  x
}



static_eval <- function(fn, call) {
  if (is_call(call[[1]], "::")) {
    call[[1]] <- call[[1]][[3]]
  }
  args <- as.list(match.call(match.fun(call[[1]]), call)[-1])
  fn(args)
}


current <- new.env(parent = emptyenv())

get_active_packet <- function() {
  current[[getwd()]]
}
