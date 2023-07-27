##' Evaluate a query against the outpack database, returning a vector
##' of matching packet ids.  Note that by default this only searches
##' through packets that are unpacked and available for direct use on
##' this computer; to search within packets known to other locations
##' (and that we might know about via their metadata) you will need to
##' use the `options` argument.
##'
##' @title Query outpack's database
##'
##' @param ... Arguments passed through to [orderly2::orderly_query],
##'   perhaps just a query expression
##'
##' @param parameters Optionally, a named list of parameters to substitute
##'   into the query (using the `this:` prefix)
##'
##' @param envir Optionally, an environment to substitute into the
##'   query (using the `environment:` prefix). The default here is to
##'   use the calling environment, but you can explicitly pass this in
##'   if you want to control where this lookup happens.
##'
##' @param options Optionally, a [orderly2::orderly_search_options]
##'   object for controlling how the search is performed, and which
##'   packets should be considered in scope. If not provided, default
##'   options are used (i.e., `orderly2::orderly_search_options()`)
##'
##' @inheritParams orderly_metadata
##'
##' @return A character vector of matching ids. In the case of no
##'   match from a query returning a single value (e.g., `latest(...)`
##'   or `single(...)`) this will be a character missing value
##'   (`NA_character_`)
##'
##' @export
orderly_search <- function(..., parameters = NULL, envir = parent.frame(),
                           options = NULL, root = NULL) {
  root <- root_open(root, locate = TRUE, require_orderly = FALSE)
  query <- as_orderly_query(...)
  options <- as_orderly_search_options(options)
  orderly_query_eval(query, parameters, envir, options, root)
}


##' Options for controlling how packet searches are carried out, for
##' example via [orderly2::orderly_search] and
##' [orderly2::orderly_run]. The details here are never included in
##' the metadata alongside the query (that is, they're not part of the
##' query even though they affect it).
##'
##' @title Packet search options
##'
##' @param location Optional vector of locations to pull from. We
##'   might in future expand this to allow wildcards, exceptions, or
##'   numeric values corresponding to the location priority (and then
##'   it's possible we'll change the name).
##'
##' @param allow_remote Logical, indicating if we should allow
##'   packets to be found that are not currently unpacked (i.e., are
##'   known only to a location that we have metadata from). If this is
##'   `TRUE`, then in conjunction with
##'   [orderly2::orderly_dependency] you might pull a large
##'   quantity of data.
##'
##' @param pull_metadata Logical, indicating if we should pull
##'   metadata immediately before the search. If `location` is
##'   given, then we will pass this through to
##'   [orderly2::outpack_location_pull_metadata] to filter locations to
##'   update.  If pulling many packets in sequence, you *will* want to
##'   update this option to `FALSE` after the first pull.
##'
##' @return An object of class `orderly_search_options` which should
##'   not be modified after creation (but see note about `pull_metadata`)
##'
##' @export
orderly_search_options <- function(location = NULL,
                                   allow_remote = FALSE,
                                   pull_metadata = FALSE) {
  ## TODO: Later, we might allow something like "before" here too to
  ## control searching against some previous time on a location.
  if (!is.null(location)) {
    assert_character(location)
  }
  assert_scalar_logical(allow_remote)
  assert_scalar_logical(pull_metadata)
  ret <- list(location = location,
              allow_remote = allow_remote,
              pull_metadata = pull_metadata)
  class(ret) <- "orderly_search_options"
  ret
}


as_orderly_search_options <- function(x, name = deparse(substitute(x))) {
  if (!is.name(name)) {
    name <- "options"
  }
  if (is.null(x)) {
    return(orderly_search_options())
  }
  if (inherits(x, "orderly_search_options")) {
    return(x)
  }
  if (!is.list(x)) {
    stop(sprintf(
      "Expected '%s' to be an 'orderly_search_options' or a list of options",
      name),
      call. = FALSE)
  }
  err <- setdiff(names(x), names(formals(orderly_search_options)))
  if (length(err) > 0) {
    stop(sprintf("Invalid option passed to 'orderly_search_options': %s",
                 paste(squote(err), collapse = ", ")),
         call. = FALSE)
  }
  do.call(orderly_search_options, x)
}


orderly_query_eval <- function(query, parameters, envir, options, root) {
  assert_is(query, "orderly_query")
  assert_is(options, "orderly_search_options")
  assert_is(root, "outpack_root")
  validate_parameters(parameters)
  assert_is(envir, "environment")
  ## It's simple enough here to pre-compare the provided parameters
  ## with query$info$parameters, but we already have nicer error
  ## reporting at runtime that shows the context of where the
  ## parameter is used.
  index <- new_query_index(root, options)

  ## All the (possibly mutable) bits that define our query environment.
  query_env <- list(index = index,
                    parameters = parameters,
                    envir = envir,
                    subquery = list2env(query$subquery))

  query_eval(query$value, query_env)
}


query_eval <- function(query, query_env) {
  switch(query$type,
         literal = query$value,
         lookup = query_eval_lookup(query, query_env),
         empty = query_eval_empty(query, query_env),
         group = query_eval_group(query, query_env),
         test = query_eval_test(query, query_env),
         latest = query_eval_latest(query, query_env),
         single = query_eval_single(query, query_env),
         subquery = query_eval_subquery(query, query_env),
         dependency = query_eval_dependency(query, query_env),
         ## Normally unreachable
         stop("Unhandled expression [outpack bug - please report]"))
}


query_eval_latest <- function(query, query_env) {
  if (length(query$args) == 0) {
    candidates <- query_env$index$index$id
  } else {
    candidates <- query_eval(query$args[[1]], query_env)
  }
  if (length(candidates) == 0) NA_character_ else last(candidates)
}


query_eval_single <- function(query, query_env) {
  candidates <- query_eval(query$args[[1]], query_env)
  len <- length(candidates)
  if (len == 0) {
    query_eval_error("Query did not find any packets",
                     query$expr, query$context)
  } else if (len > 1) {
    query_eval_error(
      sprintf("Query found %d packets, but expected exactly one", len),
      query$expr, query$context)
  }
  candidates
}


## TODO: we probably also need to make sure that none of this is
## recursive (e.g., subquery A referencing B etc; do that in the parse
## phase; things are now set up to support this).
query_eval_subquery <- function(query, query_env) {
  name <- query$args$name
  subquery <- query_env$subquery
  if (!subquery[[name]]$evaluated) {
    ## TODO: should we really not allow parameters here? Feels like
    ## they might be relevant?
    subquery_env <- list(index = query_env$index,
                         parameters = NULL,
                         envir = query_env$envir,
                         subquery = subquery)
    result <- query_eval(subquery[[name]]$parsed, subquery_env)
    subquery[[name]]$result <- result
    subquery[[name]]$evaluated <- TRUE
  }
  subquery[[name]]$result
}


query_eval_dependency <- function(query, query_env) {
  ## Eval dependency arg without scope, we need to find all packets which
  ## were usedby or used in this one, so find parents/children without scope
  ## and apply scope later when finding the results of the main query.
  id <- query_eval(query$args[[1]], query_env)
  index <- query_env$index
  switch(query$name,
         usedby = index$get_packet_depends(id, query$args[[2]]$value),
         uses = index$get_packet_uses(id, query$args[[2]]$value))
}


query_eval_lookup <- function(query, query_env) {
  index <- query_env$index
  switch(query$name,
         name = index$index$name,
         id = index$index$id,
         parameter = lapply(index$index$parameters, "[[", query$query),
         this = query_eval_lookup_get(
           query$query, query_env$parameters, "parameters",
           query$expr, query$context),
         environment = query_eval_lookup_get(
           query$query, query_env$envir, "environment",
           query$expr, query$context),
         ## Normally unreachable
         stop("Unhandled lookup [outpack bug - please report]"))
}


query_eval_empty <- function(query, query_env) {
  query_env$index$index$id
}


query_eval_group <- function(query, query_env) {
  args <- lapply(query$args, query_eval, query_env)
  switch(query$name,
         "&&" = intersect(args[[1]], args[[2]]),
         "||" = union(args[[1]], args[[2]]),
         "!" = setdiff(query_env$index$index$id, args[[1]]),
         "(" = args[[1]],
         ## Normally unreachable
         stop("Unhandled operator [outpack bug - please report]"))
}


query_eval_test <- function(query, query_env) {
  args <- lapply(query$args, query_eval, query_env)
  i <- query_eval_test_binary(query$name, args[[1]], args[[2]])
  query_env$index$index$id[i]
}


query_eval_test_binary <- function(op, a, b) {
  op <- match.fun(op)
  ## Older versions of R do not allow mixing of zero and non-zero
  ## length inputs here, but we can do this ourselves:
  if (length(a) == 0 || length(b) == 0) {
    return(logical(0))
  }
  run_op <- function(a, b) {
    !is.null(a) && !is.null(b) && is_same_type(a, b) && op(a, b)
  }
  vlapply(Map(run_op, a, b, USE.NAMES = FALSE),
          identity)
}


query_eval_lookup_get <- function(name, data, data_name, expr, context) {
  value <- switch(
    data_name,
    parameters = query_eval_lookup_parameter(name, data),
    environment = query_eval_lookup_environment(name, data),
    stop("unreachable [orderly2 bug]")) # nocov
  if (!value$found) {
    msg <- sprintf("Did not find '%s' within given %s (containing %s)",
                   name, data_name,
                   paste(squote(names(data)), collapse = ", "))
    query_eval_error(msg, expr, context)
  }
  if (!value$valid) {
    msg <- sprintf("The value of '%s' from %s is not suitable as a lookup",
                   name, data_name)
    query_eval_error(msg, expr, context)
  }
  data[[name]]
}


query_eval_lookup_environment <- function(name, envir) {
  tryCatch({
    value <- get(name, envir)
    list(found = TRUE, value = value, valid = is_simple_scalar_atomic(value))
  },
  error = function(e) list(found = FALSE, value = NULL, valid = FALSE))
}


query_eval_lookup_parameter <- function(name, list) {
  value <- list[[name]]
  list(found = name %in% names(list),
       value = value,
       valid = is_simple_scalar_atomic(value))
}
