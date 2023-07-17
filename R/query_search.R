##' Evaluate a query against the outpack database, returning a vector
##' of matching packet ids.  Note that by default this only searches
##' through packets that are unpacked and available for direct use on
##' this computer; to search within packets known to other locations
##' (and that we might know about via their metadata) you will need to
##' use the `options` argument.
##'
##' @title Query outpack's database
##'
##' @param ... Arguments passed through to [outpack_query], perhaps
##'   just a query expression
##'
##' @param parameters Optionally, a named list of parameters to substitute
##'   into the query (using the `this:` prefix)
##'
##' @param options Optionally, a [orderly2::outpack_search_options]
##'   object for controlling how the search is performed, and which
##'   packets should be considered in scope. If not provided, default
##'   options are used (i.e., `orderly2::outpack_search_options()`)
##'
##' @param root The outpack root. Will be searched for from the
##'   current directory if not given.
##'
##' @return A character vector of matching ids. In the case of no
##'   match from a query returning a single value (e.g., `latest(...)`
##'   or `single(...)`) this will be a character missing value
##'   (`NA_character_`)
##'
##' @export
outpack_search <- function(..., parameters = NULL, options = NULL,
                           root = NULL) {
  root <- outpack_root_open(root, locate = TRUE)
  query <- as_outpack_query(...)
  options <- as_outpack_search_options(options)
  outpack_query_eval(query, parameters, options, root)
}


##' Options for controlling how packet searches are carried out, for
##' example via [orderly2::outpack_search] and
##' [orderly2::outpack_packet_use_dependency]. The details here are
##' never included in the metadata alongside the query (that is,
##' they're not part of the query even though they affect it).
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
##'   `TRUE`, then inconjunction with
##'   [orderly2::outpack_packet_use_dependency] you might pull a large
##'   quantity of data.
##'
##' @param pull_metadata Logical, indicating if we should pull
##'   metadata immediately before the search. If `location` is
##'   given, then we will pass this through to
##'   [orderly2::outpack_location_pull_metadata] to filter locations to
##'   update.  If pulling many packets in sequence, you *will* want to
##'   update this option to `FALSE` after the first pull.
##'
##' @return An object of class `outpack_search_options` which should
##'   not be modified after creation (but see note about `pull_metadata`)
##'
##' @export
outpack_search_options <- function(location = NULL,
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
  class(ret) <- "outpack_search_options"
  ret
}


as_outpack_search_options <- function(x, name = deparse(substitute(x))) {
  if (!is.name(name)) {
    name <- "options"
  }
  if (is.null(x)) {
    return(outpack_search_options())
  }
  if (inherits(x, "outpack_search_options")) {
    return(x)
  }
  if (!is.list(x)) {
    stop(sprintf(
      "Expected '%s' to be an 'outpack_search_options' or a list of options",
      name),
      call. = FALSE)
  }
  err <- setdiff(names(x), names(formals(outpack_search_options)))
  if (length(err) > 0) {
    stop(sprintf("Invalid option passed to 'outpack_search_options': %s",
                 paste(squote(err), collapse = ", ")),
         call. = FALSE)
  }
  do.call(outpack_search_options, x)
}


outpack_query_eval <- function(query, parameters, options, root) {
  assert_is(query, "outpack_query")
  assert_is(options, "outpack_search_options")
  assert_is(root, "outpack_root")
  validate_parameters(parameters)
  ## It's simple enough here to pre-compare the provided parameters
  ## with query$info$parameters, but we already have nicer error
  ## reporting at runtime that shows the context of where the
  ## parameter is used.
  index <- new_query_index(root, options)
  query_eval(query$value, index, parameters, list2env(query$subquery))
}


query_eval <- function(query, index, parameters, subquery) {
  switch(query$type,
         literal = query$value,
         lookup = query_eval_lookup(query, index, parameters),
         empty = query_eval_empty(query, index, parameters, subquery),
         group = query_eval_group(query, index, parameters, subquery),
         test = query_eval_test(query, index, parameters, subquery),
         latest = query_eval_latest(query, index, parameters, subquery),
         single = query_eval_single(query, index, parameters, subquery),
         subquery = query_eval_subquery(query, index, parameters, subquery),
         dependency = query_eval_dependency(query, index, parameters, subquery),
         ## Normally unreachable
         stop("Unhandled expression [outpack bug - please report]"))
}


query_eval_latest <- function(query, index, parameters, subquery) {
  if (length(query$args) == 0) {
    candidates <- index$index$id
  } else {
    candidates <- query_eval(query$args[[1]], index, parameters, subquery)
  }
  if (length(candidates) == 0) NA_character_ else last(candidates)
}


query_eval_single <- function(query, index, parameters, subquery) {
  candidates <- query_eval(query$args[[1]], index, parameters, subquery)
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
query_eval_subquery <- function(query, index, parameters, subquery) {
  name <- query$args$name
  if (!subquery[[name]]$evaluated) {
    ## TODO: should we really not allow parameters here? Feels like
    ## they might be relevant?
    result <- query_eval(subquery[[name]]$parsed, index, parameters = NULL,
                         subquery)
    subquery[[name]]$result <- result
    subquery[[name]]$evaluated <- TRUE
  }
  subquery[[name]]$result
}


query_eval_dependency <- function(query, index, parameters, subquery) {
  ## Eval dependency arg without scope, we need to find all packets which
  ## were usedby or used in this one, so find parents/children without scope
  ## and apply scope later when finding the results of the main query.
  id <- query_eval(query$args[[1]], index, parameters, subquery)
  switch(query$name,
         usedby = index$get_packet_depends(id, query$args[[2]]$value),
         uses = index$get_packet_uses(id, query$args[[2]]$value))
}


query_eval_lookup <- function(query, index, parameters) {
  switch(query$name,
         name = index$index$name,
         id = index$index$id,
         parameter = lapply(index$index$parameters, "[[", query$query),
         this = query_eval_this(query$query, parameters, query$expr,
                                query$context),
         ## Normally unreachable
         stop("Unhandled lookup [outpack bug - please report]"))
}


query_eval_empty <- function(query, index, parameters, subquery) {
  index$index$id
}


query_eval_group <- function(query, index, parameters, subquery) {
  args <- lapply(query$args, query_eval, index, parameters, subquery)
  switch(query$name,
         "&&" = intersect(args[[1]], args[[2]]),
         "||" = union(args[[1]], args[[2]]),
         "!" = setdiff(index$index$id, args[[1]]),
         "(" = args[[1]],
         ## Normally unreachable
         stop("Unhandled operator [outpack bug - please report]"))
}


query_eval_test <- function(query, index, parameters, subquery) {
  args <- lapply(query$args, query_eval, index, parameters, subquery)
  i <- query_eval_test_binary(query$name, args[[1]], args[[2]])
  index$index$id[i]
}


query_eval_test_binary <- function(op, a, b) {
  op <- match.fun(op)
  ## Older versions of R do not allow mixing of zero and non-zero
  ## length inputs here, but we can do this ourselves:
  if (length(a) == 0 || length(b) == 0) {
    return(logical(0))
  }
  vlapply(Map(function(a, b) !is.null(a) && !is.null(b) && op(a, b),
              a, b, USE.NAMES = FALSE),
          identity)
}


query_eval_this <- function(name, parameters, expr, context) {
  if (!(name %in% names(parameters))) {
    msg <- sprintf("Did not find '%s' within given parameters (%s)",
                   name, paste(squote(names(parameters)), collapse = ", "))
    query_eval_error(msg, expr, context)
  }
  parameters[[name]]
}
