##' Explain how a query has or has not matched. This is experimental
##' and the output will change. At the moment, it can tell you why a
##' query matches, or if fails to match based on one of a number of
##' `&&`-ed together clauses.
##'
##' @title Explain a query
##'
##' @inheritParams orderly_search
##' @inheritParams orderly_search_options
##'
##' @return An object of class `orderly_query_explain`, which can be
##'   inspected (contents subject to change) and which has a print
##'   method which will show a user-friendly summary of the query
##'   result.
##'
##' @export
##' @examples
##' suppressMessages({
##'   orderly_run("data", echo = FALSE, root = path)
##'   orderly_run("depends", echo = FALSE, root = path)
##'   for (n in c(2, 4, 6, 8)) {
##'     orderly_run("parameters", list(max_cyl = n), echo = FALSE, root = path)
##'   }
##' })
##'
##' # Explain why a query matches some packets:
##' orderly_query_explain("parameter:max_cyl > 2 && name == 'parameters'",
##'                       root = path)
##'
##' # Or misses:
##' orderly_query_explain("parameter:max_cyl > 2 && name == 'data'",
##'                       root = path)
orderly_query_explain <- function(expr, name = NULL, scope = NULL,
                                  subquery = NULL, parameters = NULL,
                                  envir = parent.frame(),
                                  location = NULL,
                                  allow_remote = NULL,
                                  root = NULL) {
  root <- root_open(root, require_orderly = FALSE)
  query <- as_orderly_query(expr, name, scope, subquery)
  found <- orderly_search(query, parameters = parameters, envir = envir,
                          location = location,
                          allow_remote = allow_remote,
                          fetch_metadata = FALSE,
                          root = root)
  query_simplified <- query_simplify(query)
  ret <- list(found = found,
              n = length(stats::na.omit(found)), # latest() returns NA
              outer = query_simplified$outer,
              parts = list())

  for (name in names(query_simplified$parts)) {
    expr <- query_simplified$parts[[name]]
    found <- orderly_search(expr,
                            parameters = parameters,
                            envir = envir,
                            location = location,
                            allow_remote = allow_remote,
                            fetch_metadata = FALSE,
                            root = root)
    ret$parts[[name]] <- list(
      name = name,
      str = deparse_query(expr, NULL, NULL),
      expr = expr,
      n = length(found),
      found = found)
  }

  ## In the case where we have exactly one 'n == 0' case we can try
  ## and find more near misses, but this will be a follow-on PR. The
  ## idea is that we will identify the offending bit like:
  ##
  ## > parameter:x == value
  ##
  ## Then we evaluate the rest of the query - all the bits without
  ## this chunk to get the context. Then look to see how we can
  ## permute the name of the parameter to get a near miss, or the
  ## value to get a near miss.
  n <- vnapply(ret$parts, "[[", "n")
  if (length(n) > 1 && all(n > 0)) {
    ret$pairs <- lapply(pairs(names(query_simplified$parts)), function(p) {
      a <- p[[1]]
      b <- p[[2]]
      found <- intersect(ret$parts[[a]]$found, ret$parts[[b]]$found)
      list(a = a, b = b, found = found, n = length(found))
    })
  }

  class(ret) <- "orderly_query_explain"
  ret
}

##' @export
print.orderly_query_explain <- function(x, ...) {
  outer_open <- paste(sprintf("%s(", x$outer), collapse = "")
  outer_close <- paste(strrep(")", length(x$outer)))
  outer_parts <- paste(cli::style_bold(names(x$parts)), collapse = " && ")
  summary <- paste0(outer_open, outer_parts, outer_close)

  cli::cli_inform("Evaluated query: '{summary}' and found {x$n} packet{?s}")
  for (el in x$parts) {
    cli::cli_li("{.strong {el$name}} ({el$str}): {el$n} packet{?s}")
  }

  if (!is.null(x$pairs)) {
    cli::cli_li("Pairwise combinations:")
    ulid <- cli::cli_ul()
    for (el in x$pairs) {
      cli::cli_li("{.strong {el$a}} && {.strong {el$b}}: {el$n} packet{?s}")
    }
    cli::cli_end(ulid)
  }

  invisible(x)
}


query_simplify <- function(query) {
  query <- query$value

  outer <- character()
  while (!is.null(query)) {
    if (query$type %in% c("latest", "single")) {
      outer <- c(outer, query$type)
      query <- if (length(query$args) == 0) NULL else query$args[[1]]
    } else if (query$type == "group" && query$name == "(") {
      outer <- c(outer, query$name)
      query <- if (length(query$args) == 0) NULL else query$args[[1]]
    } else {
      break
    }
  }

  parts <- list()
  if (!is.null(query)) {
    while (query$type == "group" && query$name == "&&") {
      parts <- c(list(query$args[[2]]$expr), parts)
      query <- query$args[[1]]
    }
    parts <- c(list(query$expr), parts)
  }
  names(parts) <- LETTERS[seq_along(parts)]

  list(outer = outer, parts = parts)
}
