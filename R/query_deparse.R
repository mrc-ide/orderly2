#' Format outpack query for displaying to users. It will typically be
#' easier to use the [format] method of `outpack_query` objects.
#'
#' @param query The outpack query to print
#'
#' @param subquery Optionally a named list of subqueries - if given,
#'   each must be a valid deparseable subquery (i.e., a literal or
#'   language object).  Note that you must not provide an
#'   already-deparsed query here or it will get quoted!
#'
#' @param envir Optionally an environment of values to substitute in
#'   for `environment:` lookups; if given, then formatting a query
#'   will turn something like `latest(parameter:x == environment:a)`
#'   into `latest(parameter:x == 1)` (if `a` within `envir` is
#'   1). This can be used to make queries self contained even after
#'   the environment has gone.
#'
#' @return Query expression as a string
#' @export
#'
#' @examples
#' orderly2::outpack_query_format(quote(name == "example"))
#' orderly2::outpack_query_format(
#'   quote(usedby({A})),
#'   subquery = list(A = quote(latest(name == "a"))))
#'
#' orderly2::outpack_query_format(
#'   quote(parameter:x == environment:x))
#' #' orderly2::outpack_query_format(
#'   quote(parameter:x == environment:x), envir = list2env(x = 1))
#'
#' format(orderly2::outpack_query("latest", name = "a"))
outpack_query_format <- function(query, subquery = NULL, envir = NULL) {
  if (!is_deparseable_query(query)) {
    stop("Cannot format query, it must be a language object or be length 1.")
  }

  if (length(subquery) > 0) {
    assert_named(subquery)
    ok <- vlapply(subquery, is_deparseable_query)
    if (!all(ok)) {
      stop(sprintf("Invalid subquery, it must be deparseable: error for %s",
                   paste(squote(names(subquery)[!ok]), collapse = ", ")))
    }
  }

  deparse_query(query, subquery, envir)
}


##' @export
format.outpack_query <- function(x, envir = NULL, ...) {
  deparse_query(x$value$expr, lapply(x$subquery, "[[", "expr"), envir)
}


is_deparseable_query <- function(x) {
  is.language(x) || (is.atomic(x) && length(x) == 1)
}


deparse_query <- function(x, subquery, envir) {
  if (length(x) == 1) {
    return(deparse_single(x))
  }

  fn <- as.character(x[[1]])
  args <- x[-1]

  ## Note this includes invalid operators, even if they are invalid we
  ## still want to return formatted nicely
  prefix_operators <- list("!", "-")
  infix_operators <- list("!", "&&", "||", "==", "!=", "<", "<=", ">", ">=",
                            ":", "<-", "%in%", "+", "-", "*", "/", "&", "|")
  bracket_operators <- list("(" = ")", "{" = "}", "[" = "]")

  if (fn %in% infix_operators && length(args) == 2) {
    query_str <- deparse_infix(fn, args, subquery, envir)
  } else if (fn %in% prefix_operators) {
    query_str <- deparse_prefix(fn, args, subquery, envir)
  } else if (fn %in% names(bracket_operators)) {
    closing <- bracket_operators[[fn]]
    query_str <- deparse_brackets(fn, args, subquery, envir, closing)
  } else {
    query_str <- deparse_regular_function(fn, args, subquery, envir)
  }
  query_str
}

deparse_single <- function(x) {
  str <- as.character(x)
  if (is.character(x)) {
    str <- paste0('"', str, '"')
  } else if (is.call(x)) {
    str <- paste0(str, "()")
  }
  str
}

deparse_prefix <- function(fn, args, subquery, envir) {
  deparse_regular_function(fn, args, subquery, envir,
                           opening_bracket = "", closing_bracket = "")
}

deparse_infix <- function(fn, args, subquery, envir) {
  sep <- if (fn == ":") "" else " "
  lhs <- deparse_query(args[[1]], subquery, envir)
  rhs <- deparse_query(args[[2]], subquery, envir)
  if (fn == ":" && identical(lhs, "environment") && is.environment(envir)) {
    value <- query_eval_lookup_environment(rhs, envir)
    if (value$found && value$valid) {
      return(deparse_single(value$value))
    }
  }
  paste(lhs, fn, rhs, sep = sep)
}

deparse_brackets <- function(fn, args, subquery, envir, closing) {
  if (fn == "[") {
    func <- args[[1]]
    args <- args[-1]
  } else {
    func <- ""
  }

  if (fn == "{" && length(subquery) > 0) {
    if (length(args[[1]]) == 1 && is.name(args[[1]])) {
      sub <- subquery[[as.character(args[[1]])]]
      if (!is.null(sub)) {
        args[[1]] <- sub
      }
    }
  }

  deparse_regular_function(func, args, subquery, envir, fn, closing)
}

deparse_regular_function <- function(fn, args, subquery, envir,
                                     opening_bracket = "(",
                                     closing_bracket = ")") {
  arg_str <- paste(vcapply(args, deparse_query, subquery, envir),
                   collapse = ", ")
  paste0(fn, opening_bracket, arg_str, closing_bracket)
}
