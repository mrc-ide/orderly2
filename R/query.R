##' Construct an outpack query, typically then passed through to
##' [orderly2::orderly_search]
##'
##' @title Construct outpack query
##'
##' @param expr The query expression. A `NULL` expression matches everything.
##'
##' @param name Optionally, the name of the packet to scope the query on. This
##'   will be intersected with `scope` arg and is a shorthand way of running
##'   `scope = list(name = "name")`
##'
##' @param scope Optionally, a scope query to limit the packets
##'   searched by `pars`
##'
##' @param subquery Optionally, named list of subqueries which can be
##'   referenced by name from the `expr`.
##'
##' @return An `orderly_query` object, which should not be modified,
##'   but which can be passed to [orderly2::orderly_search()]
##'
##' @export
##' @examples
##' orderly_query(quote(latest(name == "data")))
orderly_query <- function(expr, name = NULL, scope = NULL, subquery = NULL) {
  subquery_envir <- make_subquery_envir(subquery, call = environment())
  expr_parsed <- query_parse(expr, expr, subquery_envir)
  if (!is.null(name)) {
    assert_scalar_character(name, call = environment())
    name_call <- call("==", quote(name), name)
    if (is.null(scope)) {
      scope <- name_call
    } else {
      scope <- call("&&", name_call, scope)
    }
  }
  if (!is.null(scope)) {
    expr_parsed <- query_parse_add_scope(expr_parsed, scope)
  }

  lookup <- query_collect_lookup(expr_parsed, subquery_envir)
  info <- list(
    single = is_expr_single_value(expr_parsed, subquery_envir),
    parameters = lookup$this,
    environment = lookup$environment)

  ret <- list(value = expr_parsed,
              subquery = as.list(subquery_envir),
              info = info)
  class(ret) <- "orderly_query"
  ret
}


##' @export
print.orderly_query <- function(x, ...) {
  query_str <- format(x, ...)
  cli::cli_text("<orderly_query>: {.code {query_str}}")
  invisible(x)
}


expr_is_literal_id <- function(expr, ...) {
  all(vlapply(list(...), is.null)) &&
    is.character(expr) &&
    all(grepl(re_id, expr))
}


as_orderly_query <- function(expr, name = NULL, scope = NULL, subquery = NULL,
                             arg = "expr", call = parent.frame()) {
  if (missing(expr)) {
    expr <- NULL
  }
  if (inherits(expr, "orderly_query")) {
    err <- !is.null(name) || !is.null(scope) || !is.null(subquery)
    if (err) {
      cli::cli_abort(
        "If '{arg}' is an 'orderly_query', no additional arguments allowed",
        arg = arg, call = call)
    }
    expr
  } else {
    orderly_query(expr, name, scope, subquery)
  }
}


query_parse <- function(expr, context, subquery_envir) {
  if (is.character(expr)) {
    if (length(expr) == 1 && grepl(re_id, expr)) {
      ## If we're given a single id, we construct a simple query with it
      expr <- bquote(single(id == .(expr)))
    } else {
      expr <- parse(text = expr, keep.source = FALSE)
      if (length(expr) != 1L) {
        stop("Expected a single expression")
      }
      expr <- expr[[1L]]
    }
  } else if (!(is.null(expr) || is.language(expr))) {
    stop("Invalid input for query")
  }

  ## This is used extensively in orderly, so we'll support it here
  if (identical(expr, quote(latest))) {
    expr <- quote(latest())
  }

  if (is.null(context)) {
    context <- expr
  }

  query_parse_expr(expr, context, subquery_envir)
}


query_functions <- list(
  group = list("(" = 1, "!" = 1, "&&" = 2, "||" = 2),
  test = list("==" = 2, "!=" = 2, "<" = 2, "<=" = 2, ">" = 2, ">=" = 2),
  subquery = list("{" = 1),
  dependency = list(usedby = c(1, 2), uses = c(1, 2)),
  other = list(latest = c(0, 1), single = 1))


query_component <- function(type, expr, context, args, ...) {
  stopifnot(is.character(type) && length(type) == 1)
  structure(
    list(type = type, expr = expr, context = context, args = args, ...),
    class = "orderly_query_component")
}


query_parse_expr <- function(expr, context, subquery_envir) {
  type <- query_parse_check_call(expr, context)
  fn <- switch(type,
               empty = query_parse_empty,
               test = query_parse_test,
               group = query_parse_group,
               latest = query_parse_latest,
               single = query_parse_single,
               subquery = query_parse_subquery,
               dependency = query_parse_dependency,
               ## normally unreachable
               stop("Unhandled expression [outpack bug - please report]"))
  fn(expr, context, subquery_envir)
}


query_parse_empty <- function(expr, context, subquery_envir) {
  query_component("empty", expr, context, list())
}


query_parse_test <- function(expr, context, subquery_envir) {
  args <- lapply(expr[-1], query_parse_value, context)
  name <- deparse(expr[[1]])
  query_component("test", expr, context, args, name = name)
}


query_parse_group <- function(expr, context, subquery_envir) {
  args <- lapply(expr[-1], query_parse_expr, context, subquery_envir)
  name <- deparse(expr[[1]])
  query_component("group", expr, context, args, name = name)
}


query_parse_latest <- function(expr, context, subquery_envir) {
  args <- lapply(expr[-1], query_parse_expr, context, subquery_envir)
  query_component("latest", expr, context, args)
}


query_parse_single <- function(expr, context, subquery_envir) {
  args <- lapply(expr[-1], query_parse_expr, context, subquery_envir)
  query_component("single", expr, context, args)
}


query_parse_add_scope <- function(expr_parsed, scope) {
  if (!is.language(scope)) {
    ## I don't think this is correct:
    stop("Invalid input for `scope`, it must be a language expression.")
  }

  ## Can the scope not access subqueries?
  parsed_scope <- query_parse(scope, scope, emptyenv())
  scoped_functions <- list("latest", "single")
  expr <- expr_parsed$expr

  if (expr_parsed$type == "empty") {
    expr_parsed <- parsed_scope
  } else if (expr_parsed$type %in% scoped_functions) {
    fn <- deparse(expr[[1]])
    ## Include scope inside the top level function call
    if (length(expr_parsed$args) == 0) {
      ## e.g. latest()
      expr_parsed$expr <- call(fn, scope)
      expr_parsed$args <- list(parsed_scope)
    } else {
      ## e.g. latest(name == "x")
      expr_parsed$expr <- call(fn, call("&&", expr[[-1]], scope))
      expr_parsed$args[[1]] <- query_component(
        "group", expr_parsed$expr, expr_parsed$expr,
        list(expr_parsed$args[[1]], parsed_scope), name = "&&")
    }
  } else {
    ## Include scope at end of expression
    expr_parsed$expr <- call("&&", expr, scope)
    expr_parsed <- query_component("group", expr_parsed$expr, expr_parsed$expr,
                                   list(expr_parsed, parsed_scope),
                                   name = "&&")
  }
  expr_parsed
}


is_named_subquery <- function(subquery) {
  ## Subquery is "named" if it is e.g. {sub}, otherwise it is
  ## anonymous e.g. {name == "x"}
  ## anonymous could also be length 1 e.g. `latest` so we need to account
  ## for this by checking if is.name
  sub <- subquery[[1]]
  all_funcs <- unlist(lapply(query_functions, names), use.names = FALSE)
  length(subquery) == 1 && length(sub) == 1 && is.name(sub)
}


query_parse_subquery <- function(expr, context, subquery_envir) {
  subquery <- expr[-1]
  if (is_named_subquery(subquery)) {
    query_name <- deparse(subquery[[1]])
    if (is.null(subquery_envir[[query_name]])) {
      named_subqueries <-
        names(which(vlapply(as.list(subquery_envir), function(x) !x$anonymous)))
      if (length(named_subqueries) > 0) {
        available_queries <- sprintf(
          "Available subqueries are %s.",
          paste0(squote(sort(named_subqueries)), collapse = ", "))
      } else {
        available_queries <- "No named subqueries provided."
      }
      query_parse_error(
        sprintf("Cannot locate subquery named '%s'. %s", query_name,
                available_queries),
        expr, context)
    }
  } else {
    query_name <- add_subquery(NULL, subquery[[1]], context, subquery_envir)
  }
  query_component("subquery", expr, context, args = list(name = query_name))
}


query_parse_dependency <- function(expr, context, subquery_envir) {
  name <- deparse(expr[[1]])
  args <- as.list(expr[-1])
  if (length(args) == 2) {
    if (is.numeric(args[[2]]) && args[[2]] > 0) {
      args[[2]] <- query_parse_value(args[[2]], context, subquery_envir)
    } else {
      query_parse_error(
        sprintf(paste(
          "`depth` argument in '%s()' must be a positive numeric, set",
          "to control the number of layers of parents to recurse through",
          "when listing dependencies. Use `depth = Inf` to search entire",
          "dependency tree."), name),
        expr, context)
    }
  } else {
    args[[2]] <- query_parse_value(Inf, context, subquery_envir)
  }
  args[[2]]$name <- names(args[2]) %||% "depth"
  if (is.call(args[[1]])) {
    args[[1]] <- query_parse_expr(args[[1]], context, subquery_envir)
    if (!is_expr_single_value(args[[1]], subquery_envir)) {
      query_parse_error(
        sprintf(paste(
          "%s must be called on an expression guaranteed to return",
          "a single ID. Try wrapping expression in `latest` or `single`."),
          name),
        expr, context)
    }
  } else {
    args[[1]] <- query_parse_value(args[[1]], context, subquery_envir)
  }
  query_component("dependency", expr, context, args, name = name)
}

## Check if a query component returns a single value
## Guaranteed single valued if one of the following is true
##   * it is function call to latest
##   * it is a function call to single
##   * it is an ID lookup
##   * it is a subquery whose expression is validates one of these conditions
is_expr_single_value <- function(parsed_expr, subquery_envir) {
  if (parsed_expr$type == "subquery") {
    parsed_expr_sub <- subquery_envir[[parsed_expr$args$name]]$parsed
    return(is_expr_single_value(parsed_expr_sub, subquery_envir))
  }
  parsed_expr$type %in% c("latest", "single") ||
    (parsed_expr$type == "test" && (is_id_lookup(parsed_expr$args[[1]]) ||
                                    is_id_lookup(parsed_expr$args[[2]])))
}

is_id_lookup <- function(expr) {
  expr$type == "lookup" && expr$name == "id"
}

is_logical <- function(expr) {
  truthy <- list(TRUE, FALSE, quote(true), quote(false),
                 quote(True), quote(False))
  any(vlapply(truthy, identical, expr))
}

as_logical <- function(expr) {
  if (expr == quote(true) || expr == quote(True)) {
    ret <- TRUE
  } else if (expr == quote(false) || expr == quote(False)) {
    ret <- FALSE
  } else {
    ret <- expr
  }
  ret
}

query_error <- function(msg, expr, context, prefix) {
  if (identical(expr, context)) {
    stop(sprintf("%s\n  - %s %s", msg, prefix, deparse_query(expr, NULL, NULL)),
         call. = FALSE)
  } else {
    width <- max(nchar(prefix), nchar("within"))
    stop(sprintf(
      "%s\n  - %s %s\n  - %s %s",
      msg,
      format(prefix, width = width), deparse_query(expr, NULL, NULL),
      format("within", width = width), deparse_query(context, NULL, NULL)),
      call. = FALSE)
  }
}


query_parse_error <- function(msg, expr, context) {
  query_error(msg, expr, context, "in")
}


query_eval_error <- function(msg, expr, context) {
  query_error(msg, expr, context, "while evaluating")
}


query_parse_check_call <- function(expr, context) {
  if (is.null(expr)) {
    return("empty")
  }

  if (!is.call(expr)) {
    query_parse_error(sprintf(
      "Invalid query '%s'; expected some sort of expression",
      deparse_query(expr, NULL, NULL)),
      expr, context)
  }

  fn <- as.character(expr[[1]])

  if (fn %in% names(query_functions$group)) {
    type <- "group"
  } else if (fn %in% names(query_functions$test)) {
    type <- "test"
  } else if (fn %in% names(query_functions$subquery)) {
    type <- "subquery"
  } else if (fn %in% names(query_functions$dependency)) {
    type <- "dependency"
  } else { # fn is in names(query_functions$other)
    type <- "other"
  }
  len <- query_functions[[type]][[fn]]

  if (is.null(len)) {
    query_parse_error(sprintf(
      "Invalid query '%s'; unknown query component '%s'",
      deparse_query(expr, NULL, NULL), fn),
      expr, context)
  }

  nargs <- length(expr) - 1L
  if (length(len) == 1) {
    if (nargs != len) {
      query_parse_error(sprintf(
        "Invalid call to %s(); expected %d args but received %d",
        fn, len, nargs),
        expr, context)
    }
  } else {
    if (nargs < len[[1]]) {
      query_parse_error(sprintf(
        "Invalid call to %s(); expected at least %d args but received %d",
        fn, len[[1]], nargs),
        expr, context)
    }
    if (nargs > len[[2]]) {
      query_parse_error(sprintf(
        "Invalid call to %s(); expected at most %d args but received %d",
        fn, len[[2]], nargs),
        expr, context)
    }
  }

  if (type == "other") {
    type <- fn
  }

  type
}


query_parse_value <- function(expr, context, subquery_envir) {
  if (is.numeric(expr) || is.character(expr)) {
    list(type = "literal", value = expr)
  } else if (is_logical(expr)) {
    list(type = "literal", value = as_logical(expr))
  } else if (identical(expr, quote(name)) || identical(expr, quote(id))) {
    list(type = "lookup",
         name = deparse(expr))
  } else if (is_call(expr, ":")) {
    name <- deparse_query(expr[[2]], NULL, NULL)
    valid <- c("parameter", "this", "environment")
    if (!(name %in% valid)) {
        query_parse_error(sprintf(
          "Invalid lookup '%s'", name), expr, context)
    }
    list(type = "lookup",
         name = name,
         query = deparse_query(expr[[3]], NULL, NULL),
         expr = expr,
         context = context)
  } else {
    query_parse_error(
      sprintf("Unhandled query expression value '%s'",
              deparse_query(expr, NULL, NULL)),
      expr, context)
  }
}


## By looping through in order we'll prevent any circular dependencies
## between subqueries, though some work will possibly needed to make
## this obvious to the users - I think this is hard to accidentally
## trigger though.
make_subquery_envir <- function(subquery, call = NULL) {
  if (!is.null(subquery)) {
    assert_named(subquery, unique = TRUE, call = call)
  }
  subquery_envir <- new.env()
  for (nm in names(subquery)) {
    add_subquery(nm, subquery[[nm]], NULL, subquery_envir)
  }
  subquery_envir
}


add_subquery <- function(name, expr, context, subquery_envir) {
  anonymous <- is.null(name)
  if (anonymous) {
    name <- openssl::md5(deparse_query(expr, NULL, NULL))
  }
  subquery_envir[[name]] <- list(
    name = name,
    expr = expr,
    parsed = query_parse(expr, context, subquery_envir),
    anonymous = anonymous,
    evaluated = FALSE,
    result = NULL)
  invisible(name)
}


query_collect_lookup <- function(expr_parsed, subquery_envir) {
  envir <- new.env(parent = emptyenv())
  collect <- c("this", "environment")
  for (nm in collect) {
    envir[[nm]] <- character()
  }
  f <- function(x) {
    if (is.recursive(x) && x$type == "lookup" && x$name %in% collect) {
      envir[[x$name]] <- c(envir[[x$name]], x$query)
    } else if (x$type == "subquery") {
      sub <- subquery_envir[[x$args$name]]
      if (!is.null(sub)) {
        f(sub$parsed)
      }
    } else {
      for (el in x$args) {
        f(el)
      }
    }
  }
  f(expr_parsed)
  set_names(lapply(collect, function(nm) unique(envir[[nm]])), collect)
}
