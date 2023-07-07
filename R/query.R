##' Construct an outpack query, typically then passed through to
##' [outpack_search]
##'
##' @title Construct outpack query
##'
##' @param expr The query expression
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
##' @return An `outpack_query` object, which should not be modified,
##'   but which can be passed to [orderly2::outpack_search()]
##'
##' @export
outpack_query <- function(expr, name = NULL, scope = NULL, subquery = NULL) {
  subquery_env <- make_subquery_env(subquery)
  expr_parsed <- query_parse(expr, expr, subquery_env)
  if (!is.null(name)) {
    assert_scalar_character(name)
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

  info <- list(
    single = is_expr_single_value(expr_parsed, subquery_env),
    parameters = query_parameters(expr_parsed, subquery_env))

  ret <- list(value = expr_parsed,
              subquery = as.list(subquery_env),
              info = info)
  class(ret) <- "outpack_query"
  ret
}


as_outpack_query <- function(expr, ...) {
  if (inherits(expr, "outpack_query")) {
    if (...length() > 0) {
      stop("If 'expr' is an 'outpack_query', no additional arguments allowed")
    }
    expr
  } else {
    outpack_query(expr, ...)
  }
}


query_parse <- function(expr, context, subquery_env) {
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
  } else if (!is.language(expr)) {
    stop("Invalid input for query")
  }

  ## This is used extensively in orderly, so we'll support it here
  if (identical(expr, quote(latest))) {
    expr <- quote(latest())
  }

  if (is.null(context)) {
    context <- expr
  }

  query_parse_expr(expr, context, subquery_env)
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
    class = "outpack_query_component")
}


query_parse_expr <- function(expr, context, subquery_env) {
  type <- query_parse_check_call(expr, context)
  fn <- switch(type,
               test = query_parse_test,
               group = query_parse_group,
               latest = query_parse_latest,
               single = query_parse_single,
               subquery = query_parse_subquery,
               dependency = query_parse_dependency,
               ## normally unreachable
               stop("Unhandled expression [outpack bug - please report]"))
  fn(expr, context, subquery_env)
}


query_parse_test <- function(expr, context, subquery_env) {
  args <- lapply(expr[-1], query_parse_value, context)
  name <- deparse(expr[[1]])
  query_component("test", expr, context, args, name = name)
}


query_parse_group <- function(expr, context, subquery_env) {
  args <- lapply(expr[-1], query_parse_expr, context, subquery_env)
  name <- deparse(expr[[1]])
  query_component("group", expr, context, args, name = name)
}


query_parse_latest <- function(expr, context, subquery_env) {
  args <- lapply(expr[-1], query_parse_expr, context, subquery_env)
  query_component("latest", expr, context, args)
}


query_parse_single <- function(expr, context, subquery_env) {
  args <- lapply(expr[-1], query_parse_expr, context, subquery_env)
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

  if (expr_parsed$type %in% scoped_functions) {
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


query_parse_subquery <- function(expr, context, subquery_env) {
  subquery <- expr[-1]
  if (is_named_subquery(subquery)) {
    query_name <- deparse(subquery[[1]])
    if (is.null(subquery_env[[query_name]])) {
      named_subqueries <-
        names(which(vlapply(as.list(subquery_env), function(x) !x$anonymous)))
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
    query_name <- add_subquery(NULL, subquery[[1]], context, subquery_env)
  }
  query_component("subquery", expr, context, args = list(name = query_name))
}


query_parse_dependency <- function(expr, context, subquery_env) {
  name <- deparse(expr[[1]])
  args <- as.list(expr[-1])
  if (length(args) == 2) {
    if (is.numeric(args[[2]]) && args[[2]] > 0) {
      args[[2]] <- query_parse_value(args[[2]], context, subquery_env)
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
    args[[2]] <- query_parse_value(Inf, context, subquery_env)
  }
  args[[2]]$name <- names(args[2]) %||% "depth"
  if (is.call(args[[1]])) {
    args[[1]] <- query_parse_expr(args[[1]], context, subquery_env)
    if (!is_expr_single_value(args[[1]], subquery_env)) {
      query_parse_error(
        sprintf(paste(
          "%s must be called on an expression guaranteed to return",
          "a single ID. Try wrapping expression in `latest` or `single`."),
          name),
        expr, context)
    }
  } else {
    args[[1]] <- query_parse_value(args[[1]], context, subquery_env)
  }
  query_component("dependency", expr, context, args, name = name)
}

## Check if a query component returns a single value
## Guaranteed single valued if one of the following is true
##   * it is function call to latest
##   * it is a function call to single
##   * it is an ID lookup
##   * it is a subquery whose expression is validates one of these conditions
is_expr_single_value <- function(parsed_expr, subquery_env) {
  if (parsed_expr$type == "subquery") {
    parsed_expr_sub <- subquery_env[[parsed_expr$args$name]]$parsed
    return(is_expr_single_value(parsed_expr_sub, subquery_env))
  }
  parsed_expr$type %in% c("latest", "single") ||
    (parsed_expr$type == "test" && (is_id_lookup(parsed_expr$args[[1]]) ||
                                    is_id_lookup(parsed_expr$args[[2]])))
}

is_id_lookup <- function(expr) {
  expr$type == "lookup" && expr$name == "id"
}


query_error <- function(msg, expr, context, prefix) {
  if (identical(expr, context)) {
    stop(sprintf("%s\n  - %s %s", msg, prefix, deparse_query(expr, NULL)),
         call. = FALSE)
  } else {
    width <- max(nchar(prefix), nchar("within"))
    stop(sprintf(
      "%s\n  - %s %s\n  - %s %s",
      msg,
      format(prefix, width = width), deparse_query(expr, NULL),
      format("within", width = width), deparse_query(context, NULL)),
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
  if (!is.call(expr)) {
    query_parse_error(sprintf(
      "Invalid query '%s'; expected some sort of expression",
      deparse_query(expr, NULL)),
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
      deparse_query(expr, NULL), fn),
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


query_parse_value <- function(expr, context, subquery_env) {
  if (is.numeric(expr) || is.character(expr) || is.logical(expr)) {
    list(type = "literal", value = expr)
  } else if (identical(expr, quote(name)) || identical(expr, quote(id))) {
    list(type = "lookup",
         name = deparse(expr))
  } else if (is_call(expr, ":")) {
    name <- deparse_query(expr[[2]], NULL)
    valid <- c("parameter", "this")
    if (!(name %in% valid)) {
        query_parse_error(sprintf(
          "Invalid lookup '%s'", name), expr, context)
    }
    list(type = "lookup",
         name = name,
         query = deparse_query(expr[[3]], NULL),
         expr = expr,
         context = context)
  } else {
    query_parse_error(
      sprintf("Unhandled query expression value '%s'",
              deparse_query(expr, NULL)),
      expr, context)
  }
}


## By looping through in order we'll prevent any circular dependencies
## between subqueries, though some work will possibly needed to make
## this obvious to the users - I think this is hard to accidentally
## trigger though.
make_subquery_env <- function(subquery) {
  if (!is.null(subquery)) {
    assert_named(subquery, unique = TRUE)
  }
  subquery_env <- new.env()
  for (nm in names(subquery)) {
    add_subquery(nm, subquery[[nm]], NULL, subquery_env)
  }
  subquery_env
}


add_subquery <- function(name, expr, context, subquery_env) {
  anonymous <- is.null(name)
  if (anonymous) {
    name <- openssl::md5(deparse_query(expr, NULL))
  }
  subquery_env[[name]] <- list(
    name = name,
    expr = expr,
    parsed = query_parse(expr, context, subquery_env),
    anonymous = anonymous,
    evaluated = FALSE,
    result = NULL)
  invisible(name)
}


query_parameters <- function(expr_parsed, subquery_env) {
  env <- new.env(parent = emptyenv())
  env$seen <- character()
  f <- function(x) {
    if (is.recursive(x) && x$type == "lookup" && x$name == "this") {
      env$seen <- c(env$seen, x$query)
    } else if (x$type == "subquery") {
      sub <- subquery_env[[x$args$name]]
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
  unique(env$seen)
}
