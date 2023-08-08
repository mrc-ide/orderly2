test_that("Parse basic query", {
  res <- query_parse("latest(name == 'data')", NULL, emptyenv())
  expect_identical(query_parse(quote(latest(name == "data")), NULL, emptyenv()),
                   res)
  expect_equal(res$type, "latest")
  expect_length(res$args, 1)
  expect_equal(res$args[[1]]$type, "test")
  expect_equal(res$args[[1]]$name, "==")
  expect_length(res$args[[1]]$args, 2)
  expect_equal(res$args[[1]]$args[[1]], list(type = "lookup", name = "name"))
  expect_equal(res$args[[1]]$args[[2]], list(type = "literal", value = "data"))
})


test_that("empty query is possible", {
  expect_equal(
    query_parse(NULL, NULL, new.env(parent = emptyenv())),
    query_component("empty", expr = NULL, context = NULL, args = list()))
  expect_equal(
    orderly_query(NULL),
    structure(
      list(value = query_parse(NULL, NULL, new.env(parent = emptyenv())),
           subquery = list(),
           info = list(single = FALSE,
                       parameters = character(),
                       environment = character())),
      class = "orderly_query"))
})


test_that("Prevent unparseable queries", {
  expect_error(query_parse(1, NULL, emptyenv()),
               "Invalid input for query")
  expect_error(query_parse("latest(); latest()", NULL, emptyenv()),
               "Expected a single expression")
})


test_that("print context around parse errors", {
  err <- expect_error(
    query_parse(quote(a %in% b), NULL, emptyenv()),
    "Invalid query 'a %in% b'; unknown query component '%in%'",
    fixed = TRUE)
  expect_match(err$message, "  - in a %in% b", fixed = TRUE)

  err <- expect_error(
    query_parse(quote(latest(a %in% b)), NULL, emptyenv()),
    "Invalid query 'a %in% b'; unknown query component '%in%'",
    fixed = TRUE)
  expect_match(err$message, "  - in     a %in% b", fixed = TRUE)
  expect_match(err$message, "  - within latest(a %in% b)", fixed = TRUE)
})


test_that("Expressions must be calls", {
  expect_error(
    query_parse(quote(name), NULL, emptyenv()),
    "Invalid query 'name'; expected some sort of expression")
  expect_error(
    query_parse(quote(latest(name)), NULL, emptyenv()),
    "Invalid query 'name'; expected some sort of expression")
  expect_error(
    query_parse(quote(latest(parameter:x == 1 && name)), NULL, emptyenv()),
    "Invalid query 'name'; expected some sort of expression")
})


test_that("validate argument numbers", {
  ## Have to do a fiddle here, to fail the arg length check. The error
  ## message is a bit weird too, but it will be reasonable for
  ## anything else that has a fixed number of args.
  expect_error(
    query_parse(quote(`==`(a, b, c)), NULL, emptyenv()),
    "Invalid call to ==(); expected 2 args but received 3",
    fixed = TRUE)
  expect_error(
    query_parse(quote(latest(a, b)), NULL, emptyenv()),
    "Invalid call to latest(); expected at most 1 args but received 2",
    fixed = TRUE)
  expect_error(
    query_parse(quote(uses()), NULL, emptyenv()),
    "Invalid call to uses(); expected at least 1 args but received 0",
    fixed = TRUE)
})


test_that("dependency can take literal or expression", {
  res <- query_parse(quote(usedby(id == "123")), NULL, emptyenv())
  expect_equal(res$type, "dependency")
  expect_equal(res$name, "usedby")
  expect_length(res$args, 2)
  expect_equal(res$args[[1]]$type, "test")
  expect_equal(res$args[[2]]$type, "literal")
  expect_equal(res$args[[2]]$value, Inf)
  expect_equal(res$args[[2]]$name, "depth")

  res <- query_parse(quote(usedby("123")), NULL, emptyenv())
  expect_equal(res$type, "dependency")
  expect_equal(res$name, "usedby")
  expect_length(res$args, 2)
  expect_equal(res$args[[1]], list(type = "literal", value = "123"))

  res <- query_parse(quote(usedby(latest(name == "x"))),
                     NULL, emptyenv())
  expect_equal(res$type, "dependency")
  expect_equal(res$name, "usedby")
  expect_length(res$args, 2)
  expect_equal(res$args[[1]]$type, "latest")
})


test_that("usedby requires 2nd arg boolean", {
  expect_error(
    query_parse(quote(usedby(id == "123", "123")), NULL, emptyenv()),
    paste0("`depth` argument in 'usedby()' must be a positive numeric, set to ",
           "control the number of layers of parents to recurse through when ",
           "listing dependencies. Use `depth = Inf` to search entire ",
           "dependency tree.\n",
           '  - in usedby(id == "123", "123")'),
    fixed = TRUE)

  expect_error(
    query_parse(quote(usedby(id == "123", -2)), NULL, emptyenv()),
    paste0("`depth` argument in 'usedby()' must be a positive numeric, set to ",
           "control the number of layers of parents to recurse through when ",
           "listing dependencies. Use `depth = Inf` to search entire ",
           "dependency tree.\n",
           '  - in usedby(id == "123", -2'),
    fixed = TRUE)

  res <- query_parse(quote(usedby(id == "123", 2)), NULL, emptyenv())
  expect_equal(res$type, "dependency")
  expect_equal(res$name, "usedby")
  expect_length(res$args, 2)
  expect_equal(res$args[[2]]$type, "literal")
  expect_equal(res$args[[2]]$value, 2)
  expect_equal(res$args[[2]]$name, "depth")

  ## Depth can be infinite
  res <- query_parse(quote(usedby(id == "123", Inf)), NULL, emptyenv())
  expect_equal(res$type, "dependency")
  expect_equal(res$name, "usedby")
  expect_length(res$args, 2)
  expect_equal(res$args[[2]]$type, "literal")
  expect_equal(res$args[[2]]$value, Inf)
  expect_equal(res$args[[2]]$name, "depth")

  ## Depth arg returns Inf by default
  res_default <- query_parse(quote(usedby(id == "123")), NULL, emptyenv())
  expect_equal(res_default$args[[2]], res$args[[2]])

  ## Depth arg can be named
  res <- query_parse(quote(usedby(id == "123", depth = 3)), NULL, emptyenv())
  expect_equal(res$type, "dependency")
  expect_equal(res$name, "usedby")
  expect_length(res$args, 2)
  expect_equal(res$args[[1]]$type, "test")
  expect_equal(res$args[[2]]$type, "literal")
  expect_equal(res$args[[2]]$value, 3)
  expect_equal(res$args[[2]]$name, "depth")
})


test_that("dependency can be uses type", {
  res <- query_parse(quote(uses(id == "123")), NULL, emptyenv())
  expect_equal(res$type, "dependency")
  expect_equal(res$name, "uses")
  expect_length(res$args, 2)
  expect_equal(res$args[[1]]$type, "test")
  expect_equal(res$args[[2]]$type, "literal")
  expect_equal(res$args[[2]]$value, Inf)
  expect_equal(res$args[[2]]$name, "depth")
})


test_that("Queries can only be name and parameter", {
  res <- query_parse(quote(name == "data"), NULL, emptyenv())
  expect_equal(res$type, "test")
  expect_equal(res$name, "==")
  expect_equal(res$args,
               list(list(type = "lookup", name = "name"),
                    list(type = "literal", value = "data")))

  res <- query_parse(quote(parameter:x == 1), NULL, emptyenv())
  expect_equal(res$type, "test")
  expect_equal(res$name, "==")
  expect_equal(res$args,
               list(list(type = "lookup", name = "parameter", query = "x",
                         expr = quote(parameter:x),
                         context = quote(parameter:x == 1)),
                    list(type = "literal", value = 1)))
  expect_error(
    query_parse(quote(date >= "2022-02-04"), NULL, emptyenv()),
    "Unhandled query expression value 'date'")
  expect_error(
    query_parse(quote(custom:orderly:displayname >= "my name"), NULL,
                emptyenv()),
    "Invalid lookup 'custom:orderly'")
})


test_that("construct a query", {
  obj <- orderly_query("latest")
  expect_s3_class(obj, "orderly_query")
  ## TODO: single_value, parameters
  expect_setequal(names(obj), c("value", "info", "subquery"))
  expect_s3_class(obj$value, "orderly_query_component")
  expect_equal(obj$value, query_parse("latest", "latest", emptyenv()))
  expect_equal(obj$info, list(single = TRUE,
                              parameters = character(),
                              environment = character()))
  expect_equal(obj$subquery, list())
})


test_that("convert to a query", {
  expect_identical(as_orderly_query("latest"),
                   orderly_query("latest"))
  expect_identical(as_orderly_query("latest", name = "a"),
                   orderly_query("latest", name = "a"))
  expect_identical(as_orderly_query(orderly_query("latest", name = "a")),
                   orderly_query("latest", name = "a"))
  expect_error(
    as_orderly_query(orderly_query("latest"), name = "a"),
    "If 'expr' is an 'orderly_query', no additional arguments allowed")
})


test_that("report on parameters used in the query", {
  f <- function(x) {
    orderly_query(x)$info$parameters
  }
  expect_equal(f(quote(latest())), character())
  expect_equal(f(quote(parameter:x < this:a)), "a")
  expect_equal(f(quote(parameter:x < this:a && this:a > this:b)),
               c("a", "b"))
})

test_that("report on environment variables used in the query", {
  f <- function(x) {
    orderly_query(x)$info$environment
 }
  expect_equal(f(quote(latest())), character())
  expect_equal(f(quote(parameter:x < environment:a)), "a")
  expect_equal(
    f(quote(parameter:x < environment:a && environment:a > environment:b)),
    c("a", "b"))
})


test_that("include parameters from subqueries too", {
  obj <- orderly_query("latest({B})",
                       subquery = list(B = quote(parameter:x < this:y)))
  expect_equal(obj$info$parameters, "y")
})


test_that("validate inputs to orderly_query", {
  expect_error(
    orderly_query("latest", list(a = 1)),
    "'name' must be character")
})


test_that("can construct query without expression", {
  res <- orderly_query(NULL)
  expect_equal(format(res), "NULL")

  res <- orderly_query(NULL, name = "foo")
  expect_equal(format(res), 'name == "foo"')
})
