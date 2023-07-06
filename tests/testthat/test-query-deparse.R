test_that("queries can be deparsed", {
  expect_equal(outpack_query_format(quote(x)), "x")
  expect_equal(outpack_query_format(quote("x")), '"x"')
  expect_equal(outpack_query_format(quote('x')), '"x"') # nolint
  expect_equal(outpack_query_format(quote(2)), "2")
  expect_equal(outpack_query_format(quote(TRUE)), "TRUE")
  expect_equal(outpack_query_format(quote(name == "name")), 'name == "name"')
  expect_equal(outpack_query_format(quote(id == "123")), 'id == "123"')
  expect_equal(outpack_query_format(quote(parameter:a == 2)),
               "parameter:a == 2")
  expect_equal(outpack_query_format(quote(latest)), "latest")
  expect_equal(outpack_query_format(quote(latest())), "latest()")
  expect_equal(outpack_query_format(quote(latest(a %in% b))),
               "latest(a %in% b)")
  expect_equal(
    outpack_query_format(quote(latest(parameter:x == 1 && name == "name"))),
    'latest(parameter:x == 1 && name == "name")')
  expect_equal(outpack_query_format(quote(latest(parameter:x == this:x))),
               "latest(parameter:x == this:x)")
  expect_equal(outpack_query_format(quote(latest({subquery}))), # nolint
               "latest({subquery})")
  expect_equal(outpack_query_format(quote(latest(   ))), "latest()") # nolint
  expect_equal(outpack_query_format(quote(name    ==     "name")),
               'name == "name"')
  expect_equal(
    outpack_query_format(quote(name == "other" && !(name == "data"))),
    'name == "other" && !(name == "data")')
  expect_equal(outpack_query_format(quote(x <- 23)), "x <- 23")
  expect_equal(outpack_query_format(quote(x[2])), "x[2]")
  expect_equal(outpack_query_format(quote(x[2, 3])), "x[2, 3]")
  expect_equal(outpack_query_format(quote(!x)), "!x")
  expect_equal(outpack_query_format(quote(!(x))), "!(x)")
  expect_equal(outpack_query_format(quote(!(x || y))), "!(x || y)")
  expect_equal(outpack_query_format(quote(usedby("thing", -2))),
               'usedby("thing", -2)')

  expect_error(
    outpack_query_format(c("one", "two")),
    "Cannot format query, it must be a language object or be length 1.")
})


test_that("subqueries can be interpolated into deparsed queries", {
  subquery <- list(A = "123",
                   B = quote(latest(name == "b")),
                   C = quote(latest(usedby({A})))) # nolint
  expect_equal(
    outpack_query_format(quote(latest(usedby({A}))), subquery), # nolint
    'latest(usedby({"123"}))')
  expect_equal(
    outpack_query_format(quote(latest(usedby({B}))), subquery), # nolint
    'latest(usedby({latest(name == "b")}))')
  expect_equal(
    outpack_query_format(quote(latest(usedby({Z}))), subquery), # nolint
    "latest(usedby({Z}))")
  ## Does recurse, for better or worse:
  expect_equal(
    outpack_query_format(quote(latest(usedby({C}))), subquery), # nolint
    'latest(usedby({latest(usedby({"123"}))}))')
})


test_that("subqueries must be sensible", {
  expect_error(
    outpack_query_format(quote(latest(usedby({A}))), list(A = list())), # nolint
    "Invalid subquery, it must be deparseable: error for 'A'")
})


test_that("format S3 method dispatches", {
  expect_equal(format(outpack_query("latest", "foo")),
               'latest(name == "foo")')
  expect_equal(
    format(outpack_query("latest(usedby({X}))",
                         name = "analysis",
                         subquery = list(X = 'latest(name == "data")'))),
    'latest(usedby({"latest(name == "data")"}) && name == "analysis")')
})
