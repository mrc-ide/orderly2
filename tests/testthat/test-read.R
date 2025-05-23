test_that("can parse file with no helpers", {
  expect_equal(orderly_parse_file("examples/implicit/implicit.R"),
               list(entrypoint_filename = "implicit.R",
                    strict = list(enabled = FALSE)))
})


test_that("can parse file with helpers", {
  dat <- orderly_parse_file("examples/explicit/explicit.R")
  expect_setequal(names(dat),
                  c("entrypoint_filename", "strict", "resources", "artefacts"))
  expect_equal(dat$strict, list(enabled = FALSE))
  expect_equal(dat$resources, "data.csv")
  expect_equal(dat$artefacts,
               list(list(description = "A graph of things",
                         files = "mygraph.png")))
})


test_that("can parse file from expression", {
  exprs <- parse(file = "examples/explicit/explicit.R")
  dat <- orderly_parse_expr(exprs = exprs, filename = "explicit.R")
  expect_equal(dat, orderly_parse_file("examples/explicit/explicit.R"))
})


test_that("Skip over computed resources", {
  dat <- orderly_parse_file("examples/computed-resource/computed-resource.R")
  expect_null(dat$resources)
})


test_that("Can read string vector literals from expressions", {
  expect_equal(static_character_vector(quote("x"), FALSE), "x")
  expect_equal(static_character_vector(quote(c("x")), FALSE), "x")
  expect_equal(static_character_vector(quote(c("x", "y")), FALSE), c("x", "y"))
  expect_equal(static_character_vector(quote(c("x", c("y", "z"))), FALSE),
               c("x", "y", "z"))

  expect_equal(static_character_vector(quote(c(a = "x")), FALSE),
               "x")
  expect_equal(static_character_vector(quote(c(a = "x")), TRUE),
               c(a = "x"))

  expect_null(static_character_vector(quote(a), FALSE))
  expect_null(static_character_vector(quote(c(a)), FALSE))
  expect_null(static_character_vector(quote(c(a, "x")), FALSE))
  expect_null(static_character_vector(quote(c(a, b)), FALSE))
  expect_null(static_character_vector(quote(c("x", c(a, b))), FALSE))
  expect_null(static_character_vector(quote(c("x", c("y", b))), FALSE))
  expect_null(static_character_vector(quote(c(a, c("x", "y"))), FALSE))
})


test_that("Can read string from expressions", {
  expect_equal(static_string(quote("x")), "x")
  expect_equal(static_string(quote(c("x"))), "x")

  expect_null(static_string(quote(a)))
  expect_null(static_string(quote(c(a))))
})


test_that("read dependency", {
  args <- list(name = "a", query = "latest", files = c(x = "y"))
  expect_equal(static_orderly_dependency(args), args)

  args <- list(name = NULL, query = "latest", files = c(x = "y"))
  expect_equal(static_orderly_dependency(args), args)

  args <- list(name = NULL, query = "latest", files = c("x"))
  expect_equal(static_orderly_dependency(args)$files, c(x = "x"))

  args <- list(name = NULL, query = "latest", files = c("x", y = "z"))
  expect_equal(static_orderly_dependency(args)$files, c(x = "x", y = "z"))

  expect_null(
    static_orderly_dependency(list(name = quote(a),
                                   query = "latest",
                                   files = c(x = "y"))))
  expect_null(
    static_orderly_dependency(list(name = "a",
                                   query = quote(latest),
                                   files = c(x = "y"))))
  expect_null(
    static_orderly_dependency(list(name = "a",
                                   query = "latest",
                                   files = quote(files))))
})


test_that("can parse expressions that might be interesting", {
  nms <- c("orderly_a", "orderly_b")

  expect_equal(
    orderly_read_expr(quote(orderly2::orderly_a(x, y)), nms),
    list(is_orderly = TRUE,
         name = "orderly_a",
         expr = quote(orderly2::orderly_a(x, y))))
  expect_equal(
    orderly_read_expr(quote(z <- orderly2::orderly_a(x, y)), nms),
    list(is_orderly = TRUE,
         name = "orderly_a",
         expr = quote(orderly2::orderly_a(x, y)),
         assigned_to = "z"))
  ## Can't use quote(a = expr) here or we fail on matching
  expect_equal(
    orderly_read_expr(parse(text = "z = orderly2::orderly_a(x, y)")[[1]], nms),
    list(is_orderly = TRUE,
         name = "orderly_a",
         expr = quote(orderly2::orderly_a(x, y)),
         assigned_to = "z"))

  expect_equal(
    orderly_read_expr(quote(orderly_a(x, y)), nms),
    list(is_orderly = TRUE,
         name = "orderly_a",
         expr = quote(orderly_a(x, y))))
  expect_equal(
    orderly_read_expr(quote(z <- orderly_a(x, y)), nms),
    list(is_orderly = TRUE,
         name = "orderly_a",
         expr = quote(orderly_a(x, y)),
         assigned_to = "z"))

  expect_equal(
    orderly_read_expr(quote(orderly2::orderly_c(x, y)), nms),
    list(is_orderly = FALSE,
         expr = quote(orderly2::orderly_c(x, y))))
  expect_equal(
    orderly_read_expr(quote(f(a, b, c)), nms),
    list(is_orderly = FALSE,
         expr = quote(f(a, b, c))))
})

test_that("read shared resource", {
  expect_equal(static_orderly_shared_resource(list("a", "b")),
               c(a = "a", b = "b"))

  expect_equal(static_orderly_shared_resource(list(a = "x", "b")),
               c(a = "x", b = "b"))

  expect_null(static_orderly_shared_resource(list(quote(c("a", "b")))))
})


test_that("cope with extra arguments", {
  tmp <- withr::local_tempfile()
  writeLines('orderly_artefact("a", "b", "c", "d")', tmp)
  e <- expect_error(orderly_parse_file(tmp),
                    "Failed to parse call to 'orderly_artefact()'",
                    fixed = TRUE)
  skip_on_cran()
  msg <- conditionMessage(e)
  expect_no_match(msg, "Check for a trailing comma")
})


test_that("cope with extra empty arguments", {
  tmp <- withr::local_tempfile()
  writeLines('orderly_artefact("a", "b", )', tmp)
  e <- expect_error(orderly_parse_file(tmp),
                    "Failed to parse call to 'orderly_artefact()'",
                    fixed = TRUE)
  skip_on_cran()
  msg <- conditionMessage(e)
  expect_match(msg, "Check for a trailing comma")
})
