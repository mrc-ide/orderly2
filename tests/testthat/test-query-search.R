test_that("can construct search options", {
  defaults <- orderly_search_options()
  expect_s3_class(defaults, "orderly_search_options")
  expect_mapequal(
    unclass(defaults),
    list(location = NULL,
         allow_remote = FALSE,
         pull_metadata = FALSE))

  opts <- orderly_search_options(location = c("x", "y"),
                                 allow_remote = TRUE,
                                 pull_metadata = TRUE)
  expect_s3_class(opts, "orderly_search_options")
  expect_mapequal(
    unclass(opts),
    list(location = c("x", "y"),
         allow_remote = TRUE,
         pull_metadata = TRUE))
})


test_that("can convert into search options", {
  opts <- orderly_search_options(location = "x",
                                 allow_remote = FALSE,
                                 pull_metadata = FALSE)
  expect_equal(as_orderly_search_options(NULL),
               orderly_search_options())
  expect_equal(as_orderly_search_options(list(location = "x")),
               modifyList(orderly_search_options(), list(location = "x")))
  expect_equal(as_orderly_search_options(unclass(opts)),
               opts)
  expect_equal(as_orderly_search_options(NULL, list(allow_remote = TRUE)),
               orderly_search_options(allow_remote = TRUE))
  expect_equal(as_orderly_search_options(list(location = "a"),
                                         list(allow_remote = TRUE)),
               orderly_search_options(location = "a", allow_remote = TRUE))
  expect_equal(as_orderly_search_options(list(allow_remote = FALSE,
                                              location = "a"),
                                         list(allow_remote = TRUE)),
               orderly_search_options(allow_remote = FALSE, location = "a"))
})


test_that("validate inputs to outpack search options", {
  expect_error(
    as_orderly_search_options(c(allow_remote = FALSE)),
    "Expected 'options' to be an 'orderly_search_options' or a list of options")
  expect_error(
    as_orderly_search_options(list(allow_remote = FALSE, other = FALSE)),
    "Invalid option passed to 'orderly_search_options': 'other'")
})


test_that("Can run very basic queries", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- vcapply(1:3, function(i) create_random_packet(root))
  expect_equal(
    orderly_search(quote(latest), root = root),
    last(ids))
  expect_equal(
    orderly_search(quote(latest()), root = root),
    last(ids))
  expect_equal(
    orderly_search(bquote(id == .(ids[[1]])), root = root),
    ids[[1]])
  expect_equal(
    orderly_search(quote(name == "data"), root = root),
    ids)
  expect_equal(
    orderly_search(quote(latest(name == "data")), root = root),
    last(ids))
  expect_equal(
    orderly_search(quote(name == "other"), root = root),
    character(0))
  expect_equal(
    orderly_search(quote(latest(name == "other")), root = root),
    NA_character_)
  expect_equal(
    orderly_search(quote(name == "other" || name == "data"), root = root),
    ids)
  expect_equal(
    orderly_search(quote(name == "other" && name == "data"), root = root),
    character(0))
  expect_equal(
    orderly_search(quote(!(name == "other") && name == "data"), root = root),
    ids)
  expect_equal(
    orderly_search(bquote(latest(id == .(ids[[1]]) || id == .(ids[[2]]))),
      root = root),
    ids[[2]])
})


test_that("Scope queries", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- vcapply(1:3, function(i) create_random_packet(root, "x", list(a = 1)))
  x2 <- vcapply(1:3, function(i) create_random_packet(root, "x", list(a = 2)))
  y1 <- vcapply(1:3, function(i) create_random_packet(root, "y", list(a = 1)))
  y2 <- vcapply(1:3, function(i) create_random_packet(root, "y", list(a = 2)))

  expect_equal(
    orderly_search(quote(parameter:a == 1), root = root),
    c(x1, y1))
  expect_equal(
    orderly_search(quote(parameter:a == 1), scope = quote(name == "x"),
                   root = root),
    x1)
})


test_that("Can filter based on given values", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- vcapply(1:3, function(i) create_random_packet(root, "x", list(a = 1)))
  x2 <- vcapply(1:3, function(i) create_random_packet(root, "x", list(a = 2)))

  expect_equal(
    orderly_search(quote(latest(parameter:a == this:a)),
                  parameters = list(a = 1), root = root),
    last(x1))
  expect_equal(
    orderly_search(quote(latest(parameter:a == this:a)),
                  parameters = list(a = 2), root = root),
    last(x2))
  expect_equal(
    orderly_search(quote(latest(parameter:a == this:a)),
                  parameters = list(a = 3), root = root),
    NA_character_)
  expect_error(
    orderly_search(quote(latest(parameter:a == this:x)),
                  parameters = list(a = 3), root = root),
    paste0("Did not find 'x' within given parameters (containing 'a')\n",
           "  - while evaluating this:x\n",
           "  - within           latest(parameter:a == this:x)"),
    fixed = TRUE)
  envir <- list2env(list(a = sum), parent = emptyenv())
  expect_error(
    orderly_search(quote(latest(parameter:a == environment:x)),
                  envir = envir, root = root),
    paste0("Did not find 'x' within given environment (containing 'a')\n",
           "  - while evaluating environment:x\n",
           "  - within           latest(parameter:a == environment:x)"),
    fixed = TRUE)
  expect_error(
    orderly_search(quote(latest(parameter:a == environment:a)),
                  envir = envir, root = root),
    paste0("The value of 'a' from environment is not suitable as a lookup\n",
           "  - while evaluating environment:a\n",
           "  - within           latest(parameter:a == environment:a)"),
    fixed = TRUE)
})


test_that("can use variables from the environment when searching", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- vcapply(1:3, function(i) create_random_packet(root, "x", list(a = 1)))
  x2 <- vcapply(1:3, function(i) create_random_packet(root, "x", list(a = 2)))

  envir <- new.env()
  envir$x <- 1
  expect_equal(
    orderly_search(quote(latest(parameter:a == environment:x)),
                   envir = envir, root = root),
    x1[[3]])

  expect_error(
    orderly_search(quote(latest(parameter:a == environment:other)),
                   envir = envir, root = root),
    "Did not find 'other' within given environment (containing 'x')",
    fixed = TRUE)
})


test_that("single requires exactly one packet", {
  root <- create_temporary_root(use_file_store = TRUE)

  ids <- vcapply(1:3, function(i) create_random_packet(root, "x", list(a = i)))
  expect_equal(orderly_search(quote(single(parameter:a == 2)), root = root),
               ids[[2]])
  expect_error(orderly_search(quote(single(parameter:a >= 2)), root = root),
               "Query found 2 packets, but expected exactly one")
  expect_error(orderly_search(quote(single(parameter:a > 10)), root = root),
               "Query did not find any packets")
})


test_that("switch statements will prevent regressions", {
  skip_if_not_installed("mockery")
  mockery::stub(query_parse_expr, "query_parse_check_call",
                mockery::mock("other"))
  expr <- quote(some_function())
  expect_error(
    query_parse_expr(expr, expr),
    "Unhandled expression [outpack bug - please report]",
    fixed = TRUE)

  expect_error(
    query_eval(list(type = "other")),
    "Unhandled expression [outpack bug - please report]",
    fixed = TRUE)
  expect_error(
    query_eval_lookup(list(name = "custom:orderly:displayname"),
                      new.env(parent = emptyenv())),
    "Unhandled lookup [outpack bug - please report]",
    fixed = TRUE)
  expect_error(
    query_eval_group(list(name = "operator")),
    "Unhandled operator [outpack bug - please report]",
    fixed = TRUE)
})


test_that("Can filter query to packets that are locally available (unpacked)", {
  root <- list()
  root$a <- create_temporary_root(use_file_store = TRUE)
  for (name in c("x", "y", "z")) {
    root[[name]] <- create_temporary_root(use_file_store = TRUE)
    orderly_location_add(name, "path", list(path = root[[name]]$path),
                         root = root$a)
  }

  ids <- list()
  for (name in c("x", "y", "z")) {
    ids[[name]] <- vcapply(1:3, function(i) {
      create_random_packet(root[[name]], "data", list(p = i))
    })
  }
  orderly_location_pull_metadata(root = root$a)

  options_local <- orderly_search_options(location = c("x", "y"),
                                          allow_remote = FALSE)
  options_remote <- orderly_search_options(location = c("x", "y"),
                                          allow_remote = TRUE)

  expect_equal(
    orderly_search(quote(name == "data"), options = options_remote,
                   root = root$a),
    c(ids$x, ids$y))
  expect_equal(
    orderly_search(quote(name == "data"), options = options_local,
                   root = root$a),
    character())

  for (i in ids$x) {
    suppressMessages(orderly_location_pull_packet(i, root = root$a))
  }

  expect_equal(
    orderly_search(quote(name == "data"), options = options_remote,
                   root = root$a),
    c(ids$x, ids$y))
  expect_equal(
    orderly_search(quote(name == "data"), options = options_local,
                   root = root$a),
    ids$x)
})


test_that("scope and allow_local can be used together to filter query", {
  root <- list()
  for (name in c("src", "dst")) {
    root[[name]] <- create_temporary_root(use_file_store = TRUE)
  }
  orderly_location_add("src", "path", list(path = root$src$path),
                       root = root$dst)

  x1 <- create_random_packet(root$src, "x", list(p = 1))
  x2 <- create_random_packet(root$src, "x", list(p = 1))
  y1 <- create_random_packet(root$src, "y", list(p = 1))
  y2 <- create_random_packet(root$src, "y", list(p = 1))
  orderly_location_pull_metadata(root = root$dst)

  options_local <- orderly_search_options(allow_remote = FALSE)
  options_remote <- orderly_search_options(allow_remote = TRUE)

  expect_equal(
    orderly_search(quote(latest(parameter:p == 1)), options = options_remote,
                  scope = quote(name == "x"),
                  root = root$dst),
    x2)
  expect_equal(
    orderly_search(quote(latest(parameter:p == 1)), options = options_local,
                  scope = quote(name == "x"),
                  root = root$dst),
    NA_character_)

  for (i in c(x1, y1)) {
    suppressMessages(orderly_location_pull_packet(i, root = root$dst))
  }

  expect_equal(
    orderly_search(quote(latest(parameter:p == 1)), options = options_remote,
                  scope = quote(name == "x"),
                  root = root$dst),
    x2)
  expect_equal(
    orderly_search(quote(latest(parameter:p == 1)), options = options_local,
                  scope = quote(name == "x"),
                  root = root$dst),
    x1)
})


test_that("Parse literal id query", {
  id <- "20220722-085951-148b7686"
  res <- query_parse(id, NULL, emptyenv())
  expect_identical(query_parse(bquote(single(id == .(id))), NULL, emptyenv()),
                   res)
  expect_equal(res$type, "single")
  expect_length(res$args, 1)
  expect_equal(res$args[[1]]$type, "test")
  expect_equal(res$args[[1]]$name, "==")
  expect_length(res$args[[1]]$args, 2)
  expect_equal(res$args[[1]]$args[[1]], list(type = "lookup", name = "id"))
  expect_equal(res$args[[1]]$args[[2]], list(type = "literal", value = id))
})


test_that("orderly_search allows ids", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- vcapply(1:3, function(i) create_random_packet(root))
  expect_identical(orderly_search(ids[[1]], root = root), ids[[1]])
  expect_identical(orderly_search(ids[[2]], root = root), ids[[2]])
  expect_error(
    orderly_search("20220722-085951-148b7686", root = root),
    "Query did not find any packets")
})


test_that("correct behaviour with empty queries", {
  root <- create_temporary_root(use_file_store = TRUE)
  expect_equal(orderly_search("latest", root = root), NA_character_)
  expect_equal(orderly_search(quote(name == "data"), root = root),
               character(0))
})

test_that("named queries", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- create_random_packet(root, "x", list(a = 1))
  x2 <- create_random_packet(root, "x", list(a = 2))
  y1 <- create_random_packet(root, "y", list(a = 1))
  y2 <- create_random_packet(root, "y", list(a = 2))

  expect_equal(
    orderly_search(quote(latest()), name = "x", root = root),
    x2)
  expect_equal(
    orderly_search(quote(latest()), scope = quote(parameter:a == 1),
                  name = "x", root = root),
    x1)
})


test_that("orderly_search can include subqueries", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- create_random_packet(root, "x", list(a = 1))
  x2 <- create_random_packet(root, "x", list(a = 2))
  y1 <- create_random_packet(root, "y", list(a = 1))
  y2 <- create_random_packet(root, "y", list(a = 2))

  expect_equal(
    orderly_search(quote(latest({sub})), # nolint
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    x2)
  expect_equal(
    orderly_search(
      quote({sub}), # nolint
      subquery = list(sub = quote(latest(name == "x"))),
      root = root),
    x2)
})


test_that("orderly_search returns useful error when subquery name unknown", {
  root <- create_temporary_root()

  expect_error(
    orderly_search(quote(latest({sub})), # nolint
                  root = root),
    paste0("Cannot locate subquery named 'sub'. No named subqueries ",
           "provided.\n",
           "  - in     {sub}\n",
           "  - within latest({sub})"),
    fixed = TRUE)

  expect_error(
    orderly_search(quote(latest({subq})), # nolint
                  subquery = list(sub = quote(name == "x"),
                                  foo = quote(name == "y")),
                  root = root),
    paste0("Cannot locate subquery named 'subq'. ",
           "Available subqueries are 'foo', 'sub'.\n",
           "  - in     {subq}\n",
           "  - within latest({subq})"),
    fixed = TRUE)

  ## Anonymous subqueries are not included in list
  expect_error(
    orderly_search(quote(latest({name == "x"} && {sub})), # nolint
                  root = root),
    "Cannot locate subquery named 'sub'. No named subqueries provided.")
})


test_that("orderly_search returns no results when subquery has no results", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- create_random_packet(root, "x", list(a = 1))

  ## subquery itself has no results
  expect_equal(orderly_search(quote(latest(name == "y")), root = root),
               NA_character_)

  expect_equal(
    orderly_search(quote(latest({sub})), # nolint
                  subquery = list(sub = quote(name == "y")),
                  root = root),
    NA_character_)
})


test_that("subqueries cannot be used in tests e.g. ==, <, >= etc.", {
  root <- create_temporary_root(use_file_store = TRUE)

  expect_error(
    orderly_search(quote({sub} > 2), # nolint
                  subquery = list(sub = quote(parameter:a == 2)),
                  root = root),
    paste0("Unhandled query expression value '{sub}'\n",
           "  - in     {sub}\n",
           "  - within {sub} > 2"),
    fixed = TRUE)

  expect_error(
    orderly_search(quote(latest({sub}) > 2), # nolint
                  subquery = list(sub = quote(parameter:a == 2)),
                  root = root),
    paste0("Unhandled query expression value 'latest({sub})'\n",
           "  - in     latest({sub})\n",
           "  - within latest({sub}) > 2"),
    fixed = TRUE)

  expect_error(
    orderly_search(quote(latest({sub} == "hello")), # nolint
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    paste0("Unhandled query expression value '{sub}'\n",
           "  - in     {sub}\n",
           '  - within latest({sub} == "hello")'),
    fixed = TRUE)
})


test_that("subqueries can be used in groups e.g. &&, ||, (), etc.", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- create_random_packet(root, "x", list(a = 1))
  x2 <- create_random_packet(root, "x", list(a = 2))
  y1 <- create_random_packet(root, "y", list(a = 2))

  expect_setequal(
    orderly_search(quote({sub} || parameter:a == 2), # nolint
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    c(x1, x2, y1))

  expect_setequal(
    orderly_search(quote(!{sub}), # nolint
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    y1)

  expect_setequal(
    orderly_search(quote(parameter:a == 1 && {sub} || name == "y"), # nolint
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    c(x1, y1))
  expect_setequal(
    orderly_search(quote(parameter:a == 1 && ({sub} || name == "y")), # nolint
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    x1)
})


test_that("subqueries can be used within single", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- create_random_packet(root, "x", list(a = 1))
  x2 <- create_random_packet(root, "x", list(a = 2))
  y1 <- create_random_packet(root, "y", list(a = 2))

  expect_error(
    orderly_search(quote(single({sub})), # nolint
                  subquery = list(sub = quote(name == "x")),
                  root = root),
    paste0("Query found 2 packets, but expected exactly one\n",
           "  - while evaluating single({sub})"),
    fixed = TRUE)

  expect_equal(
    orderly_search(quote(single({sub})), # nolint
                  subquery = list(sub = quote(name == "y")),
                  root = root),
    y1)
})


test_that("orderly_search can include anonymous subqueries", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- create_random_packet(root, "x", list(a = 1))
  x2 <- create_random_packet(root, "x", list(a = 2))
  y1 <- create_random_packet(root, "y", list(a = 1))
  y2 <- create_random_packet(root, "y", list(a = 2))

  expect_equal(
    orderly_search(quote(latest({name == "x"})), # nolint
                  root = root),
    x2)
})


test_that("anonymous subquery is printed nicely when it errors", {
  root <- create_temporary_root()

  x1 <- create_random_packet(root, "x", list(a = 1))

  expect_error(
    orderly_search(quote(latest({ single() })), # nolint
                  root = root),
    paste0("Invalid call to single(); ",
           "expected 1 args but received 0\n",
           "  - in     single()\n",
           "  - within latest({single()})"),
    fixed = TRUE)
})


test_that("subqueries respect scope", {
  root <- create_temporary_root(use_file_store = TRUE)

  x1 <- create_random_packet(root, "x", list(a = 1))
  x2 <- create_random_packet(root, "x", list(a = 2))
  y1 <- create_random_packet(root, "y", list(a = 1))
  y2 <- create_random_packet(root, "y", list(a = 2))

  expect_equal(
    orderly_search(quote({report_x} || parameter:a == 2), # nolint
                  subquery = list(report_x = quote(name == "x")),
                  scope = quote(name == "y"),
                  root = root),
    y2)
})


describe("orderly_search can search for packets usedby another", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 3)
  ids["d"] <- create_random_dependent_packet(root, "d", ids[c("b", "c")])

  it("works for simple case", {
    expect_setequal(
      orderly_search(bquote(usedby(.(ids["b"]))),
                    scope = quote(name == "a"),
                    root = root),
      ids["a"])
  })

  it("works with subqueries", {
    expect_setequal(
      orderly_search(quote(usedby({report_b})), # nolint
                    scope = quote(name == "a"),
                    subquery = list(report_b = quote(latest(name == "b"))),
                    root = root),
      ids["a"])
  })

  it("can return only immediate dependencies", {
    expect_setequal(
      orderly_search(quote(usedby({report_d}, 1)), # nolint
                    subquery = list(report_d = quote(latest(name == "d"))),
                    root = root),
      ids[c("b", "c")])
  })

  it("can use named arg", {
    expect_setequal(
      orderly_search(quote(usedby({report_d}, depth = 1)), # nolint
                    subquery = list(report_d = quote(latest(name == "d"))),
                    root = root),
      ids[c("b", "c")])
  })

  it("can recurse full tree", {
    res <- orderly_search(quote(usedby({report_d})), # nolint
                         subquery = list(report_d = quote(latest(name == "d"))),
                         root = root)
    expect_setequal(res, ids[c("a", "b", "c")])
    expect_length(res, 3) ## Packets are not counted twice
  })

  it("returns empty vector when id has no dependencies", {
    expect_equal(
      orderly_search(bquote(usedby(.(ids["a"]))),
                    root = root),
      character(0))
  })

  it("returns empty vector when id unknown", {
    expect_equal(
      orderly_search(quote(usedby("123")),
                    scope = quote(name == "a"),
                    root = root),
      character(0))
  })
})


test_that("usedby returns multiple ids when parent used twice", {
  root <- create_temporary_root(use_file_store = TRUE)
  id_a1 <- create_random_packet(root, "a", list(x = 1))
  id_a2 <- create_random_packet(root, "a", list(x = 1))
  id_b <- create_random_dependent_packet(root, "b", c(id_a1, id_a2))

  expect_setequal(
    orderly_search(quote(usedby({report_b})), # nolint
                  scope = quote(name == "a"),
                  subquery = list(report_b = quote(latest(name == "b"))),
                  root = root),
    c(id_a1, id_a2))
})


test_that("usedby output can be used in groupings", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 2)
  ids["c"] <- create_random_dependent_packet(root, "c", ids[c("a", "b")])

  expect_setequal(
    orderly_search(quote(usedby({report_c}) && name == "b"), # nolint
                  subquery = list(report_c = quote(latest(name == "c"))),
                  root = root),
    ids["b"])
})


test_that("usedby errors if given expression which could return multiple ids", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 2)
  ids["b"] <- create_random_dependent_packet(root, "b", ids["a"])

  expect_error(
    orderly_search(quote(usedby({report_b})), # nolint
                  subquery = list(report_b = quote(name == "b")),
                  root = root),
    paste0("usedby must be called on an expression guaranteed to return a ",
           "single ID. Try wrapping expression in `latest` or `single`.\n",
           "  - in usedby({report_b})"),
    fixed = TRUE)

  ## Suggested fix works
  expect_equal(
    orderly_search(quote(usedby(latest({report_b}))), # nolint
                  subquery = list(report_b = quote(name == "b")),
                  root = root),
    ids["a"], ignore_attr = "names")
})

test_that("usedby returns empty vector if usedby called with 0 ids", {
  root <- create_temporary_root(use_file_store = TRUE)

  expect_equal(
    orderly_search(quote(usedby({latest(name == "b")})), root = root), # nolint
    character(0))
})

test_that("usedby depth works as expected", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 3)

  expect_setequal(
    orderly_search(quote(
      usedby({latest(name == "c")}, depth = 1)), root = root), # nolint
    ids["b"])

  expect_setequal(
    orderly_search(quote(
      usedby({latest(name == "c")}, depth = 2)), root = root), # nolint
    ids[c("a", "b")])

  expect_setequal(
    orderly_search(quote(
      usedby({latest(name == "c")}, depth = Inf)), root = root), # nolint
    ids[c("a", "b")])
})


test_that("useful errors returned when scope is invalid type", {
  root <- create_temporary_root(use_file_store = TRUE)

  expect_error(
    orderly_search(quote(latest()), scope = "the scope", root = root),
    "Invalid input for `scope`, it must be a language expression.")
})


describe("orderly_search can search for packets which use another", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- create_random_packet_chain(root, 3)
  ids["d"] <- create_random_dependent_packet(root, "d", ids[c("b", "c")])

  it("works for simple case", {
    expect_setequal(
      orderly_search(bquote(uses(.(ids["b"]))),
                    scope = quote(name == "c"),
                    root = root),
      ids["c"])
  })

  it("can return only immediate dependencies", {
    expect_setequal(
      orderly_search(quote(uses({report_a}, 1)), # nolint
                    subquery = list(report_a = quote(latest(name == "a"))),
                    root = root),
      ids["b"])
  })

  it("can use named arg", {
    expect_setequal(
      orderly_search(quote(uses({report_a}, depth = 2)), # nolint
                    subquery = list(report_a = quote(latest(name == "a"))),
                    root = root),
      ids[c("b", "c", "d")])
  })

  it("can recurse full tree", {
    res <- orderly_search(quote(uses({report_a})), # nolint
                         subquery = list(report_a = quote(latest(name == "a"))),
                         root = root)
    expect_setequal(res, ids[c("b", "c", "d")])
    expect_length(res, 3) ## Packets are not counted twice
  })

  it("returns empty vector when id has no dependencies", {
    expect_equal(
      orderly_search(bquote(uses(.(ids["d"]))),
                    root = root),
      character(0))
  })

  it("returns empty vector when id unknown", {
    expect_equal(
      orderly_search(quote(uses("123")),
                    scope = quote(name == "b"),
                    root = root),
      character(0))
  })
})


test_that("uses and usedby can be used together", {
  root <- create_temporary_root(use_file_store = TRUE)
  # nolint start
  ## Setup like
  ## A -> B -> C
  ##   \
  ##     V
  ## D -> E
  # nolint end
  ids <- create_random_packet_chain(root, 3)
  ids["d"] <- create_random_packet(root, "d")
  ids["e"] <- create_random_dependent_packet(root, "e", ids[c("a", "d")])

  ## We can get to C from E (up tree then down)
  expect_setequal(
    orderly_search(
      quote(uses(single(usedby(latest(name == "e")) && name == "a"))),
      scope = quote(name == "c"),
      root = root),
    ids["c"])

  ## We can get to E from C (up tree then down)
  expect_setequal(
    orderly_search(
      quote(uses(single(usedby(latest(name == "c")) && name == "a"))),
      scope = quote(name == "e"),
      root = root),
    ids["e"])

  ## We can get to A from D (down tree then up)
  expect_setequal(
    orderly_search(quote(usedby(single(uses(latest(name == "d"))))),
                  scope = quote(name == "a"),
                  root = root),
    ids["a"])

  ## We can get to D from A (down tree then up)
  expect_setequal(
    orderly_search(
      quote(usedby(single(uses(latest(name == "a")) && name == "e"))),
      scope = quote(name == "d"),
      root = root),
    ids["d"])

  ## We can get to D from C (up tree, then down, then up again)
  expect_setequal(
    orderly_search(
      quote(usedby(single(uses({a}) && name == "e"))), # nolint
      scope = quote(name == "d"),
      subquery = list(
        a = quote(single(usedby(latest(name == "c")) && name == "a"))),
      root = root),
    ids["d"])
})


test_that("adding scope filters queries", {
  root <- create_temporary_root(path_archive = NULL, use_file_store = TRUE)

  src <- withr::local_tempdir()
  id <- list(a = character(), b = character())
  for (i in 1:3) {
    for (name in  c("a", "b")) {
      p <- outpack_packet_start_quietly(src, name, parameters = list(i = i),
                                        root = root)
      outpack_packet_end_quietly(p)
      id[[name]] <- c(id[[name]], p$id)
    }
  }

  expect_equal(
    orderly_search("latest(parameter:i < 3)", root = root),
    id$b[[2]])
  expect_equal(
    orderly_search("latest(parameter:i < 3)", name = "a", root = root),
    id$a[[2]])
  expect_equal(
    orderly_search("latest(parameter:i < 3 && name == 'a')", root = root),
    id$a[[2]])
  expect_equal(
    orderly_search(orderly_query("latest(parameter:i < 3)",
                                 scope = quote(name == "a")), root = root),
    id$a[[2]])
})


## This test exercises a bunch of options that should all produce the
## same result, but did not once.
test_that("Same result with either strings/expressions, named or not", {
  root <- create_temporary_root(use_file_store = TRUE)

  ids <- vcapply(1:5, function(i) {
    create_random_packet(root, "x", list(a = i, b = 1))
  })

  dat <- list(list(query = quote(parameter:b == 1), result = ids),
              list(query = quote(parameter:a < 3), result = ids[1:2]),
              list(query = quote(latest(parameter:a < 3)), result = ids[[2]]))
  for (x in dat) {
    for (string in c(TRUE, FALSE)) {
      query <- if (string) deparse(x$query) else x$query
      expect_setequal(orderly_search(query, root = root), x$result)
      expect_setequal(orderly_search(query, root = root, name = "x"), x$result)
    }
  }
})


test_that("allow search before query", {
  root <- list()
  root$a <- create_temporary_root(use_file_store = TRUE)
  root$b <- create_temporary_root(use_file_store = TRUE)
  orderly_location_add("b", "path", list(path = root$b$path), root = root$a)
  ids <- vcapply(1:3, function(i) {
    create_random_packet(root$b, "data", list(p = i))
  })

  expect_equal(
    orderly_search(quote(name == "data"), root = root$a),
    character(0))
  expect_equal(
    orderly_search(quote(name == "data"), root = root$a,
                   options = list(pull_metadata = TRUE, allow_remote = TRUE)),
    ids)
  expect_setequal(names(root$a$index$data()$metadata), ids)
})


test_that("empty search returns full set", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- list(a = vcapply(1:3, function(i) create_random_packet(root, "a")),
              b = vcapply(1:3, function(i) create_random_packet(root, "b")))

  expect_equal(orderly_search(root = root),
               c(ids$a, ids$b))
  expect_equal(orderly_search(name = "a", root = root),
               c(ids$a))
})


test_that("can search for queries using boolean", {
  root <- create_temporary_root(use_file_store = TRUE)
  x1 <- create_random_packet(root, "x", list(a = TRUE))
  x2 <- create_random_packet(root, "x", list(a = FALSE))
  y1 <- create_random_packet(root, "y", list(a = "TRUE"))

  expect_equal(
    orderly_search(quote(parameter:a == TRUE), root = root),
    x1)
  expect_equal(
    orderly_search(quote(parameter:a == true), root = root),
    x1)
  expect_equal(
    orderly_search(quote(parameter:a == True), root = root),
    x1)
  expect_equal(
    orderly_search(quote(parameter:a == "TRUE"), root = root),
    c(y1))
  expect_equal(
    orderly_search(quote(parameter:a == 1), root = root),
    character(0))
  expect_equal(
    orderly_search(quote(parameter:a == "1"), root = root),
    character(0))
  expect_equal(
    orderly_search(quote(parameter:a == "true"), root = root),
    character(0))
  expect_equal(
    orderly_search(quote(parameter:a == "T"), root = root),
    character(0))

  expect_equal(
    orderly_search(quote(parameter:a == FALSE), root = root),
    x2)
  expect_equal(
    orderly_search(quote(parameter:a == false), root = root),
    x2)
  expect_equal(
    orderly_search(quote(parameter:a == False), root = root),
    x2)
  expect_equal(
    orderly_search(quote(parameter:a == "FALSE"), root = root),
    character(0))
  expect_equal(
    orderly_search(quote(parameter:a == 0), root = root),
    character(0))
  expect_equal(
    orderly_search(quote(parameter:a == "0"), root = root),
    character(0))
  expect_equal(
    orderly_search(quote(parameter:a == "false"), root = root),
    character(0))
  expect_equal(
    orderly_search(quote(parameter:a == "F"), root = root),
    character(0))
})
