test_that("can simplify a query", {
  expect_equal(
    query_simplify(orderly_query("latest()")),
    list(outer = "latest", parts = set_names(list(), character())))
  expect_equal(
    query_simplify(orderly_query("latest(name == 'a')")),
    list(outer = "latest", parts = list(A = quote(name == "a"))))
  expect_equal(
    query_simplify(orderly_query("latest(name == 'a' && parameter:x == 1)")),
    list(outer = "latest", parts = list(A = quote(name == "a"),
                                        B = quote(parameter:x == 1))))
  expect_equal(
    query_simplify(
      orderly_query("name == 'a' && parameter:x == 1 && parameter:y == 2")),
    list(outer = character(),
         parts = list(A = quote(name == "a"),
                      B = quote(parameter:x == 1),
                      C = quote(parameter:y == 2))))
})


test_that("Can explain a query", {
  root <- create_temporary_root(use_file_store = TRUE)
  ids <- list()
  for (name in c("x", "y", "z")) {
    ids[[name]] <- vcapply(1:3, function(i) {
      create_random_packet(root, name, list(a = i, b = 2 * i))
    })
  }

  x <- orderly_query_explain(quote(latest()), root = root)
  out <- testthat::evaluate_promise(print(x))
  expect_length(out$messages, 1)
  expect_match(out$messages, "Evaluated query: 'latest()' and found 1 packet",
               fixed = TRUE)

  x <- orderly_query_explain(quote(latest(parameter:x > 10)), root = root)
  out <- testthat::evaluate_promise(print(x))
  expect_length(out$messages, 2)
  expect_match(out$messages[[1]],
               "Evaluated query: 'latest(A)' and found 0 packets",
               fixed = TRUE)
  expect_match(out$messages[[2]], "A (parameter:x > 10): 0 packets",
               fixed = TRUE)

  x <- orderly_query_explain(
    quote(latest(parameter:a == 1 && parameter:b == 4)), root = root)
  expect_s3_class(x, "orderly_query_explain")
  out <- testthat::evaluate_promise(print(x))
  expect_length(out$messages, 5)
  expect_match(out$messages[[1]],
               "Evaluated query: 'latest(A && B)' and found 0 packets",
               fixed = TRUE)
  expect_match(out$messages[[2]], "A (parameter:a == 1): 3 packets",
               fixed = TRUE)
  expect_match(out$messages[[3]], "B (parameter:b == 4): 3 packets",
               fixed = TRUE)
  expect_match(out$messages[[4]], "Pairwise combinations:",
               fixed = TRUE)
  expect_match(out$messages[[5]], "A && B: 0 packets",
               fixed = TRUE)

  x <- orderly_query_explain(
    quote(latest(parameter:a == 1 && parameter:b == 1)), root = root)
  expect_s3_class(x, "orderly_query_explain")
  out <- testthat::evaluate_promise(print(x))
  expect_length(out$messages, 3)
  expect_match(out$messages[[1]],
               "Evaluated query: 'latest(A && B)' and found 0 packets",
               fixed = TRUE)
  expect_match(out$messages[[2]], "A (parameter:a == 1): 3 packets",
               fixed = TRUE)
  expect_match(out$messages[[3]], "B (parameter:b == 1): 0 packets",
               fixed = TRUE)
})
