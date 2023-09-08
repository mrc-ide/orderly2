test_that("can retrieve simple graph information", {
  root <- create_temporary_root()
  ids <- create_random_packet_chain(root, 3)

  expect_error(
    orderly_graph_packets(from = ids[["a"]], to = ids[["c"]], root = root),
    "Exactly one of 'from' and 'to' must be given")
  expect_error(
    orderly_graph_packets(root = root),
    "Exactly one of 'from' and 'to' must be given")

  g1 <- orderly_graph_packets(to = ids[["c"]], root = root)
  expect_equal(g1$packets, unname(ids))
  expect_equal(nrow(g1$edges), 2)
  expect_equal(g1$edges$from, unname(ids[2:1]))
  expect_equal(g1$edges$to, unname(ids[3:2]))
  expect_equal(names(g1$edges), c("from", "to", "query", "files"))

  g2 <- orderly_graph_packets(to = ids[["a"]], root = root)
  expect_equal(g2$packets, ids[[1]])
  expect_equal(nrow(g2$edges), 0)
  expect_equal(g2$edges, g1$edges[integer(), ])

  g3 <- orderly_graph_packets(from = ids[["c"]], root = root)
  expect_equal(g3$packets, ids[[3]])
  expect_equal(nrow(g3$edges), 0)
  expect_equal(g3$edges, g2$edges)

  g4 <- orderly_graph_packets(from = ids[["a"]], root = root)
  expect_equal(g4$packets, unname(ids))
  expect_equal(nrow(g4$edges), 2)
  expect_equal(g4$edges$from, unname(ids[1:2]))
  expect_equal(g4$edges$to, unname(ids[2:3]))
  expect_equal(names(g4$edges), c("from", "to", "query", "files"))
})
