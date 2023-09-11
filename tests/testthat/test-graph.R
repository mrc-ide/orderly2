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


test_that("can extract graph from more interesting examples", {
  ## (a, b) -> c
  root <- create_temporary_root()
  ids <- character()
  ids[["a"]] <- create_random_packet(root, "a")
  ids[["b"]] <- create_random_packet(root, "b")
  ids[["c"]] <- create_random_dependent_packet(root, "c", ids[c("a", "b")])

  g <- orderly_graph_packets(to = ids[["c"]], root = root)
  expect_setequal(g$packets, unname(ids))
  expect_setequal(
    sprintf("%s -> %s", g$edges$from, g$edges$to),
    sprintf("%s -> %s", ids[c("a", "b")], ids[["c"]]))

  ids[["d"]] <- create_random_dependent_packet(root, "d", ids[["a"]])
  ids[["e"]] <- create_random_dependent_packet(root, "d", ids[c("c", "d")])

  ## Even with more packets present:
  expect_equal(orderly_graph_packets(to = ids[["c"]], root = root), g)

  g2 <- orderly_graph_packets(to = ids[["e"]], root = root)
  expect_setequal(
    sprintf("%s -> %s", g2$edges$from, g2$edges$to),
    sprintf("%s -> %s",
            ids[c("a", "b", "a", "c", "d")],
            ids[c("c", "c", "d", "e", "e")]))
})


test_that("sensible error if leaf packet not found", {
  root <- create_temporary_root()
  expect_error(
    orderly_graph_packets(from = "20230911-154647-532c17ff", root = root),
    "Packet '20230911-154647-532c17ff' does not exist for 'from'")
  expect_error(
    orderly_graph_packets(to = "20230911-154647-532c17ff", root = root),
    "Packet '20230911-154647-532c17ff' does not exist for 'to'")
})
