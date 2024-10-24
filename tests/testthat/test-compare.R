test_that("Comparing a packet to itself returns an empty diff", {
  root <- create_temporary_root()
  id <- create_random_packet(root)

  result <- orderly_compare_packets(id, id, root = root)
  expect_true(orderly_comparison_explain(result, verbose = "silent"))
  expect_snapshot(print(result), transform = scrub_packets(id))
})


test_that("Comparing packets ignores ID and time differences", {
  root <- create_temporary_root()
  id1 <- create_deterministic_packet(root)
  id2 <- create_deterministic_packet(root)

  meta1 <- orderly_metadata(id1, root = root)
  meta2 <- orderly_metadata(id2, root = root)
  expect_false(isTRUE(all.equal(meta1$id, meta2$id)))
  expect_false(isTRUE(all.equal(meta1$time, meta2$time)))

  result <- orderly_compare_packets(id1, id2, root = root)
  expect_true(orderly_comparison_explain(result, verbose = "silent"))
  expect_snapshot(print(result), transform = scrub_packets(id1, id2))
})


test_that("Can explicitly compare trivial fields", {
  root <- create_temporary_root()
  id1 <- create_deterministic_packet(root)
  id2 <- create_deterministic_packet(root)

  result <- orderly_compare_packets(id1, id2, root = root)

  expect_true(orderly_comparison_explain(result, verbose = "silent"))
  expect_false(orderly_comparison_explain(result, c("id", "time"),
                                          verbose = "silent"))

  expect_snapshot(orderly_comparison_explain(result, "id"),
                  transform = scrub_packets(id1, id2))
})


test_that("Can compare packets with different metadata", {
  root <- create_temporary_root()

  id1 <- create_deterministic_packet(root, "data", list(A = "foo"))
  id2 <- create_deterministic_packet(root, "data", list(A = "bar"))

  result <- orderly_compare_packets(id1, id2, root = root)
  expect_false(orderly_comparison_explain(result, verbose = "silent"))
  expect_true(orderly_comparison_explain(result, "files", verbose = "silent"))

  expect_snapshot(print(result), transform = scrub_packets(id1, id2))
  expect_snapshot(orderly_comparison_explain(result),
                  transform = scrub_packets(id1, id2))
  expect_snapshot(orderly_comparison_explain(result, "files"),
                  transform = scrub_packets(id1, id2))
})

test_that("Can compare packets with different file contents", {
  root <- create_temporary_root()

  id1 <- orderly_run_snippet(root, "data", writeLines("Hello", "data.txt"))
  id2 <- orderly_run_snippet(root, "data", writeLines("World", "data.txt"))

  result <- orderly_compare_packets(id1, id2, root = root)

  expect_false(orderly_comparison_explain(result, verbose = "silent"))
  expect_false(orderly_comparison_explain(result, "files", verbose = "silent"))
  expect_true(orderly_comparison_explain(result, "parameters",
                                         verbose = "silent"))

  expect_snapshot(print(result), transform = scrub_packets(id1, id2))
  expect_snapshot(orderly_comparison_explain(result, "files"),
                  transform = scrub_packets(id1, id2))
  expect_snapshot(orderly_comparison_explain(result, "files", verbose = TRUE),
                  transform = scrub_packets(id1, id2))
})

test_that("Can compare packets with binary contents", {
  root <- create_temporary_root()

  id1 <- orderly_run_snippet(root, "data", {
    orderly_artefact(description = "Output", "data.rds")
    saveRDS(1:10, "data.rds")
  })

  id2 <- orderly_run_snippet(root, "data", {
    orderly_artefact(description = "Output", "data.rds")
    saveRDS(11:20, "data.rds")
  })

  result <- orderly_compare_packets(id1, id2, root = root)

  expect_false(orderly_comparison_explain(result, verbose = "silent"))
  expect_false(orderly_comparison_explain(result, "files", verbose = "silent"))
  expect_true(orderly_comparison_explain(result, "parameters",
                                         verbose = "silent"))

  expect_snapshot(print(result), transform = scrub_packets(id1, id2))
  expect_snapshot(orderly_comparison_explain(result, "files"),
                  transform = scrub_packets(id1, id2))
  expect_snapshot(orderly_comparison_explain(result, "files", verbose = TRUE),
                  transform = scrub_packets(id1, id2))
})

test_that("Can compare packets from remote", {
  here <- create_temporary_root()
  there <- create_temporary_root()

  id1 <- create_random_packet(here)
  id2 <- create_random_packet(there)

  orderly_location_add("there", "path", list(path = there$path), root = here)

  expect_error(
    orderly_compare_packets(id1, id2, root = here),
    "Packet .* not found in outpack index")

  result <- orderly_compare_packets(id1, id2, root = here,
                                    fetch_metadata = TRUE)

  expect_snapshot(orderly_comparison_explain(result, "files"),
                  transform = scrub_packets(id1, id2))
  expect_snapshot(orderly_comparison_explain(result, "files", verbose = TRUE),
                  transform = scrub_packets(id1, id2))
})


test_that("Handles new attributes gracefully", {
  root <- create_temporary_root()

  id1 <- create_random_packet(root)
  id2 <- create_random_packet(root)

  # This is a horrible hack, designed to mimick the day where we may add new
  # attributes to the metadata.
  f <- file.path(root$path, ".outpack", "metadata", id1)
  metadata <- parse_json(file(f))
  metadata$new_key <- "value"
  writeLines(to_json(metadata, auto_unbox = TRUE), f)

  result <- orderly_compare_packets(id1, id2, root = root)
  expect_snapshot(print(result),
                  transform = scrub_packets(id1, id2))

  result_swap <- orderly_compare_packets(id2, id1, root = root)
  expect_snapshot(print(result_swap),
                  transform = scrub_packets(id1, id2))
})
