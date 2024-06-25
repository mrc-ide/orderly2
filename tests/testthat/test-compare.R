test_that("Comparing a packet to itself returns TRUE", {
  root <- create_temporary_root()
  id <- create_random_packet(root)

  result <- orderly_compare_packets(id, id, root = root)
  expect_true(result)
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
  expect_true(result)
  expect_snapshot(print(result), transform = scrub_packets(id1, id2))
})


test_that("Can compare packets with different metadata", {
  root <- create_temporary_root()

  p1 <- create_deterministic_packet(root, "data", list(A = "foo"))
  p2 <- create_deterministic_packet(root, "data", list(A = "bar"))

  all <- orderly_compare_packets(p1, p2, root = root)
  metadata <- orderly_compare_packets(p1, p2, what = "metadata", root = root)
  files <- orderly_compare_packets(p1, p2, what = "files", root = root)

  expect_false(isTRUE(all))
  expect_false(isTRUE(metadata))
  expect_true(files)

  expect_snapshot(print(all), transform = scrub_packets(p1, p2))
})


test_that("Can compare packets with different file contents", {
  root <- create_temporary_root()

  p1 <- orderly_run_snippet(root, "data", writeLines("Hello", "data.txt"))
  p2 <- orderly_run_snippet(root, "data", writeLines("World", "data.txt"))

  all <- orderly_compare_packets(p1, p2, root = root)
  metadata <- orderly_compare_packets(p1, p2, what = "metadata", root = root)
  files <- orderly_compare_packets(p1, p2, what = "files", root = root)

  expect_false(isTRUE(all))
  expect_true(metadata)
  expect_false(isTRUE(files))

  expect_snapshot(print(all), transform = scrub_packets(p1, p2))
})


test_that("Can compare artefacts only", {
  root <- create_temporary_root()

  p1 <- orderly_run_snippet(root, "data", {
    orderly_artefact("Output", "output.txt")
    writeLines(toString(2 + 1), "output.txt")
  })

  p2 <- orderly_run_snippet(root, "data", {
    orderly_artefact("Output", "output.txt")
    writeLines(toString(1 + 2), "output.txt")
  })

  files <- orderly_compare_packets(p1, p2, what = "files", root = root)
  artefacts <- orderly_compare_packets(p1, p2, what = "artefacts", root = root)

  # The packet files, in particular the source code, are different. However the
  # different snippets produce identical artefacts.
  expect_false(isTRUE(files))
  expect_true(artefacts)

  expect_snapshot(print(files), transform = scrub_packets(p1, p2))
})


test_that("Can detect newly declared artefact", {
  root <- create_temporary_root()

  p1 <- orderly_run_snippet(root, "data", {
    writeLines("Hello", "hello.txt")
  })

  p2 <- orderly_run_snippet(root, "data", {
    orderly_artefact("Output", "hello.txt")
    writeLines("Hello", "hello.txt")
  })

  files <- orderly_compare_packets(p1, p2, what = "files", root = root)
  artefacts <- orderly_compare_packets(p1, p2, what = "artefacts", root = root)

  f <- files$files[files$files$path == "hello.txt", ]
  expect_equal(f$status, "unchanged")

  # When comparing artefacts, the file is reported as "added", because while it
  # exists in the original packet, it was not an artefact.
  f <- artefacts$files[artefacts$files$path == "hello.txt", ]
  expect_equal(f$status, "added")
})


test_that("Can compare packets with binary contents", {
  root <- create_temporary_root()

  p1 <- orderly_run_snippet(root, "data", {
    orderly_artefact("Outputs", c("data.txt", "data.rds"))
    writeLines("Hello", "data.txt")
    saveRDS(1:10, "data.rds")
  })

  p2 <- orderly_run_snippet(root, "data", {
    orderly_artefact("Outputs", c("data.txt", "data.rds"))
    writeLines("World", "data.txt")
    saveRDS(11:20, "data.rds")
  })

  result <- orderly_compare_packets(p1, p2, what = "artefacts", root = root)
  expect_false(isTRUE(result))

  # We can't render a diff for binary files, so these are set to NULL, unlike
  # the text file.
  text <- result$files[result$files$path == "data.txt", ]
  binary <- result$files[result$files$path == "data.rds", ]
  expect_false(is.null(text$diff[[1]]))
  expect_true(is.null(binary$diff[[1]]))

  expect_snapshot(print(result), transform = scrub_packets(p1, p2))
})


test_that("Cannot compare artefacts of non-orderly packets", {
  root <- create_temporary_root()

  src <- withr::local_tempdir()
  writeLines("World", file.path(src, "data.txt"))
  p <- outpack_packet_start_quietly(src, "data", root = root)
  outpack_packet_end_quietly(p)

  # Only orderly packets have the right metadata needed to infer what is or
  # isn't an artefact.
  expect_error(
    orderly_compare_packets(p$id, p$id, what = "artefacts", root = root),
    "Cannot compare artefacts of non-orderly packets")

  # We can still do full packet comparison, even without the orderly-specific
  # metadata.
  expect_no_error(orderly_compare_packets(p$id, p$id, root = root))
  expect_no_error(
    orderly_compare_packets(p$id, p$id, what = "files", root = root))
})
