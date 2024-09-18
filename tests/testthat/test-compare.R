test_that("Comparing a packet to itself returns an empty diff", {
  root <- create_temporary_root()
  id <- create_random_packet(root)

  result <- orderly_compare_packets(id, id, root = root)
  expect_true(result$is_equal())
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
  expect_true(result$is_equal())
  expect_snapshot(print(result), transform = scrub_packets(id1, id2))
})


test_that("Can compare packets with different metadata", {
  root <- create_temporary_root()

  p1 <- create_deterministic_packet(root, "data", list(A = "foo"))
  p2 <- create_deterministic_packet(root, "data", list(A = "bar"))

  everything <- orderly_compare_packets(p1, p2, root = root)
  metadata <- orderly_compare_packets(p1, p2, root = root, what = "metadata")
  files <- orderly_compare_packets(p1, p2, root = root, what = "files")

  expect_false(everything$is_equal())
  expect_false(metadata$is_equal())
  expect_true(files$is_equal())

  expect_snapshot(print(everything), transform = scrub_packets(p1, p2))
  expect_snapshot(print(metadata), transform = scrub_packets(p1, p2))
  expect_snapshot(print(files), transform = scrub_packets(p1, p2))
})


test_that("Can compare packets with different file contents", {
  root <- create_temporary_root()

  p1 <- orderly_run_snippet(root, "data", writeLines("Hello", "data.txt"))
  p2 <- orderly_run_snippet(root, "data", writeLines("World", "data.txt"))

  everything <- orderly_compare_packets(p1, p2, root = root)
  metadata <- orderly_compare_packets(p1, p2, root = root, what = "metadata")
  files <- orderly_compare_packets(p1, p2, root = root, what = "files")

  expect_false(everything$is_equal())
  expect_true(metadata$is_equal())
  expect_false(files$is_equal())

  expect_snapshot(print(everything), transform = scrub_packets(p1, p2))
  expect_snapshot(print(metadata), transform = scrub_packets(p1, p2))
  expect_snapshot(print(files), transform = scrub_packets(p1, p2))
  expect_snapshot(print(files, verbose = TRUE),
                  transform = scrub_packets(p1, p2))
})


test_that("Can compare artefacts only", {
  root <- create_temporary_root()

  p1 <- orderly_run_snippet(root, "data", {
    orderly_artefact(description = "Output", "output.txt")
    writeLines(toString(2 + 1), "output.txt")
  })

  p2 <- orderly_run_snippet(root, "data", {
    orderly_artefact(description = "Output", "output.txt")
    writeLines(toString(1 + 2), "output.txt")
  })

  files <- orderly_compare_packets(p1, p2, what = "files", root = root)
  artefacts <- orderly_compare_packets(p1, p2, what = "artefacts", root = root)

  # The packet files, in particular the source code, are different. However the
  # different snippets produce identical artefacts.
  expect_false(files$is_equal())
  expect_true(artefacts$is_equal())

  expect_snapshot(print(files), transform = scrub_packets(p1, p2))
})


test_that("Can detect newly declared artefact", {
  root <- create_temporary_root()

  p1 <- orderly_run_snippet(root, "data", {
    writeLines("Hello", "hello.txt")
  })

  p2 <- orderly_run_snippet(root, "data", {
    orderly_artefact(description = "Output", "hello.txt")
    writeLines("Hello", "hello.txt")
  })

  artefacts <- orderly_compare_packets(p1, p2, what = "artefacts", root = root)

  expect_false(artefacts$is_equal())
  expect_snapshot(print(artefacts), transform = scrub_packets(p1, p2))
})


test_that("Can compare packets with binary contents", {
  root <- create_temporary_root()

  p1 <- orderly_run_snippet(root, "data", {
    orderly_artefact(description = "Output", "data.rds")
    saveRDS(1:10, "data.rds")
  })

  p2 <- orderly_run_snippet(root, "data", {
    orderly_artefact(description = "Output", "data.rds")
    saveRDS(11:20, "data.rds")
  })

  result <- orderly_compare_packets(p1, p2, what = "artefacts", root = root)
  expect_false(result$is_equal())
  expect_snapshot(print(result), transform = scrub_packets(p1, p2))
  expect_snapshot(print(result, verbose = TRUE),
                  transform = scrub_packets(p1, p2))
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


test_that("Can compare packets from remote", {
  here <- create_temporary_root()
  there <- create_temporary_root()

  p1 <- create_random_packet(there)
  p2 <- create_random_packet(here)

  orderly_location_add("there", "path", list(path = there$path), root = here)
  orderly_location_pull_metadata(root = here)

  expect_error(
    orderly_compare_packets(p1, p2, root = here),
    "Unable to copy files, as they are not available locally")

  result <- suppressMessages({
    orderly_compare_packets(p1, p2, search_options = list(allow_remote = TRUE),
                            root = here)
  })
  expect_false(result$is_equal())
})


test_that("Checks bad what argument", {
  root <- create_temporary_root()
  id <- create_random_packet(root)

  expect_error(
    orderly_compare_packets(id, id, root = root, what = "xx"),
    '`what` must be one of .*, not "xx"')

  expect_error(
    orderly_compare_packets(id, id, root = root, what = character(0)),
    "`what` must not be empty")

  expect_error(
    orderly_compare_packets(id, id, root = root, what = TRUE),
    "`what` must be a character vector")

  expect_error(
    orderly_compare_packets(id, id, root = root,
                            what = c("files", "artefacts")),
    '`what` must contain both "files" and "artefacts"')
})
