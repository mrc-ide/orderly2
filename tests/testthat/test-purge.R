test_that("Can purge a packet", {
  root <- create_temporary_root()
  ids <- create_random_packet_chain(root, 3)
  orderly_purge(ids[[3]], root = root)
})
