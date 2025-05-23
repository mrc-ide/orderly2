test_that("Can validate a packet that is valid", {
  root <- create_temporary_root()
  id <- create_random_packet(root = root)
  res <- evaluate_promise(orderly_validate_archive(id, root = root))
  expect_match(res$messages, sprintf("%s (data) is valid", id), fixed = TRUE)
  expect_equal(res$result, character())
})


test_that("Can validate a packet that is invalid", {
  root <- create_temporary_root()
  id <- create_random_packet(root = root)
  path <- file.path(root$path, "archive", "data", id, "data.rds")
  forcibly_truncate_file(path)
  res <- evaluate_promise(orderly_validate_archive(id, root = root))
  expect_match(res$messages, sprintf("%s (data) is invalid", id), fixed = TRUE)
  expect_equal(res$result, id)
  expect_true(file.exists(path)) # not deleted
  expect_equal(root$index$unpacked(), id) # still there
})


test_that("Can orphan an invalid packet", {
  root <- create_temporary_root()
  ids <- replicate(3, create_random_packet(root = root))
  id <- ids[[2]]
  path <- file.path(root$path, "archive", "data", id, "data.rds")
  forcibly_truncate_file(path)
  res <- evaluate_promise(
    orderly_validate_archive(id, action = "orphan", root = root))
  expect_match(res$messages, sprintf("%s (data) is invalid", id), fixed = TRUE)
  expect_equal(res$result, id)
  expect_true(file.exists(path)) # not deleted
  expect_equal(root$index$unpacked(), ids[-2])
  expect_equal(root$index$location(orphan)$packet, id)
})


test_that("Can delete an invalid packet", {
  root <- create_temporary_root()
  ids <- replicate(3, create_random_packet(root = root))
  id <- ids[[2]]
  path <- file.path(root$path, "archive", "data", id, "data.rds")
  forcibly_truncate_file(path)
  res <- evaluate_promise(
    orderly_validate_archive(action = "delete", root = root))
  expect_match(res$messages, sprintf("%s (data) is invalid", id), fixed = TRUE,
               all = FALSE)
  expect_equal(res$result, id)
  expect_false(file.exists(path))
  expect_equal(root$index$unpacked(), ids[-2])
  expect_equal(root$index$location(orphan)$packet, id)
})


test_that("nothing to validate if no archive", {
  root <- create_temporary_root(use_file_store = TRUE, path_archive = NULL)
  ids <- replicate(3, create_random_packet(root = root))
  expect_error(orderly_validate_archive(root = root),
               "You have no archive to validate")
})


test_that("recursively validate, errors in upstream are a problem", {
  root <- create_temporary_root(require_complete_tree = TRUE)
  ids <- create_random_packet_chain(5, root = root)

  forcibly_truncate_file(
    file.path(root$path, "archive", "c", ids[["c"]], "data.rds"))

  res <- evaluate_promise(
    orderly_validate_archive(ids[["d"]], root = root))
  expect_equal(res$result, unname(ids[3:4]))
  expect_length(res$messages, 4)
  expect_match(res$messages[[1]], sprintf("%s (a) is valid", ids[["a"]]),
               fixed = TRUE)
  expect_match(res$messages[[2]], sprintf("%s (b) is valid", ids[["b"]]),
               fixed = TRUE)
  expect_match(res$messages[[3]],
               sprintf("%s (c) is invalid due to its files", ids[["c"]]),
               fixed = TRUE)
  expect_match(res$messages[[4]],
               sprintf("%s (d) is invalid due to its upstream packets",
                       ids[["d"]]),
               fixed = TRUE)

  res2 <- evaluate_promise(
    orderly_validate_archive(ids[["d"]], action = "orphan", root = root))
  expect_equal(res, res2)

  expect_equal(root$index$unpacked(), unname(ids[-(3:4)]))
  expect_equal(root$index$location(orphan)$packet, unname(ids[3:4]))
})


test_that("invalidate all children of corrupt parent", {
  root <- create_temporary_root(require_complete_tree = TRUE)
  id1 <- create_random_packet(root)
  id2 <- replicate(3, create_random_dependent_packet(root, "child", id1))
  id3 <- create_random_dependent_packet(root, "grandchild", id2)
  res <- evaluate_promise(orderly_validate_archive(root = root))
  expect_equal(res$result, character())
  expect_length(res$messages, 5)
  re <- "^.+ ([0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}) \\([a-z]+\\) is valid\\n$"
  expect_match(res$messages, re, all = TRUE)
  expect_equal(sub(re, "\\1", res$messages), c(id1, id2, id3))
  ## Same if we start from the end; all 5 in the same order
  expect_equal(
    evaluate_promise(orderly_validate_archive(id3, root = root)),
    res)

  forcibly_truncate_file(
    file.path(root$path, "archive", "child", id2[[1]], "data.rds"))
  res <- evaluate_promise(orderly_validate_archive(id3, root = root))
  expect_match(res$messages[c(1, 3, 4)], re, all = TRUE)
  expect_match(res$messages[[2]],
               sprintf("%s (child) is invalid due to its files", id2[[1]]),
               fixed = TRUE)
  expect_match(
    res$messages[[5]],
    sprintf("%s (grandchild) is invalid due to its upstream packets", id3),
    fixed = TRUE)
})


test_that("don't invalidate children when complete tree off", {
  root <- create_temporary_root(require_complete_tree = FALSE)
  id1 <- create_random_packet(root)
  id2 <- replicate(3, create_random_dependent_packet(root, "child", id1))
  id3 <- create_random_dependent_packet(root, "grandchild", id2)
  res <- evaluate_promise(orderly_validate_archive(root = root))
  expect_equal(res$result, character())
  expect_length(res$messages, 5)
  re <- "^.+ ([0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}) \\([a-z]+\\) is valid\\n$"
  expect_match(res$messages, re, all = TRUE)
  expect_equal(sub(re, "\\1", res$messages), c(id1, id2, id3))
  ## No longer recurses if called from the end
  res <- evaluate_promise(orderly_validate_archive(id3, root = root))
  expect_length(res$messages, 1)

  ## We no longer know, or care, if this is invalid due to children
  forcibly_truncate_file(
    file.path(root$path, "archive", "child", id2[[1]], "data.rds"))
  expect_equal(
    evaluate_promise(orderly_validate_archive(id3, root = root)),
    res)
})


test_that("can inform sensibly if jsonvalidate is missing", {
  skip_if_not_installed("mockery")
  mockery::stub(ensure_jsonvalidate,
                "requireNamespace",
                mockery::mock(TRUE, FALSE))
  expect_no_error(ensure_jsonvalidate())
  expect_error(
    ensure_jsonvalidate(),
    "The jsonvalidate package is missing, but you have requested it")
})
