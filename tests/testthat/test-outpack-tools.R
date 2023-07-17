test_that("can extract simple data", {
  root <- create_temporary_root()
  ids <- vcapply(1:5, function(i) {
    create_random_packet(root, parameters = list(i = i))
  })

  d <- outpack_metadata_extract('name == "data"', root = root)
  expect_setequal(names(d), c("id", "name", "parameters"))
  expect_equal(d$id, ids)
  expect_equal(d$name, rep("data", 5))
  expect_equal(d$parameters, I(lapply(1:5, function(i) list(i = i))))
})

test_that("can extract from parameters", {
  root <- create_temporary_root()
  ids <- vcapply(1:5, function(i) {
    create_random_packet(root, parameters = list(i = i))
  })
  meta <- lapply(ids, outpack_metadata, root = root)

  d <- outpack_metadata_extract('name == "data"',
                                extract = c(i = "parameters.i"),
                                root = root)
  expect_setequal(names(d), c("id", "i"))
  expect_equal(d$id, ids)
  expect_equal(d$i, I(as.list(1:5)))

  d <- outpack_metadata_extract('name == "data"',
                                extract = c("name", "time"),
                                root = root)
  expect_setequal(names(d), c("id", "name", "time"))
  expect_equal(d$id, ids)
  expect_equal(d$name, rep("data", 5))
  expect_equal(d$time, I(lapply(meta, "[[", "time")))
})

test_that("can extract from time", {
  root <- create_temporary_root()
  ids <- vcapply(1:5, function(i) {
    create_random_packet(root, parameters = list(i = i))
  })
  meta <- lapply(ids, outpack_metadata, root = root)

  d <- outpack_metadata_extract(
    'name == "data"',
    extract = c(start = "time.start", end = "time.end"),
    root = root)
  expect_setequal(names(d), c("id", "start", "end"))
  expect_equal(d$id, ids)
  expect_equal(d$start, num_to_time(vnapply(meta, function(x) x$time$start)))
  expect_equal(d$end, num_to_time(vnapply(meta, function(x) x$time$end)))
  expect_equal(d$end, num_to_time(vnapply(meta, function(x) x$time$end)))
})
