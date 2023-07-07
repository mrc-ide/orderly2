test_that("ids are valid", {
  id <- outpack_id()
  expect_silent(validate_outpack_id(id))
})


test_that("can detect malformed id", {
  expect_error(validate_outpack_id("myid"),
               "Malformed id 'myid'")
})


test_that("Can resolve dependencies where there are none", {
  expect_equal(
    find_all_dependencies("a", mock_metadata_depends("a")),
    "a")
  expect_equal(
    find_all_dependencies("a", c(mock_metadata_depends("a"),
                                 mock_metadata_depends("b", "a"))),
    "a")
})


test_that("Can filter dependency tree", {
  metadata <- c(
    mock_metadata_depends("a"),
    mock_metadata_depends("b"),
    mock_metadata_depends("c"),
    mock_metadata_depends("d", c("a", "b")),
    mock_metadata_depends("e", c("b", "c")),
    mock_metadata_depends("f", c("a", "c")),
    mock_metadata_depends("g", c("a", "f", "c")),
    mock_metadata_depends("h", c("a", "b", "c")),
    mock_metadata_depends("i", "f"),
    mock_metadata_depends("j", c("i", "e", "a")))

  expect_equal(find_all_dependencies("a", metadata), "a")
  expect_equal(find_all_dependencies("b", metadata), "b")
  expect_equal(find_all_dependencies("c", metadata), "c")

  expect_equal(find_all_dependencies("d", metadata), c("a", "b", "d"))
  expect_equal(find_all_dependencies("e", metadata), c("b", "c", "e"))
  expect_equal(find_all_dependencies("f", metadata), c("a", "c", "f"))

  expect_equal(find_all_dependencies("g", metadata), c("a", "c", "f", "g"))
  expect_equal(find_all_dependencies("h", metadata), c("a", "b", "c", "h"))
  expect_equal(find_all_dependencies("i", metadata), c("a", "c", "f", "i"))
  expect_equal(find_all_dependencies("j", metadata),
               c("a", "b", "c", "e", "f", "i", "j"))
})


test_that("Can select multiple dependencies at once", {
  metadata <- c(
    mock_metadata_depends("a"),
    mock_metadata_depends("b"),
    mock_metadata_depends("c"),
    mock_metadata_depends("d", c("a", "b")),
    mock_metadata_depends("e", c("b", "c")),
    mock_metadata_depends("f", c("a", "c")),
    mock_metadata_depends("g", c("a", "f", "c")),
    mock_metadata_depends("h", c("a", "b", "c")),
    mock_metadata_depends("i", "f"),
    mock_metadata_depends("j", c("i", "e", "a")))

  expect_equal(find_all_dependencies(character(0), metadata), character(0))
  expect_equal(find_all_dependencies(c("c", "b", "a"), metadata),
               c("a", "b", "c"))
  expect_equal(find_all_dependencies(c("d", "e", "f"), metadata),
               c("a", "b", "c", "d", "e", "f"))
})
