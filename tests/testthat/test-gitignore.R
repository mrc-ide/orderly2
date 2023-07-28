test_that("can update empty contents", {
  expect_equal(
    gitignore_update_contents(character(), c("a", "b")),
    c("# ---VVV--- added by orderly ---VVV----------------",
      "# Don't manually edit content between these markers",
      "a",
      "b",
      "# ---^^^--- added by orderly ---^^^----------------"))
})


test_that("can update contents without orderly", {
  expect_equal(
    gitignore_update_contents(c("x", "y"), c("a", "b")),
    c("x",
      "y",
      "",
      "# ---VVV--- added by orderly ---VVV----------------",
      "# Don't manually edit content between these markers",
      "a",
      "b",
      "# ---^^^--- added by orderly ---^^^----------------"))
})


test_that("can update existing contents", {
  start <- c("x",
             "y",
             "",
             "# ---VVV--- added by orderly ---VVV----------------",
             "# Don't manually edit content between these markers",
             "a",
             "b",
             "# ---^^^--- added by orderly ---^^^----------------")
  expect_equal(
    gitignore_update_contents(start, c("c", "d", "e")),
    c("x",
      "y",
      "",
      "# ---VVV--- added by orderly ---VVV----------------",
      "# Don't manually edit content between these markers",
      "c",
      "d",
      "e",
      "# ---^^^--- added by orderly ---^^^----------------"))
})


test_that("can update existing contents", {
  start <- c("x",
             "y",
             "",
             "# ---VVV--- added by orderly ---VVV----------------",
             "# Don't manually edit content between these markers",
             "a",
             "b",
             "# ---^^^--- added by orderly ---^^^----------------",
             "z")
  expect_equal(
    gitignore_update_contents(start, c("c", "d", "e")),
    c("x",
      "y",
      "",
      "# ---VVV--- added by orderly ---VVV----------------",
      "# Don't manually edit content between these markers",
      "c",
      "d",
      "e",
      "# ---^^^--- added by orderly ---^^^----------------",
      "z"))
})


test_that("can create gitignore where nonexistant", {
  path_full <- withr::local_tempfile()
  root <- dirname(path_full)
  path <- basename(path_full)
  expect_true(gitignore_update_file(root, path, c("a", "b"), "never"))
  expect_false(gitignore_update_file(root, path, c("a", "b"), "never"))
  expect_equal(readLines(path_full),
               gitignore_update_contents(character(), c("a", "b")))
})


test_that("can create gitignore where empty", {
  path_full <- withr::local_tempfile()
  root <- dirname(path_full)
  path <- basename(path_full)
  file.create(path_full)
  expect_true(gitignore_update_file(root, path, c("a", "b"), "never"))
  expect_false(gitignore_update_file(root, path, c("a", "b"), "never"))
  expect_equal(readLines(path_full),
               gitignore_update_contents(character(), c("a", "b")))
})


test_that("can update file with existing contents", {
  path_full <- withr::local_tempfile()
  root <- dirname(path_full)
  path <- basename(path_full)
  writeLines(c("x", "y"), path_full)

  expect_true(gitignore_update_file(root, path, c("a", "b"), "never"))
  expect_false(gitignore_update_file(root, path, c("a", "b"), "never"))
  expect_equal(readLines(path_full),
               gitignore_update_contents(c("x", "y"), c("a", "b")))
})


test_that("can prompt to update when creating a new file", {
  skip_if_not_installed("mockery")
  mock_prompt_update <- mockery::mock(TRUE, cycle = TRUE)
  mock_writelines <- mockery::mock()

  mockery::stub(gitignore_update_file, "prompt_update", mock_prompt_update)
  mockery::stub(gitignore_update_file, "writeLines", mock_writelines)

  path_full <- withr::local_tempfile()
  root <- dirname(path_full)
  path <- basename(path_full)

  expect_true(gitignore_update_file(root, path, c("x", "y"), "always"))
  mockery::expect_called(mock_prompt_update, 1)
  mockery::expect_called(mock_writelines, 1)

  expect_true(gitignore_update_file(root, path, c("x", "y"), "if_new_file"))
  mockery::expect_called(mock_prompt_update, 2)
  mockery::expect_called(mock_writelines, 2)

  expect_true(gitignore_update_file(root, path, c("x", "y"),
                                    "if_manually_created"))
  mockery::expect_called(mock_prompt_update, 2)
  mockery::expect_called(mock_writelines, 3)

  expect_true(gitignore_update_file(root, path, c("x", "y"), "never"))
  mockery::expect_called(mock_prompt_update, 2)
  mockery::expect_called(mock_writelines, 4)

  contents <- gitignore_update_contents(character(), c("x", "y"))
  expect_equal(mockery::mock_args(mock_prompt_update),
               rep(list(list(character(), contents, path, root)), 2))
  expect_equal(mockery::mock_args(mock_writelines),
               rep(list(list(contents, path_full)), 4))
})


test_that("can prompt to update when adding to a file without markers", {
  skip_if_not_installed("mockery")
  mock_prompt_update <- mockery::mock(TRUE, cycle = TRUE)
  mock_writelines <- mockery::mock()

  mockery::stub(gitignore_update_file, "prompt_update", mock_prompt_update)
  mockery::stub(gitignore_update_file, "writeLines", mock_writelines)

  path_full <- withr::local_tempfile()
  root <- dirname(path_full)
  path <- basename(path_full)
  writeLines(c("a", "b"), path_full)

  expect_true(gitignore_update_file(root, path, c("x", "y"), "always"))
  mockery::expect_called(mock_prompt_update, 1)
  mockery::expect_called(mock_writelines, 1)

  expect_true(gitignore_update_file(root, path, c("x", "y"),
                                    "if_manually_created"))
  mockery::expect_called(mock_prompt_update, 2)
  mockery::expect_called(mock_writelines, 2)

  expect_true(gitignore_update_file(root, path, c("x", "y"), "if_new_file"))
  mockery::expect_called(mock_prompt_update, 2)
  mockery::expect_called(mock_writelines, 3)

  expect_true(gitignore_update_file(root, path, c("x", "y"), "never"))
  mockery::expect_called(mock_prompt_update, 2)
  mockery::expect_called(mock_writelines, 4)

  contents <- gitignore_update_contents(c("a", "b"), c("x", "y"))
  expect_equal(mockery::mock_args(mock_prompt_update),
               rep(list(list(c("a", "b"), contents, path, root)), 2))
  expect_equal(mockery::mock_args(mock_writelines),
               rep(list(list(contents, path_full)), 4))
})


test_that("can prompt to update when adding to a file with markers", {
  skip_if_not_installed("mockery")
  mock_prompt_update <- mockery::mock(TRUE, cycle = TRUE)
  mock_writelines <- mockery::mock()

  mockery::stub(gitignore_update_file, "prompt_update", mock_prompt_update)
  mockery::stub(gitignore_update_file, "writeLines", mock_writelines)

  path_full <- withr::local_tempfile()
  root <- dirname(path_full)
  path <- basename(path_full)
  old <- gitignore_update_contents(c("a", "b"), c("x1", "y1"))
  writeLines(old, path_full)
  new <- c("x2", "y", "z2")

  expect_true(gitignore_update_file(root, path, new, "always"))
  mockery::expect_called(mock_prompt_update, 1)
  mockery::expect_called(mock_writelines, 1)

  expect_true(gitignore_update_file(root, path, new, "if_manually_created"))
  mockery::expect_called(mock_prompt_update, 1)
  mockery::expect_called(mock_writelines, 2)

  expect_true(gitignore_update_file(root, path, new, "if_new_file"))
  mockery::expect_called(mock_prompt_update, 1)
  mockery::expect_called(mock_writelines, 3)

  expect_true(gitignore_update_file(root, path, new, "never"))
  mockery::expect_called(mock_prompt_update, 1)
  mockery::expect_called(mock_writelines, 4)

  contents <- gitignore_update_contents(old, new)
  expect_equal(mockery::mock_args(mock_prompt_update),
               list(list(old, contents, path, root)))
  expect_equal(mockery::mock_args(mock_writelines),
               rep(list(list(contents, path_full)), 4))
})


test_that("prompting does not make changes to a file if not change requested", {
  skip_if_not_installed("mockery")
  mock_prompt_update <- mockery::mock(FALSE, cycle = TRUE)
  mock_writelines <- mockery::mock()
  mockery::stub(gitignore_update_file, "prompt_update", mock_prompt_update)
  mockery::stub(gitignore_update_file, "writeLines", mock_writelines)
  path_full <- withr::local_tempfile()
  root <- dirname(path_full)
  path <- basename(path_full)
  expect_false(gitignore_update_file(root, path, c("x", "y"), "always"))
  mockery::expect_called(mock_prompt_update, 1)
  mockery::expect_called(mock_writelines, 0)
})


test_that("prompting shows difference and asks user for input", {
  skip_if_not_installed("mockery")
  mock_ask <- mockery::mock(TRUE, FALSE)
  mockery::stub(prompt_update, "prompt_ask_yes_no", mock_ask)
  old <- c("a", "b", "c", "d")
  new <- c("b", "x", "c", "d", "y")
  path <- "path.txt"
  root <- "/path/to/root"
  a <- testthat::evaluate_promise(prompt_update(old, new, path, root))
  b <- testthat::evaluate_promise(prompt_update(old, new, path, root))

  expect_equal(a$result, TRUE)
  expect_equal(b$result, FALSE)
  expect_equal(a$messages, b$messages[1:4])
  expect_match(a$messages[[1]],
               "I am going to make changes to the file 'path.txt'\n$")
  expect_match(a$messages[[2]],
               "\\(within orderly root '/path/to/root'\\)\n$")
  expect_match(a$messages[[3]],
               "Proposed changes:\n$")
  expect_match(a$messages[[4]], "-a\n")
  expect_equal(b$messages[[5]], "Not making any changes to the file\n")

  mockery::expect_called(mock_ask, 2)
  expect_equal(mockery::mock_args(mock_ask),
               rep(list(list("OK to apply these changes?")), 2))
})


test_that("can add a basic root gitignore", {
  path <- test_prepare_orderly_example("data")
  ignore <- c(".outpack", "orderly_envir.yml", "draft", "archive")
  expect_equal(gitignore_content_root(root_open(path, FALSE, FALSE)), ignore)
  expect_message(
    expect_true(orderly_gitignore_update("(root)", "never", path)),
    "Wrote '.gitignore'")
  expect_equal(
    readLines(file.path(path, ".gitignore")),
    gitignore_update_contents(character(), ignore))
})


test_that("can add a source .gitignore", {
  path <- test_prepare_orderly_example("data")
  ignore <- c("data.rds")
  expect_equal(gitignore_content_src("data", root_open(path, FALSE, FALSE)),
               ignore)
  expect_message(
    expect_true(orderly_gitignore_update("data", "never", path)),
    "Wrote 'src/data/.gitignore'")
  expect_equal(
    readLines(file.path(path, "src", "data", ".gitignore")),
    gitignore_update_contents(character(), ignore))
})
