test_that("can update empty contents", {
  expect_equal(
    gitignore_update_contents(character(), c("a", "b"), ".gitignore", "root"),
    c("# ---VVV--- added by orderly ---VVV----------------",
      "# Don't manually edit content between these markers",
      "a",
      "b",
      "# ---^^^--- added by orderly ---^^^----------------"))
})


test_that("can update contents without orderly", {
  expect_equal(
    gitignore_update_contents(c("x", "y"), c("a", "b"), ".gitignore", "root"),
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
    gitignore_update_contents(start, c("c", "d", "e"), ".gitignore", "root"),
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
    gitignore_update_contents(start, c("c", "d", "e"), ".gitignore", "root"),
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


test_that("can alert user on corrupt contents", {
  path <- ".gitignore"
  root <- "root"
  str <- gitignore_update_contents(c("x", "y"), c("c", "d", "e"), path, root)
  new <- c("f", "g")
  err <- expect_error(
    gitignore_update_contents(c(str, str), new, path, root),
    "Can't edit '.gitignore', markers are corrupted")
  expect_equal(err$body,
               c(i = "(within orderly root 'root')",
                 i = "Please see ?orderly_gitignore_update for more details"))
  err <- expect_error(
    gitignore_update_contents(rev(str), new, path, root),
    "Can't edit '.gitignore', markers are corrupted")
})


test_that("can create gitignore where nonexistant", {
  path_full <- withr::local_tempfile()
  root <- dirname(path_full)
  path <- basename(path_full)
  expect_true(gitignore_update_file(root, path, c("a", "b")))
  expect_false(gitignore_update_file(root, path, c("a", "b")))
  expect_equal(
    readLines(path_full),
    gitignore_update_contents(character(), c("a", "b"), path, root))
})


test_that("can create gitignore where empty", {
  path_full <- withr::local_tempfile()
  root <- dirname(path_full)
  path <- basename(path_full)
  file.create(path_full)
  expect_true(gitignore_update_file(root, path, c("a", "b")))
  expect_false(gitignore_update_file(root, path, c("a", "b")))
  expect_equal(
    readLines(path_full),
    gitignore_update_contents(character(), c("a", "b"), path, root))
})


test_that("can update file with existing contents", {
  path_full <- withr::local_tempfile()
  root <- dirname(path_full)
  path <- basename(path_full)
  writeLines(c("x", "y"), path_full)

  expect_true(gitignore_update_file(root, path, c("a", "b")))
  expect_false(gitignore_update_file(root, path, c("a", "b")))
  expect_equal(
    readLines(path_full),
    gitignore_update_contents(c("x", "y"), c("a", "b"), path, root))
})


test_that("can add a basic root gitignore", {
  path <- test_prepare_orderly_example("data")
  ignore <- c(".outpack", "orderly_envir.yml", "draft", "archive")
  expect_equal(gitignore_content_root(root_open(path, FALSE, FALSE)), ignore)
  expect_message(
    expect_true(orderly_gitignore_update("(root)", path)),
    "Wrote '.gitignore'")
  expect_equal(
    readLines(file.path(path, ".gitignore")),
    gitignore_update_contents(character(), ignore, path, root))
})


test_that("can add a source .gitignore", {
  path <- test_prepare_orderly_example("data")
  ignore <- c("data.rds")
  expect_equal(gitignore_content_src("data", root_open(path, FALSE, FALSE)),
               ignore)
  expect_message(
    expect_true(orderly_gitignore_update("data", path)),
    "Wrote 'src/data/.gitignore'")
  expect_equal(
    readLines(file.path(path, "src", "data", ".gitignore")),
    gitignore_update_contents(character(), ignore, path, root))
})


test_that("can compute ignore for dependencies", {
  path <- test_prepare_orderly_example("depends")
  expect_equal(
    gitignore_content_src("depends", root_open(path, FALSE, FALSE)),
    c("input.rds", "graph.png"))
})


test_that("can compute ignore for dependencies", {
  path <- test_prepare_orderly_example("shared")
  expect_setequal(
    gitignore_content_src("shared", root_open(path, FALSE, FALSE)),
    c("mygraph.png", "shared_data.csv"))
})


test_that("don't ignore resources that are artefacts", {
  path <- test_prepare_orderly_example("reexport")
  expect_equal(
    gitignore_content_src("reexport", root_open(path, FALSE, FALSE)),
    "mygraph.png")
})
