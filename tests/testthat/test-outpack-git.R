test_that("git_info on non-version-controlled path is NULL", {
  path <- withr::local_tempdir()
  expect_null(git_info(path))
})


test_that("can get git information from a path", {
  path <- withr::local_tempdir()
  gert::git_init(path)
  file.create(file.path(path, "hello"))
  gert::git_add(".", repo = path)
  user <- "author <author@example.com>"
  hash <- gert::git_commit("initial", author = user, committer = user,
                           repo = path)
  branch <- gert::git_branch(repo = path)
  expect_mapequal(
    git_info(path),
    list(sha = hash, branch = branch, url = character(0)))

  gert::git_remote_add("git@example.com/example", "origin", repo = path)
  expect_mapequal(
    git_info(path),
    list(sha = hash, branch = branch, url = "git@example.com/example"))

  gert::git_remote_add("https://example.com/git/example", "other", repo = path)
  expect_mapequal(
    git_info(path),
    list(sha = hash,
         branch = branch,
         url = c("git@example.com/example", "https://example.com/git/example")))
})


test_that("store git information into packet, if under git's control", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path_src <- create_temporary_simple_src()

  ## Note that the git repo is in the src, not in the outpack root
  git_info <- helper_add_git(path_src)

  p <- outpack_packet_start(path_src, "example", root = root)
  id <- p$id
  outpack_packet_run(p, "script.R")
  outpack_packet_end(p)

  meta <- orderly_metadata(id, root = root$path)
  expect_mapequal(meta$git,
                  list(branch = git_info$branch,
                       sha = git_info$sha,
                       url = git_info$url))
})


test_that("store no information into packet, if no git found", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path_src <- create_temporary_simple_src()

  p <- outpack_packet_start(path_src, "example", root = root)
  id <- p$id
  outpack_packet_run(p, "script.R")
  outpack_packet_end(p)

  meta <- orderly_metadata(id, root = root$path)
  expect_true("git" %in% names(meta))
  expect_null(meta$git)
})
