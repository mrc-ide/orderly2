test_that("Configuration must be empty", {
  tmp <- tempfile()
  on.exit(fs::dir_delete(tmp))
  fs::dir_create(tmp)
  writeLines(c(empty_config_contents(), "a: 1"),
             file.path(tmp, "orderly_config.yml"))
  expect_error(orderly_config_read(tmp),
               "Unknown fields in .+: a")
})


test_that("Configuration must exist", {
  tmp <- tempfile()
  on.exit(fs::dir_delete(tmp))
  fs::dir_create(tmp)
  outpack_init_no_orderly(tmp)
  expect_error(orderly_config_read(tmp),
               "Orderly configuration does not exist: 'orderly_config.yml'")
})


test_that("error of opening an outpack root that is not an orderly root", {
  tmp <- withr::local_tempfile()
  root <- outpack_init_no_orderly(tmp)

  err <- expect_error(
    withr::with_dir(tmp, root_open(".", require_orderly = TRUE)),
    "Did not find 'orderly_config.yml' in '.",
    fixed = TRUE)
  expect_equal(
    err$body,
    c(x = paste("Your directory has an '.outpack/' path, so is a valid",
                "outpack root, but does not contain 'orderly_config.yml' so",
                "cannot be used as an orderly root"),
      i = 'Please run orderly2::orderly_init(".") to initialise',
      i = "See ?orderly_init for more arguments to this function"))
})


test_that("pass back a root", {
  path_outpack <- withr::local_tempfile()
  root_outpack <- outpack_init_no_orderly(path_outpack)
  path_orderly <- test_prepare_orderly_example(character())
  root_orderly <- root_open(path_orderly, require_orderly = TRUE)

  expect_identical(root_open(root_orderly, require_orderly = FALSE),
                   root_orderly)
  expect_identical(root_open(root_orderly, require_orderly = TRUE),
                   root_orderly)
  expect_identical(root_open(root_outpack, require_orderly = FALSE),
                   root_outpack)
  expect_error(
    root_open(root_outpack, require_orderly = TRUE),
    sprintf("Did not find 'orderly_config.yml' in '%s'", root_outpack$path))
})


test_that("can silently detect that git setup is ok", {
  root <- create_temporary_root()
  info <- helper_add_git(root$path)
  expect_false(file.exists(file.path(root$path, ".outpack", "r", "git_ok")))
  expect_silent(root_check_git(root, NULL))
  expect_true(file.exists(file.path(root$path, ".outpack", "r", "git_ok")))
})


test_that("can silently notice that git is not used", {
  root <- create_temporary_root()
  expect_false(file.exists(file.path(root$path, ".outpack", "r", "git_ok")))
  expect_silent(root_check_git(root, NULL))
  expect_false(file.exists(file.path(root$path, ".outpack", "r", "git_ok")))
  expect_false(file.exists(file.path(root$path, ".gitignore")))
})


test_that("can add gitignore if git setup is ok, but not present", {
  root <- create_temporary_root()
  info <- helper_add_git(root$path)
  fs::file_delete(file.path(root$path, ".gitignore"))
  expect_false(file.exists(file.path(root$path, ".outpack", "r", "git_ok")))
  expect_message(root_check_git(root, NULL), "Wrote '.gitignore'")
  expect_true(file.exists(file.path(root$path, ".outpack", "r", "git_ok")))
  expect_true(file.exists(file.path(root$path, ".gitignore")))
})


test_that("can error with instructions if files are added to git", {
  ## Make sure that these are never set for the tests
  withr::local_options(
    orderly_git_error_is_warning = NULL,
    orderly_git_error_ignore = NULL)

  root <- create_temporary_root()
  info <- helper_add_git(root$path)
  id <- create_random_packet(root$path)

  ## Need to do some work here to make this fail now:
  fs::file_delete(file.path(root$path, ".gitignore"))
  fs::file_delete(file.path(root$path, ".outpack", "r", "git_ok"))

  gert::git_add(".", repo = root$path)
  user <- "author <author@example.com>"
  gert::git_commit("add everything", author = user, committer = user,
                   repo = root$path)
  err <- expect_error(root_check_git(root, NULL),
                      "Detected \\d+ outpack files committed to git")
  expect_false(file.exists(file.path(root$path, ".outpack", "r", "git_ok")))
  expect_equal(err$body[[1]],
               "Detected files were found in '.outpack/' and 'archive/'")
  expect_match(err$body[[2]],
               "For tips on resolving this, please see .+troubleshooting.html")
  expect_match(err$body[[3]], "^Found: ")
  expect_match(err$body[[4]],
               "To turn this into a warning and continue anyway")

  expect_error(create_random_packet(root$path), err$message, fixed = TRUE)

  path_ok <- file.path(root$path, ".outpack", "r", "git_ok")

  ## Can ignore the warning entirely:
  expect_silent(
    withr::with_options(list(orderly_git_error_ignore = TRUE),
                        root_check_git(root, NULL)))

  withr::with_options(
    list(orderly_git_error_is_warning = TRUE),
    expect_warning(id1 <- create_random_packet(root$path),
                   err$message, fixed = TRUE))
  expect_type(id1, "character")

  withr::with_options(
    list(orderly_git_error_is_warning = TRUE),
    expect_warning(id2 <- create_random_packet(root$path), NA)) # no warning

  expect_type(id2, "character")

  expect_false(file_exists(path_ok))
})


test_that("can do git check in subdir", {
  ## Make sure that these are never set for the tests
  withr::local_options(
    orderly_git_error_is_warning = NULL,
    orderly_git_error_ignore = NULL)

  path <- withr::local_tempdir()
  root <- file.path(path, "root")
  suppressMessages(orderly_init(root))

  info <- helper_add_git(path)

  expect_warning(
    root_check_git(list(path = root), NULL),
    "Can't check if files are correctly gitignored")
})


test_that("can identify a plain source root", {
  info <- test_prepare_orderly_example_separate("explicit")
  expect_equal(normalise_path(orderly_src_root(info$src, FALSE)),
               normalise_path(info$src))
  expect_equal(
    orderly_src_root(file.path(info$src, "src", "explicit"), TRUE),
    orderly_src_root(info$src, FALSE))
  expect_error(
    orderly_src_root(file.path(info$src, "src", "explicit"), FALSE),
    "Did not find existing orderly source root in")

  p <- file.path(info$outpack, "a", "b", "c")
  fs::dir_create(p)

  err <- expect_error(
    orderly_src_root(info$outpack, FALSE),
    "Did not find existing orderly source root in")
  expect_equal(err$body, c(i = "Expected to find file 'orderly_config.yml'"))

  err <- expect_error(
    orderly_src_root(p, TRUE),
    "Did not find existing orderly source root in")
  expect_equal(err$body,
               c(i = "Expected to find file 'orderly_config.yml'",
                 i = "Looked in parents of this path without success"))
})


test_that("can identify a plain source root from a full root", {
  path <- test_prepare_orderly_example("explicit")
  root <- root_open(path)
  expect_equal(orderly_src_root(root$path, FALSE), root$path)
  expect_equal(orderly_src_root(root, FALSE), root$path)
})


test_that("can use ORDERLY_ROOT to control the working directory", {
  a <- create_temporary_root()
  b <- create_temporary_root()
  c <- create_temporary_root()
  path_a <- normalise_path(a$path)
  path_b <- normalise_path(b$path)
  path_c <- normalise_path(b$path)

  withr::with_envvar(c(ORDERLY_ROOT = NA_character_), {
    withr::with_dir(path_a, {
      expect_equal(root_open(NULL)$path, path_a)
      expect_equal(root_open(path_b)$path, path_b)
    })
  })

  withr::with_envvar(c(ORDERLY_ROOT = path_c), {
    withr::with_dir(path_a, {
      expect_equal(root_open(NULL)$path, path_c)
      expect_equal(root_open(path_b)$path, path_b)
    })
  })
})
