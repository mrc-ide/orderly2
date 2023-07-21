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
                                extract = c(i = "parameters.i is number"),
                                root = root)
  expect_setequal(names(d), c("id", "i"))
  expect_equal(d$id, ids)
  expect_equal(d$i, as.numeric(1:5))
})


test_that("raise sensible error on type assertion failure", {
  root <- create_temporary_root()
  ids <- vcapply(1:5, function(i) {
    create_random_packet(root, parameters = list(i = i))
  })

  err <- expect_error(
    outpack_metadata_extract('name == "data"', root = root,
                             extract = c(i = "parameters.i is string")),
    "Expected all values of 'parameters.i' to be strings (or NULL)",
    fixed = TRUE)
  expect_equal(
    err$body,
    set_names(c(sprintf("Found `%d` (a number) for packet '%s'", 1:3, ids[1:3]),
                "(...and 2 more)"),
              rep("i", 4)))
})


test_that("can extract from time", {
  root <- create_temporary_root()
  ids <- vcapply(1:5, function(i) {
    create_random_packet(root, parameters = list(i = i))
  })
  meta <- lapply(ids, outpack_metadata, root = root)

  d <- outpack_metadata_extract('name == "data"',
                                extract = c("name", "time"),
                                root = root)
  expect_setequal(names(d), c("id", "name", "time"))
  expect_equal(d$id, ids)
  expect_equal(d$name, rep("data", 5))
  expect_equal(d$time, I(lapply(meta, "[[", "time")))

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


test_that("can extract files metadata", {
  root <- create_temporary_root()
  ids <- vcapply(1:5, function(i) {
    create_random_packet(root, parameters = list(i = i))
  })

  meta <- lapply(ids, outpack_metadata, root = root)

  d <- outpack_metadata_extract('name == "data"',
                                extract = "files",
                                root = root)
  expect_setequal(names(d), c("id", "files"))
  expect_equal(d$id, ids)
  expect_equal(d$files, I(lapply(meta, "[[", "files")))

  d <- outpack_metadata_extract('name == "data"',
                                extract = c("files" = "files.path"),
                                root = root)
  expect_setequal(names(d), c("id", "files"))
  expect_equal(d$id, ids)
  expect_equal(d$files, I(lapply(meta, function(x) x$files$path)))
})


test_that("can extract git metadata", {
  ## See test-outpack-git.R for this example
  root <- create_temporary_root()
  path_src <- create_temporary_simple_src()

  info <- helper_add_git(path_src)

  p <- outpack_packet_start(path_src, "example", root = root)
  id <- p$id
  outpack_packet_run(p, "script.R")
  outpack_packet_end(p)

  meta <- outpack_root_open(root$path)$metadata(id, TRUE)

  d <- outpack_metadata_extract('name == "example"',
                                extract = "git",
                                root = root)
  expect_equal(d$git, I(list(meta$git)))

  d <- outpack_metadata_extract('name == "example"',
                                extract = "git.sha",
                                root = root)
  expect_equal(d$git_sha, meta$git$sha)
  expect_type(d$git_sha, "character")
})


test_that("fill in types for git data when missing", {
  root <- create_temporary_root()
  ids <- vcapply(1:5, function(i) {
    create_random_packet(root, parameters = list(i = i))
  })

  d <- outpack_metadata_extract('name == "data"',
                                extract = "git",
                                root = root)
  expect_equal(d$git, I(vector("list", 5)))

  d <- outpack_metadata_extract('name == "data"',
                                extract = "git.sha",
                                root = root)
  expect_equal(d$git_sha, rep(NA_character_, 5))
})


test_that("can extract orderly metadata", {
  path <- test_prepare_orderly_example("parameters")
  env <- new.env()
  ids <- vcapply(1:3, function(i) {
    orderly_run("parameters", root = path, envir = env,
                parameters = list(a = i, b = 20, c = 30))
  })

  expect_equal(
    outpack_metadata_extract('name == "parameters"', extract = "script",
                             root = path)$script,
    I(as.list(rep("orderly.R", 3))))
  expect_equal(
    outpack_metadata_extract('name == "parameters"',
                             extract = c(script = "script is string"),
                             root = path)$script,
    rep("orderly.R", 3))

  id_extra <- create_random_packet(path, "parameters")
  expect_equal(
    outpack_metadata_extract('name == "parameters"', extract = "script",
                             root = path)$script,
    I(list("orderly.R", "orderly.R", "orderly.R", character())))

  err <- expect_error(
    outpack_metadata_extract('name == "parameters"',
                             extract = c(script = "script is string"),
                             root = path)$script,
    "Expected all values of 'script' to evaluate to a scalar (if not NULL)",
    fixed = TRUE)
  expect_equal(err$body,
               c(i = sprintf("Value for %s has length 0", id_extra)))
})


test_that("can extract orderly custom metadata", {
  ## This is example in the docs
  path <- test_prepare_orderly_example("description")
  env <- new.env()
  id <- orderly_run("description", root = path, envir = env)
  root <- orderly_root(path, FALSE)
  d <- outpack_metadata_extract(
    'name == "description"',
    extract = c(display = "custom.orderly.description.display is string"),
    root = path)
  expect_equal(d, data_frame(id = id, display = "Packet with description"))
})


test_that("can extract session metadata", {
  path <- test_prepare_orderly_example("parameters")
  env <- new.env()
  ids <- vcapply(1:3, function(i) {
    orderly_run("parameters", root = path, envir = env,
                parameters = list(a = i, b = 20, c = 30))
  })
  meta <- lapply(ids, outpack_metadata, root = path)

  d <- outpack_metadata_extract(
    'name == "parameters"',
    extract = c("session" = "custom.orderly.session"),
    root = path)
  expect_equal(d$session,
               I(lapply(meta, "[[", c("custom", "orderly", "session"))))

  d <- outpack_metadata_extract(
    'name == "parameters"',
    extract = c(version = "custom.orderly.session.platform.version is string"),
    root = path)
  v <- meta[[1]]$custom$orderly$session$platform$version
  expect_equal(d, data_frame(id = ids, version = v))
  expect_type(d$version, "character")
})


test_that("can pass a vector of ids through", {
  root <- create_temporary_root()
  ids <- vcapply(1:5, function(i) {
    create_random_packet(root, parameters = list(i = i))
  })

  expect_identical(
    outpack_metadata_extract(ids, root = root),
    outpack_metadata_extract('name == "data"', root = root))
})


test_that("validate extraction", {
  expect_equal(
    parse_extract(NULL),
    data_frame(from = I(list("name", "parameters")),
               to = c("name", "parameters"),
               is = NA_character_))
  expect_equal(
    parse_extract(c("a", "b")),
    data_frame(from = I(list("a", "b")),
               to = c("a", "b"),
               is = NA_character_))
  expect_equal(
    parse_extract(c("a.x", "b.y.z")),
    data_frame(from = I(list(c("a", "x"), c("b", "y", "z"))),
               to = c("a_x", "b_y_z"),
               is = NA_character_))
  expect_equal(
    parse_extract(c(a = "a.x", "b.y.z")),
    data_frame(from = I(list(c("a", "x"), c("b", "y", "z"))),
               to = c("a", "b_y_z"),
               is = NA_character_))
  expect_equal(
    parse_extract(c(a = "a.x is string", "b.y.z")),
    data_frame(from = I(list(c("a", "x"), c("b", "y", "z"))),
               to = c("a", "b_y_z"),
               is = c("string", NA_character_)))
  expect_equal(
    parse_extract(c("a is string", "b.y.z is number")),
    data_frame(from = I(list("a", c("b", "y", "z"))),
               to = c("a", "b_y_z"),
               is = c("string", "number")))

  expect_error(
    parse_extract(c("id", "a", "b")),
    "Don't use 'id' as a column to extract; this column is always added")
  err <- expect_error(
    parse_extract(c("a", "b", "a", "b", "c")),
    "All destination columns in 'extract' must be unique")
  expect_equal(err$body, c(x = "Duplicated names: 'a', 'b'"))

  err <- expect_error(
    parse_extract(c("a is number", "b is char", "c is bool")),
    "Invalid conversion type 'char', 'bool' requested in 'extract'")
  expect_equal(
    err$body,
    c(i = "'is' must be one of 'string', 'number', 'boolean', 'list'",
      x = "Extraction of 'b' used type 'char'",
      x = "Extraction of 'c' used type 'bool'"))
})


## This is only used to construc nice error messages
test_that("helper converts types correctly", {
  expect_equal(storage_mode_scalar(num_to_time(1)), "time")

  expect_equal(storage_mode_scalar(TRUE), "boolean")
  expect_equal(storage_mode_scalar(NA), "boolean")

  expect_equal(storage_mode_scalar(1L), "number")
  expect_equal(storage_mode_scalar(NA_integer_), "number")
  expect_equal(storage_mode_scalar(1), "number")
  expect_equal(storage_mode_scalar(NA_real_), "number")

  expect_equal(storage_mode_scalar("str"), "string")
  expect_equal(storage_mode_scalar(NA_character_), "string")

  expect_equal(storage_mode_scalar(list()), "list")
})


test_that("sensible behaviour if extracting nonsense", {
  root <- create_temporary_root()
  ids <- vcapply(1:5, function(i) {
    create_random_packet(root, parameters = list(i = i))
  })
  d <- outpack_metadata_extract('name == "data"',
                                extract = c(a = "a.b.c.d"),
                                root = root)
  expect_equal(d$a, I(vector("list", 5)))

  d <- outpack_metadata_extract('name == "data"',
                                extract = c(a = "a.b.c.d is string"),
                                root = root)
  expect_equal(d$a, rep(NA_character_, 5))
})
