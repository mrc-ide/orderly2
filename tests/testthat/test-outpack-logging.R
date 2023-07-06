test_that("can log creation of outpack repo", {
  path <- temp_file()
  expect_message(
    root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE),
    "[ init       ]",
    fixed = TRUE)
})


test_that("can enable console logging for packet level, if disabled at root", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)

  path_src <- create_temporary_simple_src()
  msg <- capture_messages(
    p <- outpack_packet_start(path_src, "example", root = root,
                              logging_console = TRUE))

  expect_false(root$logger$console)
  expect_true(p$logger$console)

  expect_length(msg, 3)
  expect_equal(msg[[1]], "[ name       ]  example\n")
  expect_equal(msg[[2]], sprintf("[ id         ]  %s\n", p$id))
  expect_match(msg[[3]], "^\\[ start")
})


test_that("can disable console logging for packet level, if enabled at root", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  outpack_config_set(logging.console = TRUE, root = root)

  path_src <- create_temporary_simple_src()
  msg <- capture_messages(
    p <- outpack_packet_start(path_src, "example", root = root,
                              logging_console = FALSE))
  expect_equal(msg, character())

  expect_true(root$logger$console)
  expect_false(p$logger$console)
})


test_that("can log basic packet running", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  outpack_config_set(logging.console = TRUE, root = root)

  path_src <- create_temporary_simple_src()

  inputs <- c("data.csv", "script.R")
  env <- new.env()

  msg <- capture_messages(
    p <- outpack_packet_start(path_src, "example", root = root))

  expect_length(msg, 3)
  expect_equal(msg[[1]], "[ name       ]  example\n")
  expect_equal(msg[[2]], sprintf("[ id         ]  %s\n", p$id))
  expect_match(msg[[3]], "^\\[ start")

  res <- evaluate_promise(
    outpack_packet_run(p, "script.R", env))
  expect_equal(res$messages,
               c("[ script     ]  script.R\n", "[ result     ]  success\n"))

  expect_match(res$output, "read.csv('data.csv')", fixed = TRUE)

  msg <- capture_messages(outpack_packet_end(p))

  expect_length(msg, 2)
  expect_match(msg[[1]], "^\\[ end")
  expect_match(msg[[2]], "^\\[ elapsed")

  expect_true(file.exists(file.path(path_src, "log.json")))
  dat <- log_read(file.path(path_src, "log.json"))

  expect_equal(nrow(dat), 8)
  expect_equal(
    dat$topic,
    c("name", "id", "start", "script", "result", "output", "end", "elapsed"))
  expect_equal(
    dat$caller,
    paste0("orderly2::outpack_packet_",
           rep(c("start", "run", "end"), c(3, 3, 2))))
  expect_equal(dat$log_level, rep("info", 8))
  expect_s3_class(dat$time, "POSIXct")
  expect_true(is.list(dat$detail))
  expect_match(
    dat$detail[[6]][[1]],
    "read.csv('data.csv')",
    fixed = TRUE)
})


test_that("can change threshold when running a packet", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)

  path_src <- create_temporary_simple_src()
  msg <- capture_messages(
    p <- outpack_packet_start(path_src, "example", root = root,
                              logging_threshold = "trace"))
  expect_equal(p$logger$threshold, "trace")
})


test_that("can safely deserialise logs that would otherwise simplify", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)

  path_src <- create_temporary_simple_src()
  p <- outpack_packet_start(path_src, "example", root = root)
  outpack_packet_end(p)
  dat <- log_read(file.path(path_src, "log.json"))
  expect_true(is.list(dat$detail))
  expect_equal(lengths(dat$detail), rep(1, 5))
})


test_that("sensible handling of parameter vectors", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path_src <- create_temporary_simple_src()
  msg <- capture_messages(
    p <- outpack_packet_start(path_src, "example",
                              parameters = list(a = 1, b = 2),
                              root = root, logging_console = TRUE))
  expect_length(msg, 4)
  expect_equal(msg[[1]], "[ name       ]  example\n")
  expect_equal(msg[[2]], sprintf("[ id         ]  %s\n", p$id))
  expect_equal(msg[[3]], "[ parameter  ]  a: 1\n[ ...        ]  b: 2\n")
  expect_match(msg[[4]], "^\\[ start")
})


test_that("control logging threshold", {
  object <- structure(
    list(logger = list(console = TRUE, threshold = "info")),
    class = "outpack_root")
  expect_message(
    outpack_log_info(object, "hello", "info", "test"),
    "[ hello      ]  info", fixed = TRUE)
  expect_silent(
    outpack_log_debug(object, "hello", "debug", "test"))
  expect_silent(
    outpack_log_trace(object, "hello", "trace", "test"))

  object <- structure(
    list(logger = list(console = TRUE, threshold = "debug")),
    class = "outpack_root")
  expect_message(
    outpack_log_info(object, "hello", "info", "test"),
    "[ hello      ]  info", fixed = TRUE)
  expect_message(
    outpack_log_debug(object, "hello", "debug", "test"),
    "[ hello      ]  debug", fixed = TRUE)
  expect_silent(
    outpack_log_trace(object, "hello", "trace", "test"))

  object <- structure(
    list(logger = list(console = TRUE, threshold = "trace")),
    class = "outpack_root")
  expect_message(
    outpack_log_info(object, "hello", "info", "test"),
    "[ hello      ]  info", fixed = TRUE)
  expect_message(
    outpack_log_debug(object, "hello", "debug", "test"),
    "[ hello      ]  debug", fixed = TRUE)
  expect_message(
    outpack_log_trace(object, "hello", "trace", "test"),
    "[ hello      ]  trace", fixed = TRUE)
})


test_that("reject logging calls with invalid object", {
  expect_error(outpack_log_info(NULL, "hello", "error", "test"),
               "Invalid call to outpack_log")
})
