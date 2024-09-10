test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("drop null can return corner case", {
  expect_null(drop_null(list(), NULL))
  expect_null(drop_null(list(NULL), NULL))
  expect_null(drop_null(list(NULL, NULL), NULL))
  expect_equal(drop_null(list(NULL, NULL), list()), list())
})


test_that("drop_null can filter list", {
  expect_equal(drop_null(list("x", NULL)), list("x"))
  expect_equal(drop_null(list("x", "y")), list("x", "y"))
  expect_equal(drop_null(list("x", NULL, "y")), list("x", "y"))
})


test_that("assert_simple_scalar_atomic rejects non-atomic entries", {
  value <- list(1)
  expect_error(assert_simple_scalar_atomic(value),
               "'value' must be atomic (string, numeric, logical)",
               fixed = TRUE)
  expect_silent(assert_simple_scalar_atomic(1))
})


test_that("can resolve environment variables", {
  variables <- c(V1 = "a", V2 = "b")
  expect_equal(
    resolve_envvar(list(A = "$V1", B = "$V2", C = "c"), variables, "x"),
    list(A = "a", B = "b", C = "c"))
  expect_equal(
    resolve_envvar(list(list(A = "$V1", B = "$V2", C = "c")), variables, "x"),
    list(list(A = "a", B = "b", C = "c")))
  expect_equal(
    resolve_envvar(list(list(A = list(a = "$V1"),
                             B = list(b = "$V2"),
                             C = list(c = "c"))), variables, "x"),
    list(list(A = list(a = "a"), B = list(b = "b"), C = list(c = "c"))))
})


test_that("can return meaningful errors if lookup fails", {
  variables <- c(V1 = "a", V2 = "b")
  expect_error(
    resolve_envvar(list(A = "$V3", B = "$V2", C = "c"), variables, "x"),
    "Environment variable 'V3' is not set\n\t(used in x$A)",
    fixed = TRUE)
  expect_error(
    resolve_envvar(list(list(A = "$V3", B = "$V2", C = "c")), variables, "x"),
    "Environment variable 'V3' is not set\n\t(used in x[[1]]$A)",
    fixed = TRUE)
  expect_error(
    resolve_envvar(list(list(A = list(a = "$V1"),
                             B = list(b = "$V3"),
                             C = list(c = "c"))), variables, "x"),
    "Environment variable 'V3' is not set\n\t(used in x[[1]]$B$b)",
    fixed = TRUE)
})


test_that("can ignore errors and substitute in NA values", {
  variables <- c(V1 = "a", V2 = "b")
  expect_equal(
    resolve_envvar(list(A = "$V3", B = "$V2", C = "c"), variables, "x", FALSE),
    list(A = NA_character_, B = "b", C = "c"))
  expect_equal(
    resolve_envvar(list(list(A = "$V3", B = "$V2", C = "c")),
                   variables, "x", FALSE),
    list(list(A = NA_character_, B = "b", C = "c")))
  expect_equal(
    resolve_envvar(list(list(A = list(a = "$V1"),
                             B = list(b = "$V3"),
                             C = list(c = "c"))), variables, "x", FALSE),
    list(list(A = list(a = "a"),
              B = list(b = NA_character_),
              C = list(c = "c"))))
})


test_that("yaml_read throws nicely", {
  expect_error(suppressWarnings(yaml_read("foo")), "while reading 'foo'")
})


test_that("sys_getenv", {
  withr::with_envvar(
    c("SOME_VAR" = NA_character_), {
      expect_error(
        sys_getenv("SOME_VAR", "loc"),
        "Environment variable 'SOME_VAR' is not set.*used in loc")
      expect_null(sys_getenv("SOME_VAR", "loc", FALSE))
      expect_identical(sys_getenv("SOME_VAR", "loc", FALSE, NA_character_),
                       NA_character_)
    })

  ## On windows if environment variable is empty then windows will
  ## return NA from call to Sys.getenv
  if (is_windows()) {
    expected_err <- "Environment variable 'SOME_VAR' is not set.*used in loc"
  } else {
    expected_err <- "Environment variable 'SOME_VAR' is empty.*used in loc"
  }

  withr::with_envvar(
    c("SOME_VAR" = ""),
    expect_error(
      sys_getenv("SOME_VAR", "loc"),
      expected_err))
  withr::with_envvar(
    c("SOME_VAR" = "x"),
    expect_identical(sys_getenv("SOME_VAR", "loc"), "x"))
})


test_that("check fields", {
  expect_silent(check_fields(list(), "x", character(), character()))

  expect_silent(check_fields(list(a = 1), "x", "a", character()))
  expect_silent(check_fields(list(a = 1), "x", character(), "a"))

  expect_silent(check_fields(list(a = 1, b = 1), "x", "a", "b"))
  expect_silent(check_fields(list(a = 1, b = 1), "x", c("a", "b"), character()))
  expect_silent(check_fields(list(a = 1, b = 1), "x", character(), letters))

  expect_error(
    check_fields(list(a = 1, b = 2), "x", c("a", "b", "c"), character()),
    "Fields missing from x: c")
  expect_error(
    check_fields(list(a = 1, b = 2), "x", c("a", "b", "c", "d"), character()),
    "Fields missing from x: c, d")

  expect_error(
    check_fields(list(a = 1, b = 2), "x", "a", character()),
    "Unknown fields in x: b")
  expect_error(
    check_fields(list(a = 1, b = 2, c = 3, d = 4), "x", "a", "b"),
    "Unknown fields in x: c, d")
})


test_that("Descend failure", {
  path <- temp_file()
  dir.create(path)
  expect_null(find_file_descend(".orderly_foobar", tempdir(), path))
  expect_null(find_file_descend(".orderly_foobar", "/", path))
  expect_null(find_file_descend(".orderly_foobar", "/", "/"))
})


test_that("can get near matches", {
  x <- c("apples", "applez", "appell", "applly")
  expect_equal(
    near_match("apple", x),
    c("apples", "applez", "appell", "applly"))
  expect_equal(
    near_match("apple", x, 1),
    c("apples", "applez"))
  expect_equal(
    near_match("apple", x, 2, 3),
    c("apples", "applez", "appell"))
})


test_that("validate namespaced symbol strings", {
  expect_equal(check_symbol_from_str("a::b", "x"),
               list(namespace = "a", symbol = "b"))
  expect_error(check_symbol_from_str("b", "x"),
               "Expected fully qualified name for 'x'")
  expect_error(check_symbol_from_str("a:::b", "x"),
               "Expected fully qualified name for 'x'")
  expect_error(check_symbol_from_str("a::b::c", "x"),
               "Expected fully qualified name for 'x'")
})


test_that("can perform simple string interpolation", {
  envir <- list2env(list(a = 1, b = "banana", c = "carrot"),
                    parent = emptyenv())

  expect_equal(string_interpolate_simple("hello", envir), "hello")
  expect_equal(string_interpolate_simple("${hello", envir), "${hello")
  expect_equal(string_interpolate_simple("a ${b} c", envir),
               "a banana c")
  expect_equal(string_interpolate_simple("a ${b} ${c}", envir),
               "a banana carrot")
  expect_equal(string_interpolate_simple("a ${b} ${ b }", envir),
               "a banana banana")

  expect_equal(string_interpolate_simple(
    c("${a}/${b}", "${a}/${c}"), envir),
    c("1/banana", "1/carrot"))
  expect_equal(string_interpolate_simple(
    I(c("${a}/${b}", "${a}/${c}")), envir),
    I(c("${a}/${b}", "${a}/${c}")))
})


test_that("prevent problematic string interpolations", {
  envir <- list2env(
    list(a = NULL, b = "${a}", c = letters, d = "${d}", f = args),
    parent = emptyenv())

  err <- expect_error(
    string_interpolate_simple("a/${b}/c", envir),
    "Can't perform recursive string interpolation")
  expect_equal(
    err$body,
    c(x = "Tried to substitute '${b}' to '${a}'",
      i = "Was interpolating string 'a/${b}/c'",
      i = "Don't use '${...}' within the values you are substituting to"))

  err <- expect_error(
    string_interpolate_simple("a/${b}/${d}", envir),
    "Can't perform recursive string interpolation")
  expect_equal(
    err$body,
    c(x = "Tried to substitute '${b}' to '${a}'",
      x = "Tried to substitute '${d}' to '${d}'",
      i = "Was interpolating string 'a/${b}/${d}'",
      i = "Don't use '${...}' within the values you are substituting to"))

  err <- expect_error(
    string_interpolate_simple("a/${a}", envir),
    "Failed to convert string interpolation variable to string")
  expect_equal(
    err$body,
    c(x = "Failed when retrieving 'a' which has length 0",
      i = "Was interpolating string 'a/${a}'",
      i = "All values in ${...} must refer to strings"))

  err <- expect_error(
    string_interpolate_simple("a/${x}", envir),
    "Failed to find value for 'x'")
  expect_equal(err$body, c("i" = "Was interpolating string 'a/${x}'"))

  err <- expect_error(
    string_interpolate_simple("a/${f}", envir),
    "Failed to convert 'f' to character")
  msg <- tryCatch(as.character(args), error = identity)
  expect_equal(err$body,
               c(x = msg$message,
                 "i" = "Was interpolating string 'a/${f}'"))
})


test_that("pairs generates pairs", {
  expect_equal(pairs(c("a", "b")), list(c("a", "b")))
  expect_equal(pairs(c("a", "b", "c")),
               list(c("a", "b"), c("a", "c"), c("b", "c")))
  expect_equal(pairs(c("a", "b", "c", "d")),
               list(c("a", "b"), c("a", "c"), c("b", "c"),
                    c("a", "d"), c("b", "d"), c("c", "d")))
})


test_that("can print pretty bytes", {
  expect_equal(pretty_bytes(100), "100 B")
  expect_equal(pretty_bytes(5000), "5 kB")
  expect_equal(pretty_bytes(5123), "5.1 kB")
  expect_equal(pretty_bytes(5000000), "5 MB")
  expect_equal(pretty_bytes(5123456), "5.1 MB")
  expect_equal(pretty_bytes(5000000000), "5,000 MB")
  expect_equal(pretty_bytes(5123456789), "5,123.5 MB")
})


test_that("set_names copes with common pathologies", {
  expect_equal(set_names(character(), "x"),
               structure(character(), names = character()))
  expect_equal(set_names("a", "x"),
               c("x" = "a"))
  expect_equal(set_names(c("a", "b"), "x"),
               c("x" = "a", x = "b"))
  expect_equal(set_names(c("a", "b"), c("x", "y")),
               c("x" = "a", y = "b"))
  expect_null(set_names(NULL, "x"))
})


test_that("can collapse with special last case", {
  x <- c("x", "y", "z")
  expect_equal(collapse(x), "x, y, z")
  expect_equal(collapse(x, " or "), "x, y or z")
  expect_equal(collapse(x[1:2], " or "), "x or y")
  expect_equal(collapse(x[1], " or "), "x")
  expect_equal(collapse(x[0], " or "), "")
})


test_that("can gracefully cope with rds save failure", {
  mock_move <- mockery::mock(stop("some error"), cycle = TRUE)
  mockery::stub(saverds_atomic, "fs::file_move", mock_move)
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "file.rds")
  expect_silent(
    saverds_atomic(NULL, path, allow_fail = TRUE))
  expect_equal(dir(tmp), character())
  expect_error(
    saverds_atomic(NULL, path, allow_fail = FALSE),
    "some error")
  expect_equal(dir(tmp), character())
})


test_that("read_string strips newlines", {
  path <- tempfile()
  writeLines(c("", "12345678"), path)

  # 8 characters, a leading newline and a trailing one
  # Each newline may be one of two bytes each, depending on the platform.
  expect_gte(file.info(path)$size, 10)

  result <- expect_silent(read_string(path))
  expect_equal(result, "12345678")
})


<<<<<<< HEAD
describe("expand_dirs_virtual", {
  files <- list(
    "d1" = c("f2", "f3"),
    "d2" = c("f4", "d3/f5"),
    "d2/d3" = c("f5"))
  is_dir <- function(p) p %in% c("d1", "d2", "d2/d3")
  list_files <- function(p) files[[p]]

  check <- function(object, expected) {
    expect_equal(expand_dirs_virtual(object, is_dir, list_files), expected)
  }

  it("accepts a character vector", {
    check("f1", "f1")
    check("d1", c("d1/f2", "d1/f3"))
    check(c("f1", "d1"), c("f1", "d1/f2", "d1/f3"))
    check("d2", c("d2/f4", "d2/d3/f5"))
  })

  it("accepts a dataframe", {
    check(
      data_frame(here = "g1", there = "f1"),
      data_frame(here = "g1", there = "f1"))

    check(
      data_frame(here = "dest", there = "d1"),
      data_frame(here = c("dest/f2", "dest/f3"),
                 there = c("d1/f2", "d1/f3")))

    check(
      data_frame(here = c("g1", "dest"), there = c("f1", "d1")),
      data_frame(here = c("g1", "dest/f2", "dest/f3"),
                 there = c("f1", "d1/f2", "d1/f3")))

    check(
      data_frame(here = "dest", there = "d2"),
      data_frame(here = c("dest/f4", "dest/d3/f5"),
                 there = c("d2/f4", "d2/d3/f5")))

    check(
      data_frame(here = c("foo", "bar"), there = c("d2", "d2")),
      data_frame(here = c("foo/f4", "foo/d3/f5", "bar/f4", "bar/d3/f5"),
                 there = c("d2/f4", "d2/d3/f5", "d2/f4", "d2/d3/f5")))
  })
})


describe("expand_dirs", {
  p <- withr::local_tempdir()
  files <- c("f1", "d1/f2", "d1/f3", "d2/f4", "d2/d3/f5")
  fs::dir_create(fs::path(p, unique(dirname(files))))
  fs::file_create(fs::path(p, files))

  it("accepts a character vector", {
    check <- function(object, expected) {
      expect_setequal(expand_dirs(object, p), expected)
    }

    check("f1", "f1")
    check("d1", c("d1/f2", "d1/f3"))
    check(c("f1", "d1"), c("f1", "d1/f2", "d1/f3"))
    check("d2", c("d2/f4", "d2/d3/f5"))
  })

  it("accepts a dataframe", {
    check <- function(object, expected) {
      result <- expand_dirs(object, p)
      # This compares the dataframes ignoring the order, which is not
      # deterministic
      expect_setequal(unname(split(result, seq_len(nrow(result)))),
                      unname(split(expected, seq_len(nrow(expected)))))
    }

    check(
      data_frame(here = "g1", there = "f1"),
      data_frame(here = "g1", there = "f1"))

    check(
      data_frame(here = "dest", there = "d1"),
      data_frame(here = c("dest/f2", "dest/f3"), there = c("d1/f2", "d1/f3")))

    check(
      data_frame(here = c("g1", "dest"), there = c("f1", "d1")),
      data_frame(here = c("g1", "dest/f2", "dest/f3"),
                 there = c("f1", "d1/f2", "d1/f3")))

    check(
      data_frame(here = "dest", there = "d2"),
      data_frame(here = c("dest/f4", "dest/d3/f5"),
                 there = c("d2/f4", "d2/d3/f5")))

    check(
      data_frame(here = c("foo", "bar"), there = c("d2", "d2")),
      data_frame(here = c("foo/f4", "foo/d3/f5", "bar/f4", "bar/d3/f5"),
                 there = c("d2/f4", "d2/d3/f5", "d2/f4", "d2/d3/f5")))
  })
})


test_that("fill_missing_names works", {
  expect_equal(fill_missing_names(NULL), NULL)
  expect_equal(fill_missing_names(c("a", "b")), c(a = "a", b = "b"))
  expect_equal(fill_missing_names(c("a", "a")), c(a = "a", a = "a"))
  expect_equal(fill_missing_names(c(x = "a", "a")), c(x = "a", a = "a"))
})


test_that("parse_json accepts literal string", {
  expect_equal(parse_json('{ "x": 1 }'), list(x = 1))
  expect_equal(parse_json("null"), NULL)
})


test_that("parse_json accepts files", {
  f <- withr::local_tempfile()
  writeLines('{ "x": 1 }', f)
  expect_equal(parse_json(file(f)), list(x = 1))
})


test_that("parse_json is not confused by ambiguity", {
  withr::with_dir(withr::local_tempdir(), {
    writeLines('{ "contents": "hello" }', "true")
    writeLines('{ "contents": "world" }', "4")
    expect_equal(parse_json("true"), TRUE)
    expect_equal(parse_json("4"), 4)
    expect_equal(parse_json(file("true")), list(contents = "hello"))
    expect_equal(parse_json(file("4")), list(contents = "world"))
  })
})


test_that("parse_json includes file path in its errors", {
  f <- file.path(withr::local_tempdir(), "contents.json")
  writeLines("bad json", f)
  expect_error(parse_json(file(f)),
               "Error while reading .*contents.json.*lexical error")
})


test_that("parse_json includes name argument in its errors", {
  expect_error(parse_json("bad json", name = "my file"),
               "Error while reading my file.*lexical error")
})


describe("copy_files", {
  src1 <- withr::local_tempfile()
  src2 <- withr::local_tempfile()
  writeLines("Hello", src1)
  writeLines("World", src2)

  it("can copy one file", {
    dst <- file.path(withr::local_tempdir(), "destination.txt")

    copy_files(src1, dst)
    expect_equal(readLines(dst), "Hello")
  })


  it("can copy multiple files", {
    d <- withr::local_tempdir()
    dst1 <- file.path(d, "destination1.txt")
    dst2 <- file.path(d, "destination2.txt")

    copy_files(c(src1, src2), c(dst1, dst2))
    expect_equal(readLines(dst1), "Hello")
    expect_equal(readLines(dst2), "World")
  })


  it("can copy zero files", {
    expect_no_error(copy_files(character(0), character(0)))
    expect_no_error(copy_files(character(0), character(0), overwrite = TRUE))
    expect_no_error(copy_files(character(0), character(0), make_writable = TRUE))
  })


  it("creates parent directories", {
    d <- withr::local_tempdir()
    dst <- file.path(d, "path", "to", "destination.txt")

    expect_false(fs::dir_exists(file.path(d, "path", "to")))
    copy_files(src1, dst)
    expect_true(fs::dir_exists(file.path(d, "path", "to")))
  })


  it("can copy a read-only file", {
    d <- withr::local_tempdir()
    src <- file.path(d, "source.txt")
    dst <- file.path(d, sprintf("destination%d.txt", 1:4))

    writeLines("Hello", src)
    fs::file_chmod(src, "a-w")

    copy_files(src, dst[[1]])
    copy_files(src, dst[[2]], overwrite = TRUE)
    copy_files(src, dst[[3]], make_writable = TRUE)
    copy_files(src, dst[[4]], make_writable = TRUE, overwrite = TRUE)

    expect_true(all(fs::file_access(dst, mode="read")))
    expect_equal(unname(fs::file_access(dst, mode="write")),
                 c(FALSE, FALSE, TRUE, TRUE))
  })


  it("can overwrite an existing file", {
    d <- withr::local_tempdir()
    dst <- file.path(d, "destination.txt")

    fs::file_create(dst)

    expect_error(copy_files(src1, dst), "file already exists")

    copy_files(src1, dst, overwrite = TRUE)
    expect_equal(readLines(dst), "Hello")
  })


  it("overwrites a single file out of two", {
    d <- withr::local_tempdir()
    dst1 <- file.path(d, "destination1.txt")
    dst2 <- file.path(d, "destination2.txt")

    fs::file_create(dst1)
    copy_files(c(src1, src2), c(dst1, dst2), overwrite = TRUE)

    expect_equal(readLines(dst1), "Hello")
    expect_equal(readLines(dst2), "World")
  })

  it("errors if the destination is a directory", {
    d <- withr::local_tempdir()
    expect_error(copy_files(src1, d), "Destination path is a directory")
  })

  it("errors if argument length is different", {
    d <- withr::local_tempfile()
    expect_error(copy_files(c(src1, src2), d),
                 "have different lengths")
  })
})
