options(outpack.schema_validate =
          requireNamespace("jsonvalidate", quietly = TRUE) &&
          packageVersion("jsonvalidate") >= "1.4.0",
        orderly.index_progress = FALSE,
        orderly.quiet = TRUE)


test_prepare_orderly_example <- function(examples, ...) {
  tmp <- tempfile()
  withr::defer_parent(fs::dir_delete(tmp))
  suppressMessages(orderly_init(tmp, ...))
  copy_examples(examples, tmp)
  as.character(fs::path_norm(tmp))
}


test_prepare_orderly_example_separate <- function(examples, ...) {
  tmp <- tempfile()
  withr::defer_parent(fs::dir_delete(tmp))

  path_outpack <- file.path(tmp, "outpack")
  suppressMessages(orderly_init(path_outpack, ...))
  fs::file_delete(file.path(path_outpack, "orderly_config.yml"))

  path_src <- file.path(tmp, "src")
  copy_examples(examples, path_src)

  list(src = path_src, outpack = path_outpack)
}


copy_examples <- function(examples, path_src) {
  if (file.exists(path_src)) {
    config <- readLines(file.path(path_src, "orderly_config.yml"))
  } else {
    config <- empty_config_contents()
  }

  fs::dir_create(path_src)
  if (any(c("shared", "shared-shorthand", "shared-dir") %in% examples)) {
    fs::dir_create(file.path(path_src, "shared"))
    if (any(c("shared", "shared-shorthand") %in% examples)) {
      fs::file_copy(test_path("examples/explicit/data.csv"),
                    file.path(path_src, "shared"))
    }
    if ("shared-dir" %in% examples) {
      fs::dir_create(file.path(path_src, "shared", "data"))
    }
  }

  if ("plugin" %in% examples) {
    register_example_plugin()
    config <- c(config,
                "plugins:",
                "  example.random:",
                "    distribution:",
                "      normal")
  }
  writeLines(config, file.path(path_src, "orderly_config.yml"))

  fs::dir_create(file.path(path_src, "src"))
  for (i in examples) {
    fs::dir_copy(test_path("examples", i), file.path(path_src, "src"))
  }
}


test_path <- function(...) {
  if (basename(getwd()) == "testthat") {
    file.path(...)
  } else {
    testthat::test_path(...)
  }
}


clear_plugins <- function() {
  rm(list = ls(envir = .plugins), envir = .plugins)
}


register_example_plugin <- function() {
  testthat::skip_if_not_installed("pkgload")
  pkgload::load_all(test_path("plugins/example.random"),
                    quiet = TRUE, export_all = FALSE)
}


prepend_lines <- function(path, lines) {
  txt <- readLines(path)
  writeLines(c(lines, txt), path)
}


append_lines <- function(path, lines) {
  txt <- readLines(path)
  writeLines(c(txt, lines), path)
}


reset_interactive <- function() {
  clear_envir(.interactive)
}


clear_envir <- function(envir) {
  rm(list = ls(envir), envir = envir)
}


skip_on_solaris <- function() {
  testthat::skip_on_os("solaris")
}


is_windows <- function() {
  tolower(Sys.info()[["sysname"]]) == "windows"
}


orderly_init_quietly <- function(...) {
  suppressMessages(orderly_init(...))
}


orderly_run_quietly <- function(..., echo = FALSE) {
  suppressMessages(orderly_run(..., echo = echo))
}
