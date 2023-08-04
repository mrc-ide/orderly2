options(outpack.schema_validate =
          requireNamespace("jsonvalidate", quietly = TRUE) &&
          packageVersion("jsonvalidate") >= "1.4.0")


test_prepare_orderly_example <- function(examples, ...) {
  tmp <- tempfile()
  withr::defer_parent(unlink(tmp, recursive = TRUE))
  orderly_init(tmp, logging_console = FALSE)
  config <- readLines(file.path(tmp, "orderly_config.yml"))

  if (any(c("shared", "shared-dir") %in% examples)) {
    fs::dir_create(file.path(tmp, "shared"))
    if ("shared" %in% examples) {
      fs::file_copy(test_path("examples/explicit/data.csv"),
                    file.path(tmp, "shared"))
    }
    if ("shared-dir" %in% examples) {
      fs::dir_create(file.path(tmp, "shared", "data"))
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

  writeLines(config, file.path(tmp, "orderly_config.yml"))
  fs::dir_create(file.path(tmp, "src"))
  for (i in examples) {
    fs::dir_copy(test_path("examples", i), file.path(tmp, "src"))
  }
  tmp
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


skip_if_older_gert <- function() {
  testthat::skip_if(is.null(gert_git_ignore_path_is_ignored()),
                    "older gert")
}
