options(outpack.schema_validate =
          requireNamespace("jsonvalidate", quietly = TRUE) &&
          packageVersion("jsonvalidate") >= "1.4.0")


test_prepare_orderly_example <- function(examples, ...) {
  tmp <- tempfile()
  withr::defer_parent(unlink(tmp, recursive = TRUE))
  orderly_init(tmp, logging_console = FALSE)
  config <- readLines(file.path(tmp, "orderly_config.yml"))

  if (any(c("global", "global-dir") %in% examples)) {
    config <- c(config,
                "global_resources: global")
    fs::dir_create(file.path(tmp, "global"))
    if ("global" %in% examples) {
      fs::file_copy(test_path("examples/explicit/data.csv"),
                    file.path(tmp, "global"))
    }
    if ("global-dir" %in% examples) {
      fs::dir_create(file.path(tmp, "global", "data"))
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
  clear_env(.interactive)
}


clear_env <- function(env) {
  rm(list = ls(env), envir = env)
}


skip_on_solaris <- function() {
  testthat::skip_on_os("solaris")
}


is_windows <- function() {
  tolower(Sys.info()[["sysname"]]) == "windows"
}
