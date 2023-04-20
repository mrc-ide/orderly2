options(outpack.schema_validate =
          requireNamespace("jsonvalidate", quietly = TRUE) &&
          packageVersion("jsonvalidate") >= "1.4.0")


test_prepare_orderly_example <- function(examples, ...) {
  tmp <- tempfile()
  withr::defer_parent(unlink(tmp, recursive = TRUE))
  orderly_init(tmp)

  config <- character()

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


## Expects dependencies to be a list of name and id (or search query)
## e.g.
## list(a = "latest", b = "latest(parameter:x == 2)") # nolint
## By convention will expect report artefact to be "data.rds" and will
## map this to "input<i>.rds" in the orderly.R where i is the number of
## dependencies
create_random_report <- function(root, name = "data", dependencies = NULL) {
  report_dir <- fs::dir_create(file.path(root, "src", name))
  withr::defer_parent(unlink(report_dir, recursive = TRUE))

  if (!is.null(dependencies)) {
    assert_named(dependencies)
    variables <- c()
    script <- unlist(lapply(seq_along(dependencies), function(i) {
      name <- names(dependencies)[[i]]
      query <- dependencies[[i]]
      file_input <- paste0("input", i, ".rds")
      variable <- paste0("x", i)
      variables <- c(variables, variable)
      c(sprintf(
        "orderly3::orderly_depends(\"%s\", \"%s\", c(%s = \"data.rds\"))",
        name, query, file_input),
        sprintf("%s <- readRDS(\"%s\")", variable, file_input))
    }))
    script <- c(
      script,
      sprintf("x <- %s + runif(10)", paste(variables, collapse = " + ")))
  } else {
    script <- "x <- runif(10)"
  }

  script <- c(script,
              "orderly3::orderly_artefact(\"Final data\", \"data.rds\")",
              "saveRDS(x, \"data.rds\")")

  writeLines(script, file.path(report_dir, "orderly.R"))
  invisible(report_dir)
}
