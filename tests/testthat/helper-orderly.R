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
