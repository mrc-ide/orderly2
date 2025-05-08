tests_schema_validate <- function() {
  opt <- getOption("outpack.schema_validate", NULL)
  if (!is.null(opt)) {
    return(opt)
  }
  if (isTRUE(as.logical(Sys.getenv("CI", "false")))) {
    return(TRUE)
  }
  requireNamespace("jsonvalidate", quietly = TRUE)
}


withr::local_options(
  orderly.index_progress = FALSE,
  outpack.schema_validate = tests_schema_validate(),
  .local_envir = teardown_env())
