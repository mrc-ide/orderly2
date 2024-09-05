assert_simple_scalar_atomic <- function(x, name = deparse(substitute(x)),
                                        arg = name, call = parent.frame()) {
  assert_scalar(x, name, call = call)
  if (!is_simple_atomic(x)) {
    cli::cli_abort("'{name}' must be atomic (string, numeric, logical)",
                   call = call, arg = arg)
  }
  invisible(x)
}


find_entrypoint_filename <- function(src, suppress_zero_files = FALSE,
                                     suppress_multiple_files = FALSE) {
  reportname <- basename(src)
  names <- c(sprintf("%s.R", reportname), "orderly.R")
  files_exist <- file.exists(file.path(src, names))
  n_found <- sum(files_exist)
  if (n_found > 1 && !suppress_multiple_files) {
    cli::cli_abort(
      paste("Please only create {names[[1]]} file, orderly.R",
            "has been deprecated")
    )
  }
  if (n_found == 0 && !suppress_zero_files) {
    cli::cli_abort(
      "Please create {names[[1]]} file"
    )
  }
  if (files_exist[[2]]) {
    cli::cli_warn(
      paste("Naming convention orderly.R will be deprecated",
            "soon. Please change orderly file name to",
            "<reportname>.R"),
      .frequency = "regularly",
      .frequency_id = "deprecate_orderly_file_name"
    )
  }
  if (n_found == 1) names[files_exist] else NA_character_
}
