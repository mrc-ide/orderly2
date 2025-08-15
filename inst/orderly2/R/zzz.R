.onLoad <- function(...) {
  msg <- c(
    "You have loaded orderly2, rather than orderly",
    i = paste("Possibly you have code that should be updated; please see",
              "https://mrc-ide.github.io/orderly/articles/migrating.html"))
  if (isTRUE(getOption("orderly2.error", FALSE))) {
    cli::cli_abort(msg)
  }
  if (!isTRUE(getOption("orderly2.nowarn", FALSE))) {
    cli::cli_warn(msg)
  }
}
