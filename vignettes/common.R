## Common support code for vignettes. This will not be echoed to the
## user, so be sure not to define functions here that they might want
## to use.
##
## Typically, include this in the Rmd with
##
## ```{r, include = FALSE}
## source("common.R")
## ```

dir_tree <- function(path, sub = ".", ...) {
  withr::with_dir(path, fs::dir_tree(sub, ...))
}

lang_output <- function(x, lang) {
  writeLines(c(sprintf("```%s", lang), x, "```"))
}
r_output <- function(x) {
  lang_output(x, "r")
}
yaml_output <- function(x) {
  lang_output(x, "yaml")
}
json_output <- function(x) {
  lang_output(x, "json")
}
plain_output <- function(x) {
  lang_output(x, "plain")
}
orderly_file <- function(...) {
  system.file(..., package = "orderly2", mustWork = TRUE)
}

inline <- function(x) {
  sprintf("`%s`", format(x))
}

knitr::opts_chunk$set(
  collapse = TRUE)

.here <- getwd()
knitr::knit_hooks$set(inwd = function(before, options) {
  if (before) {
    setwd(options$inwd)
  } else {
    setwd(.here)
  }
  invisible()
}
)
