#!/usr/bin/env Rscript
stopifnot(read.dcf("DESCRIPTION", "Package") == "orderly2")
nms <- getNamespaceExports("orderly")
writeLines(
  c(sprintf("#' @importFrom orderly %s", nms),
    sprintf("#' @export %s", nms),
    "NULL"),
  "R/imports.R")
devtools::document()
