#!/usr/bin/env Rscript

# Ensure this script is run from the 'inst/orderly2' subdirectory:
stopifnot(read.dcf("DESCRIPTION", "Package") == "orderly2")

# We will not have to rerun this very often, if ever again, because
# people using 'orderly2::fn' in their code won't expect new orderly1
# functions to appear.  Nonetheless, the file *was* automatically
# generated so we might as well save the script
nms <- getNamespaceExports("orderly")
writeLines(
  c(sprintf("importFrom(orderly, %s)", nms),
    sprintf("export(%s)", nms)),
  "NAMESPACE")
