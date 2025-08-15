## We have had orderly out as 'orderly2' for long enough that there
## are quite a few existing source archives that use things like
## `orderly2::fn()` and workflows that include `orderly2`.  So we want
## to have the ability to smooth over this as best as possible.  There
## are several aspects to this:
##
## 1. we should offer to rewrite people's source code
##
## 2. we should create a stub package on the fly that contains all the
## new orderly bits, with an infrequent warning that this has happened
##
## 3. we should create a real, installable, package that wrap over
## orderly (inst/orderly2, we'll pop this onto the universe).
load_orderly2_support <- function() {
  if (isTRUE(cache$orderly2_support)) {
    return(invisible())
  }
  if (isTRUE(getOption("orderly_disable_orderly2_compat", FALSE))) {
    cli::cli_abort(
      paste("Not loading orderly2 support, as this is disabled by the option",
            "'orderly_disable_orderly2_compat'"))
  }
  if (isNamespaceLoaded("orderly2")) {
    if (getNamespaceVersion("orderly2") != "1.99.99") {
      cli::cli_abort(
        c("Can't load orderly2 compatibility as orderly2 is loaded",
          i = paste("You have an old version of 'orderly2' installed and",
                    "loaded; please try again in a fresh session, perhaps",
                    "after removing 'orderly2"),
          i = 'Try {.code remove.packages("orderly2")}'))
    }
  } else {
    installed <- orderly2_installed_version()
    withr::local_options(orderly2.nowarn = TRUE)
    if (installed$installed && installed$version == "1.99.99") {
      loadNamespace("orderly2")
    } else {
      pkgload::load_all(orderly_file("orderly2"),
                        export_all = FALSE,
                        export_imports = FALSE,
                        attach = FALSE,
                        helpers = FALSE,
                        attach_testthat = FALSE,
                        quiet = TRUE)
    }
  }
  cache$orderly2_support <- TRUE
  invisible()
}


orderly2_installed_version <- function() {
  if (is.null(cache$orderly2_version)) {
    version <- tryCatch(
      utils::packageVersion("orderly2"),
      error = function(e) NULL)
    cache$orderly2_version <- list(installed = !is.null(version),
                                   version = version)
  }
  cache$orderly2_version
}
