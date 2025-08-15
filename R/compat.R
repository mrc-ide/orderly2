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
  correct <- numeric_version("1.99.99")
  if (isNamespaceLoaded("orderly2")) {
    if (getNamespaceVersion("orderly2") != correct) {
      cli::cli_abort(
        c("Can't load orderly2 compatibility as orderly2 is loaded",
          i = paste("You have an old version of 'orderly2' installed and",
                    "loaded; please try again in a fresh session, perhaps",
                    "after removing 'orderly2"),
          i = 'Try {.code remove.packages("orderly2")}'))
    }
  } else {
    installed_version <-
      tryCatch(packageVersion("orderly2"), error = function(e) NULL)
    withr::local_options(orderly2.nowarn = TRUE)
    if (!is.null(installed_version) && installed_version == correct) {
      ## The user has installed our dummy version of orderly2, so just
      ## load that.
      loadNamespace("orderly2")
    } else {
      ## Try and use pkgload to load the namespace, which is a bit
      ## weird but works
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


##' Migrate source code from orderly2 (version 1.99.78 and previous)
##' to refer to orderly (version 1.99.79 and subsequent).  This is a
##' one-off and will not exist in the package for long, get it while
##' it's hot.
##'
##' @title Migrate orderly2 sources
##'
##' @param path Path to the repo.  We will not change anything here
##'   unless the path is under source control, and unless the git
##'   status is "up to date" (i.e., no local unsaved modifications or
##'   untracked files).  It is recommended to run this against a fresh
##'   clone.
##'
##' @return Nothing, called for side effects only
##' @export
orderly_migrate_source_from_orderly2 <- function(path) {
  status <- tryCatch(
    gert::git_status(repo = path),
    error = function(e) {
      cli::cli_abort(
        "Not migrating '{path}' as it does not appear to be version controlled")
    })
  if (nrow(status) != 0) {
    cli::cli_abort(
      c("Not migrating '{path}' as 'git status' is not clean",
        i = "Try running this in a fresh clone"))
  }
  files <- dir(path, pattern = "\\.[Rr]", recursive = TRUE)
  cli::cli_alert_info("Migrating {length(files)} file{?s} in '{path}'")
  changed <- logical(files)
  for (i in seq_along(files)) {
    changed[[i]] <- orderly_migrate_file(path, files[[i]])
  }
  if (any(changed)) {
    cli::cli_alert_success("Changed {sum(changed)} file{?s}")
    cli::cli_alert_info("Please add and commit these to git")
  } else {
    cli::cli_alert_success("Nothing to change!")
  }
}


orderly_migrate_file <- function(path, file) {
  filename <- file.path(path, file)
  prev <- txt <- readLines(filename)
  txt <- sub("orderly2::", "orderly::", txt, fixed = TRUE)
  txt <- sub("library(orderly2)", "library(orderly)", txt, fixed = TRUE)
  n <- sum(txt != prev)
  if (n > 0) {
    cli::cli_alert_info("Updated {n} line{?s} in {path}")
    writeLines(txt, filename)
  }
}
