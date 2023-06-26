orderly_context <- function() {
  p <- get_active_packet()
  is_active <- !is.null(p)
  if (is_active) {
    path <- p$path
    root <- p$root$path
    config <- p$orderly3$config
    env <- p$orderly3$envir
    src <- p$orderly3$src
    parameters <- p$parameters
    name <- p$name
    id <- p$id
    search_options <- p$orderly3$search_options
  } else {
    path <- getwd()
    root <- detect_orderly_interactive_path(path)$path
    config <- orderly_root(root, FALSE)$config
    env <- orderly_environment("orderly3")
    src <- path
    parameters <- current_orderly_parameters(src, env)
    name <- basename(path)
    id <- NA_character_
    search_options <- .interactive$search_options
  }
  list(is_active = is_active, path = path, config = config, env = env,
       root = root, src = src, name = name, id = id, parameters = parameters,
       search_options = search_options, packet = p)
}


## This is a real trick, and is only used in the case where the report
## is being run interactively, and in that case the correct thing is
## *almost certainly* the global environment, as the user has to do
## some tricks to stop that being the case; for example running
##
## > source("orderly.R", local = TRUE)
##
## We want to find the environment that corresponds to the top level
## environment for orderly; that will be the one that called the
## plugin function. So we'll have a stack of frames (corresponding to
## the environments on the call stack) with *parents* that look like
## this, outermost first:
##
## - (anything else)
## - (calling environment) <-- this is what we're looking for
## - (plugin)
## - (orderly3; orderly_plugin_context)
## - (orderly3; from orderly_context)
## - (orderly3; from orderly_environment)
##
## so we loop down the stack looking for the first call to a function
## in the plugin package, then take the frame *above* that.
##
## When we want this for a non-plugin case (i.e., the caller is in
## orderly3) then we just need to pass name = "orderly3" here
orderly_environment <- function(name) {
  frames <- sys.frames()
  for (i in seq_along(frames)[-1]) {
    if (environmentName(parent.env(frames[[i]])) == name) {
      return(frames[[i - 1]])
    }
  }
  ## This error should never surface if the plugin is configured correctly
  stop("Could not determine calling environment safely - please report")
}


##' Fetch information about the actively running report.  This allows
##' you to reflect information about your report back as part of the
##' report, for example embedding the current report id, or
##' information about computed dependencies. This information is in a
##' slightly different format to orderly version 1.x and does not
##' (currently) include information about dependencies when run
##' outside of [orderly3::orderly_run], but this was never reliable
##' previously.
##'
##' @title Information about currently running report
##'
##' @return A list with elements
##'
##' * `name`: The name of the current report
##' * `id`: The id of the current report, `NA` if running interactively
##' * `root`: The orderly root path
##' * `depends`: A data frame with information about the dependencies
##'   (not available interactively)
##'     - `index`: an integer sequence along calls to
##'       [`orderly3::orderly_dependency`]
##'     - `name`: the name of the dependency
##'     - `query`: the query used to find the dependency
##'     - `id`: the computed id of the included packet
##'     - `filename`: the file used from the packet
##'     - `as`: the filename used locally
##' @export
orderly_run_info <- function() {
  ctx <- orderly_context()

  id <- ctx$packet$id %||% NA_character_
  name <- ctx$name

  root <- orderly_root(ctx$root, FALSE)

  deps <- ctx$packet$depends
  deps_n <- vnapply(deps, function(x) nrow(x$files))
  deps_name <- vcapply(deps, function(x) root$outpack$metadata(x$packet)$name)
  depends <- data_frame(
    index = rep(seq_along(deps), deps_n),
    name = rep(deps_name, deps_n),
    query = rep(vcapply(deps, "[[", "query"), deps_n),
    id = rep(vcapply(deps, "[[", "packet"), deps_n),
    there = unlist(lapply(deps, function(x) x$files$there)) %||% character(),
    here = unlist(lapply(deps, function(x) x$files$here)) %||% character())

  list(name = name, id = id, root = root$path, depends = depends)
}
