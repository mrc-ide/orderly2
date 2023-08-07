orderly_context <- function(envir) {
  p <- get_active_packet()
  is_active <- !is.null(p)
  if (is_active) {
    path <- p$path
    root <- p$root$path
    config <- p$orderly2$config
    envir <- p$orderly2$envir
    src <- p$orderly2$src
    parameters <- p$parameters
    name <- p$name
    id <- p$id
    search_options <- p$orderly2$search_options
  } else {
    path <- getwd()
    root <- detect_orderly_interactive_path(path)$path
    config <- root_open(root,
                        locate = FALSE,
                        require_orderly = TRUE)$config$orderly
    src <- path
    parameters <- current_orderly_parameters(src, envir)
    name <- basename(path)
    id <- NA_character_
    search_options <- .interactive$search_options
  }
  list(is_active = is_active, path = path, config = config, envir = envir,
       root = root, src = src, name = name, id = id, parameters = parameters,
       search_options = search_options, packet = p)
}


##' Fetch information about the actively running report.  This allows
##' you to reflect information about your report back as part of the
##' report, for example embedding the current report id, or
##' information about computed dependencies. This information is in a
##' slightly different format to orderly version 1.x and does not
##' (currently) include information about dependencies when run
##' outside of [orderly2::orderly_run], but this was never reliable
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
##'       [`orderly2::orderly_dependency`]
##'     - `name`: the name of the dependency
##'     - `query`: the query used to find the dependency
##'     - `id`: the computed id of the included packet
##'     - `filename`: the file used from the packet
##'     - `as`: the filename used locally
##' @export
orderly_run_info <- function() {
  ctx <- orderly_context(rlang::caller_env())

  id <- ctx$packet$id %||% NA_character_
  name <- ctx$name

  root <- root_open(ctx$root, FALSE, TRUE, environment())

  deps <- ctx$packet$depends
  deps_n <- vnapply(deps, function(x) nrow(x$files))
  deps_name <- vcapply(deps, function(x) {
    outpack_metadata_core(x$packet, root)$name
  })
  depends <- data_frame(
    index = rep(seq_along(deps), deps_n),
    name = rep(deps_name, deps_n),
    query = rep(vcapply(deps, "[[", "query"), deps_n),
    id = rep(vcapply(deps, "[[", "packet"), deps_n),
    there = unlist(lapply(deps, function(x) x$files$there)) %||% character(),
    here = unlist(lapply(deps, function(x) x$files$here)) %||% character())

  list(name = name, id = id, root = root$path, depends = depends)
}
