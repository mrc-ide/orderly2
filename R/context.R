orderly_context <- function(envir) {
  p <- get_active_packet()
  is_active <- !is.null(p)
  if (is_active) {
    path <- p$path
    root <- p$root$path
    root_src <- p$orderly2$root
    config <- p$orderly2$config
    envir <- p$orderly2$envir
    src <- p$orderly2$src
    parameters <- p$parameters
    name <- p$name
    id <- p$id
    search_options <- p$orderly2$search_options
  } else {
    path <- getwd()
    root_src <- detect_orderly_interactive_path(path)
    root <- root_src # for now at least
    config <- orderly_config_read(root)
    src <- path
    parameters <- current_orderly_parameters(src, envir)
    name <- basename(path)
    id <- NA_character_
    search_options <- .interactive$search_options
  }
  list(is_active = is_active, path = path, config = config, envir = envir,
       root = root, root_src = root_src, src = src, name = name,
       id = id, parameters = parameters, search_options = search_options,
       packet = p)
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
##' @examples
##' # An example from the orderly examples
##' orderly_example_show("run_info")
##'
##' # Prepare to run
##' path <- orderly_example("demo")
##' orderly_run("data", root = path, echo = FALSE)
##'
##' # Here, see the printed information frmo a real running report
##' orderly_run("run_info", root = path)
orderly_run_info <- function() {
  ctx <- orderly_context(rlang::caller_env())

  id <- ctx$packet$id %||% NA_character_
  name <- ctx$name

  root <- root_open(ctx$root, require_orderly = TRUE)

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
