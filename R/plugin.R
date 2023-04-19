##' Create an orderly plugin. A plugin is typically defined by a
##' package and is used to extend orderly by enabling new
##' functionality, declared in `orderly_config.yml` and `orderly.R`
##' and affecting the running of reports primarily by creating new
##' objects in the report environment.  This system is discussed in
##' more detail in `vignette("plugins")`, but will be expanded (likely
##' in breaking ways) soon.
##'
##' @title Register an orderly plugin
##'
##' @param name The name of the plugin, typically the package name
##'
##' @param config A function to read, check and process the
##'   configuration section in `orderly_config.yml`. This function
##'   will be passed the deserialised data from the plugin's section
##'   of `orderly_config.yml`, and the full path to that file.  As the
##'   order of loading of plugins is not defined, each plugin must
##'   standalone and should not try and interact with other plugins at
##'   load. It should return a processed copy of the configuration
##'   data, to be passed in as the second argument to `read`.
##'
##' @param serialise A function to serialise any metadata added by the
##'   plugin's functions to the outpack metadata. It will be passed a
##'   list of all entries pushed in via
##'   [`orderly3::orderly_plugin_add_metadata()`]; this is a named
##'   list with names corresponding to the `field` argument to
##'   `orderly_plugin_add_metadata` and each list element being an
##'   unnamed list with values corresponding to `data`.
##'
##' @param cleanup Optionally, a function to clean up any state that
##'   your plugin uses. You can call `orderly_plugin_context` from
##'   within this function and access anything you need from that. If
##'   not given, then no cleanup is done.
##'
##' @param schema Optionally a path to a schema for the metadata
##'   created by this plugin. See `vignette("plugins")` for details.
##'
##' @return Nothing, this function is called for its side effect of
##'   registering a plugin.
##'
##' @export
orderly_plugin_register <- function(name, config, serialise, cleanup = NULL,
                                    schema = NULL) {
  assert_scalar_character(name)
  .plugins[[name]] <- orderly_plugin(config, serialise, cleanup, schema)
}


load_orderly_plugin <- function(name) {
  assert_scalar_character(name)
  if (!(name %in% names(.plugins))) {
    loadNamespace(name)
  }
  plugin <- .plugins[[name]]
  if (is.null(plugin)) {
    stop(sprintf("Plugin '%s' not found", name))
  }

  plugin
}


.plugins <- new.env(parent = emptyenv())


orderly_plugin <- function(config, serialise, cleanup, schema) {
  assert_is(config, "function")
  assert_is(serialise, "function")
  if (is.null(cleanup)) {
    cleanup <- plugin_no_cleanup
  }
  assert_is(cleanup, "function")
  if (!is.null(schema)) {
    assert_file_exists(schema, name = "Schema file")
    schema <- paste(readLines(schema), collapse = "\n")
    class(schema) <- "json"
  }
  ret <- list(config = config,
              serialise = serialise,
              cleanup = cleanup,
              schema = schema)
  class(ret) <- "orderly_plugin"
  ret
}

##' Fetch the running context, for use within a plugin.  The intention
##' here is that within free functions that your plugin makes
##' available, you will call this function to get information about
##' the state of a packet.  You will then typically call
##' [`orderly3::orderly_plugin_add_metadata()`] afterwards.
##'
##' When a plugin function is called, orderly3 will be running in one
##' of two modes; (1) from within [`orderly3::orderly_run()`], in
##' which case we're part way through creating a packet in a brand new
##' directory, and possibly using a special environment for
##' evaluation, or (2) interactively, with a user developing their
##' report. The plugin needs to be able to support both modes, and
##' this function will return information about the state to help you
##' cope with either case.
##'
##' @title Fetch plugin context
##'
##' @param name Name of the plugin
##'
##' @return A list with elements:
##'
##' * `is_active`: a logical, indicating if we're running under
##'   [`orderly3::orderly_run()`]; you may need to change behaviour
##'   depending on this value.
##'
##' * `path`: the path of the running packet. This is almost always the
##'   working directory, unless the packet contains calls to [`setwd()`]
##'   or similar. You may create files here.
##'
##' * `config`: the configuration for this plugin, after processing
##'   with the plugin's `read` function (see
##'   [`orderly3::orderly_plugin_register`])
##'
##' * `env`: the environment that the packet is running in. Often this
##'   will be the global environment, but do not assume this! You may
##'   read and write from this environment.
##'
##' * `src`: the path to the packet source directory. This is
##'   different to the current directory when the packet is running,
##'   but the same when the user is interactively working with a
##'   report. You may *read* from this directory but *must not write
##'   to it*
##'
##' * `parameters`: the parameters as passed through to the run the
##'   report.
##'
##' @seealso [orderly3::orderly_plugin_register],
##' [orderly3::orderly_plugin_add_metadata]
##' @export
orderly_plugin_context <- function(name) {
  assert_scalar_character(name)
  ctx <- orderly_context()
  check_plugin_enabled(name, ctx$config)
  ctx$config <- ctx$config$plugins[[name]]$config
  ctx
}


##' Add plugin-specific metadata to a running packet. This will take
##' some describing. You accumulate any number of bits of metadata
##' into arbitrary fields, and then later on serialise these to json.
##'
##' @title Add metadata from plugin
##'
##' @param name The name of the plugin; must be the same as used in
##'   [orderly3::orderly_plugin_register] and
##'   [orderly3::orderly_plugin_context]
##'
##' @param field The name of a field to add the data to. This is
##'   required even if your plugin only produces one sort of data, in
##'   which case you can remove it later on within your serialisation
##'   function.
##'
##' @param data Arbitrary data to be added to the currently running
##'   packet
##'
##' @return Nothing, called only for its side effects
##'
##' @export
orderly_plugin_add_metadata <- function(name, field, data) {
  assert_scalar_character(name)
  assert_scalar_character(field)
  p <- get_active_packet()
  if (!is.null(p)) {
    check_plugin_enabled(name, p$orderly3$config)
    p$orderly3$plugins[[name]][[field]] <-
      c(p$orderly3$plugins[[name]][[field]], list(data))
  }
}


## This is a real trick, and is only used in the case where the report
## is being run interactively, and in that case the correct thing is
## *almost certainly* the global environment, as the user has to do
## some tricks to stop that being the case; for example running
##
##   source("orderly.R", local = TRUE)
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
## - (orderly3; from orderly_plugin_environment)
##
## so we loop down the stack looking for the first call to a function
## in the plugin package, then take the frame *above* that.
orderly_plugin_environment <- function(name) {
  frames <- sys.frames()
  for (i in seq_along(frames)[-1]) {
    if (environmentName(parent.env(frames[[i]])) == name) {
      return(frames[[i - 1]])
    }
  }
  ## This error should never surface if the plugin is configured correctly
  stop("Could not determine plugin calling environment safely - please report")
}


check_plugin_enabled <- function(name, config) {
  if (is.null(config$plugins[[name]])) {
    stop(sprintf("Plugin '%s' not enabled in 'orderly_config.yml'", name))
  }
}


plugin_run_cleanup <- function(path, plugins) {
  for (p in plugins) {
    withr::with_dir(path, p$cleanup())
  }
}


plugin_no_cleanup <- function() {
}
