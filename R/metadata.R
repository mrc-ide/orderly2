##' Put orderly2 into "strict mode", which is closer to the defaults
##' in orderly 1.0.0; in this mode only explicitly included files (via
##' [orderly2::orderly_resource] and
##' [orderly2::orderly_shared_resource]) are copied when running a
##' packet, and we warn about any unexpected files at the end of the
##' run.  Using strict mode allows orderly2 to be more aggressive in
##' how it deletes files within the source directory, more accurate in
##' what it reports to you, and faster to start packets after
##' developing them interactively.
##'
##' In future, we may extend strict mode to allow requiring that no
##' computation occurs within orderly functions (i.e., that the
##' requirements to run a packet are fully known before actually
##' running it).  Most likely this will *not* be the default behaviour
##' and `orderly_strict_mode` will gain an argument.
##'
##' We will allow server processes to either override this value
##' (enabling it even when it is not explicitly given) and/or require
##' it.
##'
##' @title Enable orderly strict mode
##' @export
##' @return Undefined
orderly_strict_mode <- function() {
  p <- get_active_packet()
  if (!is.null(p)) {
    prevent_multiple_calls(p, "strict_mode", environment())
    p$orderly2$strict <- static_orderly_strict_mode(list())
  }
  invisible()
}


static_orderly_strict_mode <- function(args) {
  list(enabled = TRUE)
}


##' Declare orderly parameters. You should only have one call to this
##' within your file! Typically you'd put the call to this function
##' very close to the top so that it's easy to scan, though the order
##' does not really matter.  Parameters are scalar atomic values
##' (e.g. a string, number or boolean) and defaults must be present
##' literally (i.e., they may not come from a variable
##' itself). Provide `NULL` if you do not have a default, in which
##' case this parameter will be required.
##'
##' # Parameters and variables
##'
##' Prior to orderly 1.99.61, parameters are always available as
##' variables in the execution environment.  In order to harmonise the
##' R and Python versions of orderly, we are moving away from this, at
##' least by default.  The recommended way of using parameters is to
##' assign it to a variable, for example:
##'
##' ```
##' pars <- orderly_parameters(debug = FALSE, replicates = NULL)
##' ```
##'
##' This defines two parameters, `debug` (with a default) and
##' `replicates` (without a default).  In the running report, you can
##' access these by subsetting the `pars` object (e.g., `pars$debug`
##' or `pars[["replicates"]]`).
##'
##' To get the old behaviour, do not assign to a variable:
##'
##' ```
##' orderly_parameters(debug = FALSE, replicates = NULL)
##' ```
##'
##' This will create two bindings in the environment (`debug` and
##' `replicates`) but will also generate a deprecation warning and we
##' will remove support in a release of orderly 2.x.  If you really want
##' the old behaviour, you can achieve it by writing:
##'
##' ```
##' pars <- orderly_parameters(debug = FALSE, replicates = NULL)
##' list2env(pars, environment())
##' ```
##'
##' # Behaviour in interactive sessions
##'
##' We want you to be able to run through an orderly report
##' interactively, e.g. via `source()`, by copy/paste or via the "Run"
##' or "Source" button in RStudio.  This is not very compatible with
##' use of orderly parameters, because normally you'd provide these to
##' [orderly_run()], so we need a mechanism to get the parameters from
##' you.
##'
##' The behaviour differs if you have assigned the result of
##' `orderly_parameters` to a variable or are using the (deprecated)
##' behaviour of exporting parameters as variables.
##'
##' ## New behaviour
##'
##' Suppose that you are assigning to `pars`.  The first time we run
##' though your code we won't see a value of `pars` and we'll prompt
##' for values for each parameter.  Those that have default values in
##' your list will offer these values to make selection of parameters
##' faster.
##'
##' On subsequent calls, `pars` will be present with the values you
##' used previously; these will be reused.  If you want to be
##' re-prompted, delete `pars` (i.e., `rm("pars")`) or assign `NULL`
##' (i.e., `pars <- NULL`).
##'
##' ## Old behaviour
##'
##' This is now deprecated, and you should update your code.
##'
##' When running interactively (i.e., via `source()` or running an
##'   orderly file session by copy/paste or in Rstudio), the
##'   `orderly_parameters()` function has different behaviour, and
##'   this behaviour depends on whether parameters will be exported to the
##'   environment or not.
##'
##' First, we look in the current environment (most likely the global
##'   environment) for values of your parameters - that is, variables
##'   bound to the names of your parameters.  For any parameters that
##'   are not found we will look at the default values and use these
##'   if possible, but if not possible then we will either error or
##'   prompt based on the global option
##'   `orderly_interactive_parameters_missing_error`. If this is
##'   `TRUE`, then we will ask you to enter a value for the parameters
##'   (strings will need to be entered with quotes).
##'
##' @title Declare orderly parameters
##'
##' @param ... Any number of parameters
##'
##' @return A list of parameters.  This list is "strict" so accessing
##'   elements that are not present will throw an error rather than
##'   returning `NULL`.
##'
##' @export
orderly_parameters <- function(...) {
  p <- get_active_packet()
  if (is.null(p)) {
    pars <- orderly_context(rlang::caller_env())$parameters
  } else {
    pars <- p$parameters
  }

  pars
}


static_orderly_parameters <- function(args, call) {
  if (length(args) == 0L) {
    return(NULL)
  }
  assert_named(args, unique = TRUE, name = "Arguments to 'orderly_parameters'",
               call = call, arg = NULL)
  check_parameter_values(args, TRUE, call)
  as_strict_list(args, name = "parameters")
}


current_orderly_parameters <- function(src, envir) {
  dat <- orderly_read(src)
  base <- dat$parameters
  target <- dat$parameters_target
  check_parameters_interactive(envir, base, target, NULL)
}


##' Describe the current packet
##'
##' @title Describe the current packet
##'
##' @param display A friendly name for the report; this will be
##'   displayed in some locations of the web interface, packit. If
##'   given, it must be a scalar character.
##'
##' @param long A longer description of the report. If given,
##'   it must be a scalar character.
##'
##' @param custom Any additional metadata. If given, it must be a named
##'   list, with all elements being scalar atomics (character, number,
##'   logical).
##'
##' @return Undefined
##' @export
orderly_description <- function(display = NULL, long = NULL, custom = NULL) {
  assert_scalar_character(display, allow_null = TRUE, call = environment())
  assert_scalar_character(long, allow_null = TRUE, call = environment())
  if (!is.null(custom)) {
    assert_named(custom, unique = TRUE, call = environment())
    assert_is(custom, "list", call = environment())
    for (i in names(custom)) {
      assert_simple_scalar_atomic(custom[[i]], sprintf("custom$%s", i),
                                  call = environment())
    }
  }

  p <- get_active_packet()
  if (!is.null(p)) {
    prevent_multiple_calls(p, "description", environment())
    p$orderly2$description <-
      list(display = display, long = long, custom = custom)
  }
  invisible()
}


static_orderly_description <- function(args) {
  list(displayname = static_string(args$displayname),
       description = static_string(args$description),
       ## It's possible we could do better here, but it's not trivial
       ## and not high value:
       custom = NULL)
}


##' Declare that a file, or group of files, are an orderly
##' resource. By explicitly declaring files as resources orderly will
##' mark the files as immutable inputs and validate that your analysis
##' does not modify them when run with [orderly_run()]
##'
##' @title Declare orderly resources
##'
##' @param files Any number of names of files or directories.  If you
##'   list a directory it is expanded recursively to include all
##'   subdirectories and files.
##'
##' @return Invisibly, a character vector of resources included by the
##'   call. Don't rely on the order of these files if they are
##'   expanded from directories, as this is likely platform dependent.
##'
##' @export
orderly_resource <- function(files) {
  p <- get_active_packet()
  src <- if (is.null(p)) "." else p$orderly2$src
  assert_file_exists_relative(files, workdir = src, name = "Resource file",
                              call = environment())
  files_expanded <- expand_dirs(files, src)
  if (!is.null(p)) {
    if (p$orderly2$strict$enabled) {
      copy_files(file.path(src, files_expanded),
                 file.path(p$path, files_expanded))
    } else {
      ## Above we're looking in the underlying source directory, here
      ## we're looking within the running directory; it's not obvious
      ## when this second case would fail, really.
      assert_file_exists_relative(files, workdir = p$path,
                                  name = "Resource file", call = environment())
    }
    outpack_packet_file_mark(p, files_expanded, "immutable")
    p$orderly2$resources <- c(p$orderly2$resources, files_expanded)
  }

  invisible(files_expanded)
}


static_orderly_resource <- function(args) {
  list(files = static_character_vector(args$files, FALSE))
}


##' Declare an artefact. By doing this you turn on a number of orderly
##' features; see Details below. You can have multiple calls to this
##' function within your orderly script.
##'
##' (1) files matching this will *not* be copied over from the src
##' directory to the draft directory unless they are also listed as a
##' resource with [orderly_resource()]. This feature is only enabled
##' if you call this function from the top level of the orderly script
##' and if it contains only string literals (no variables).
##'
##' (2) if your script fails to produce these files, then
##' [orderly_run()] will fail, guaranteeing that your task does really
##' produce the things you need it to.
##'
##' (3) within the final metadata, your artefacts will have additional
##' metadata; the description that you provide and a grouping
##'
##' @title Declare orderly artefacts
##'
##' @param description The name of the artefact
##'
##' @param files The files within this artefact
##'
##' @return Undefined
##'
##' @export
orderly_artefact <- function(description = NULL, files) {
  assert_scalar_character(description, allow_null = TRUE, call = environment())
  assert_character(files, call = environment()) # also check length >0 ?

  call <- sys.call()
  if (length(call) > 2 && !("description" %in% names(call))) {
    description_str <- deparse1(call)
    if (nchar(description_str) < 30) {
      hint <- c(
        i = "Use 'orderly_artefact(..., description = {description_str})'")
    } else {
      hint <- NULL
    }
    explanation <- paste(
      "In future versions of orderly, we will change the order of the",
      "arguments to 'orderly_artefact()' so that 'files' comes first.",
      "If you name your calls to 'description' then you will be compatible",
      "when we make this change.")
    cli::cli_warn(c(
      "Please use a named argument for the description in 'orderly_artefact()'",
      hint,
      explanation))
  }

  p <- get_active_packet()
  if (!is.null(p)) {
    artefact <- list(description = description, files = files)
    p$orderly2$artefacts <- c(p$orderly2$artefacts, list(artefact))
  }

  invisible()
}


static_orderly_artefact <- function(args) {
  list(description = static_character_vector(args$description, FALSE),
       files = static_character_vector(args$files, FALSE))
}


##' Declare a dependency on another packet
##'
##' See [orderly2::orderly_run] for some details about how search
##' options are used to select which locations packets are found from,
##' and if any data is fetched over the network. If you are running
##' interactively, this will obviously not work, so you should use
##' [orderly2::orderly_interactive_set_search_options()] to set the
##' options that this function will respond to.
##'
##' @title Declare a dependency
##'
##' @param name The name of the packet to depend on
##'
##' @param query The query to search for; often this will simply be
##'   the string `latest`, indicating the most recent version. You may
##'   want a more complex query here though.
##'
##' @inheritParams orderly_copy_files
##'
##' @return Undefined
##' @export
orderly_dependency <- function(name, query, files) {
  assert_scalar_character(name, allow_null = TRUE, call = environment())

  ctx <- orderly_context(rlang::caller_env())
  subquery <- NULL
  query <- orderly_query(query, name = name, subquery = subquery)
  search_options <- ctx$search_options %||% build_search_options()

  ## TODO: this separation of codepaths here is quite weird.  We
  ## should do the copy here and have the outpack function probably
  ## just do the metadata update.  The logic is otherwise fine I
  ## think.
  if (ctx$is_active) {
    res <- outpack_packet_use_dependency(ctx$packet, query, files,
                                         search_options = search_options,
                                         envir = ctx$envir,
                                         overwrite = TRUE)
  } else {
    res <- orderly_copy_files(
      query,
      files = files,
      dest = ctx$path,
      overwrite = TRUE,
      parameters = ctx$parameters,
      location = search_options$location,
      allow_remote = search_options$allow_remote,
      fetch_metadata = search_options$fetch_metadata,
      envir = ctx$envir,
      root = ctx$root)
  }

  cli::cli_alert_info(
    "Depending on {.pkg {res$name}} @ {.code {res$id}} (via {format(query)})")
  invisible(res)
}


static_orderly_dependency <- function(args) {
  name <- args$name
  query <- args$query
  files <- args$files

  static_name <- static_string(name)
  has_name <- !is.null(static_name) || is.null(name)

  name <- static_string(name)
  files <- fill_missing_names(static_character_vector(files, TRUE))

  if (is.character(args$query)) {
    query <- static_string(query)
  } else if (is_call(args$query, "quote") && length(args$query) == 2) {
    ## For now, just convert to string; we don't yet do much with this though
    query <- deparse1(args$query[[2]])
  } else {
    query <- NULL
  }

  if (!has_name || is.null(files) || is.null(query)) {
    return(NULL)
  }
  list(name = static_name, query = query, files = files)
}


##' Copy shared resources into a packet directory. You can use this to
##' share common resources (data or code) between multiple packets.
##' Additional metadata will be added to keep track of where the files
##' came from.  Using this function requires the shared resources
##' directory `shared/` exists at the orderly root; an error will be
##' raised if this is not configured when we attempt to fetch files.
##'
##' @title Copy shared resources into a packet directory
##'
##' @param ... The shared resources to copy. If arguments are named, the name
##'   will be the destination file while the value is the filename within the
##'   shared resource directory.
##'
##' You can use a limited form of string interpolation in the names of
##'   this argument; using `${variable}` will pick up values from
##'   `envir` and substitute them into your string.  This is similar
##'   to the interpolation you might be familiar with from
##'   `glue::glue` or similar, but much simpler with no concatenation
##'   or other fancy features supported.
##'
##' @return Invisibly, a data.frame with columns `here` (the fileames
##'   as as copied into the running packet) and `there` (the filenames
##'   within `shared/`).  As for [orderly2::orderly_resource], do not
##'   rely on the ordering where directory expansion was performed.
##'
##' @export
orderly_shared_resource <- function(...) {
  files <- validate_file_from_to(
    list(...), parent.frame(),
    name = "arguments to 'orderly_shared_resource'",
    call = environment())

  if (nrow(files) == 0) {
    cli::cli_abort("'orderly_shared_resource' requires at least one argument",
                   call = environment())
  }

  ctx <- orderly_context(rlang::caller_env())

  files <- copy_shared_resource(ctx$root_src, ctx$path, ctx$config, files,
                                environment())

  if (ctx$is_active) {
    outpack_packet_file_mark(ctx$packet, files$here, "immutable")
    ctx$packet$orderly2$shared_resources <-
      rbind(ctx$packet$orderly2$shared_resources, files)
  }

  invisible(files)
}

copy_shared_resource <- function(path_root, path_dest, config, files, call) {
  shared_dir <- "shared"
  shared_path <- file.path(path_root, shared_dir)
  if (!is_directory(shared_path)) {
    cli::cli_abort(sprintf(
      "The shared resources directory '%s' does not exist at orderly's root",
      shared_dir))
  }

  assert_file_exists_relative(files$there, workdir = shared_path,
                              name = "Shared resource file", call = call)

  files_expanded <- expand_dirs(files, shared_path)
  copy_files(fs::path(shared_path, files_expanded$there),
             fs::path(path_dest, files_expanded$here),
             overwrite = TRUE)
  files_expanded
}


static_orderly_shared_resource <- function(args) {
  fill_missing_names(unlist(lapply(args, static_string), FALSE, TRUE))
}


static_string <- function(x) {
  if (is.character(x)) {
    x
  } else if (is_call(x, "c") && length(x) == 2 && is.character(x[[2]])) {
    x[[2]]
  } else {
    NULL
  }
}


static_character_vector <- function(x, named) {
  if (is.character(x)) {
    x
  } else if (is_call(x, "c")) {
    x <- lapply(x[-1], static_character_vector, named)
    x <- if (all(vlapply(x, is.character))) unlist(x, FALSE, named) else NULL
  } else {
    x <- NULL
  }
  x
}



static_eval <- function(fn, expr, call = NULL) {
  if (is_call(expr[[1]], "::")) {
    expr[[1]] <- expr[[1]][[3]]
  }
  name <- expr[[1]]
  args <- tryCatch(
    as.list(match.call(match.fun(name), expr))[-1],
    error = function(e) {
      msg <- conditionMessage(e)
      expr_str <- deparse1(expr)
      if (grepl("alist()", msg, fixed = TRUE)) {
        hint <- c(i = "Check for a trailing comma with no argument following")
      } else {
        hint <- NULL
      }
      cli::cli_abort(c("Failed to parse call to '{name}()'",
                       ">" = expr_str,
                       hint),
                     parent = e,
                     call = call)
    })
  fn(args)
}



current <- new.env(parent = emptyenv())

get_active_packet <- function() {
  current[[getwd()]]
}


prevent_multiple_calls <- function(packet, name, call) {
  if (!is.null(packet$orderly2[[name]])) {
    entrypoint_filename <- find_entrypoint_filename(packet$orderly2$src)
    cli::cli_abort(
      c("Only one call to 'orderly2::orderly_{name}' is allowed",
        i = paste("You have already called this function earlier",
                  "in your {entrypoint_filename}")),
      call = call)
  }
}
