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
##' within your file, though this is not enforced! Typically you'd put
##' it very close to the top, though the order does not really matter.
##' Parameters are scalar atomic values (e.g. a string, number or
##' boolean) and defaults must be present literally (i.e., they may
##' not come from a variable itself). Provide `NULL` if you do not
##' have a default, in which case this parameter will be required.
##'
##' @title Declare orderly parameters
##'
##' @param ... Any number of parameters
##'
##' @return Undefined
##'
##' @export
orderly_parameters <- function(...) {
  p <- get_active_packet()
  if (is.null(p)) {
    call <- environment()
    envir <- parent.frame()
    pars <- static_orderly_parameters(list(...), call)
    check_parameters_interactive(envir, pars, call)
  }

  invisible()
}


static_orderly_parameters <- function(args, call) {
  if (length(args) == 0L) {
    return(NULL)
  }
  assert_named(args, unique = TRUE, name = "Arguments to 'orderly_parameters'")
  check_parameter_values(args, TRUE, call)

  args
}


current_orderly_parameters <- function(src, envir) {
  dat <- orderly_read(src)
  pars <- static_orderly_parameters(dat$parameters)
  values <- check_parameters_interactive(envir, pars, NULL)
  values
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
  if (!is.null(display)) {
    assert_scalar_character(display)
  }
  if (!is.null(long)) {
    assert_scalar_character(long)
  }
  if (!is.null(custom)) {
    assert_named(custom)
    assert_is(custom, "list")
    for (i in names(custom)) {
      assert_simple_scalar_atomic(custom[[i]], sprintf("custom$%s", i))
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
##' @param files Any number of names of files
##'
##' @return Invisibly, a character vector of resources included by the
##'   call. Don't rely on the order of these files if they are
##'   expanded from directories, as this is likely platform dependent.
##'
##' @export
orderly_resource <- function(files) {
  ## TODO: an error here needs to throw a condition that we can easily
  ## handle and or defer; that's not too hard to do though - convert
  ## the error into something with a special class, perhaps make it a
  ## warning in normal R and then register a handler for it in the
  ## main run.
  assert_character(files)

  p <- get_active_packet()
  if (is.null(p)) {
    assert_file_exists(files)
    files_expanded <- expand_dirs(files, ".")
  } else {
    src <- p$orderly2$src
    assert_file_exists(files, workdir = src)
    files_expanded <- expand_dirs(files, src)
    if (p$orderly2$strict$enabled) {
      copy_files(src, p$path, files_expanded)
    } else {
      assert_file_exists(files, workdir = p$path)
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
orderly_artefact <- function(description, files) {
  assert_scalar_character(description)
  assert_character(files) # also check length >0 ?

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
  assert_scalar_character(name)

  ctx <- orderly_context(rlang::caller_env())
  subquery <- NULL
  query <- orderly_query(query, name = name, subquery = subquery)
  search_options <- as_orderly_search_options(ctx$search_options)
  if (ctx$is_active) {
    id <- outpack_packet_use_dependency(ctx$packet, query, files,
                                        search_options = search_options,
                                        envir = ctx$envir,
                                        overwrite = TRUE)
    cli::cli_alert_info(
      "Depending on {.pkg {name}} @ {.code {id}} (via {format(query)})")
  } else {
    ## TODO: also echo here I think
    orderly_copy_files(query, files = files, dest = ctx$path, overwrite = TRUE,
                       parameters = ctx$parameters, options = search_options,
                       envir = ctx$envir, root = ctx$root)
  }

  invisible()
}


static_orderly_dependency <- function(args) {
  name <- args$name
  query <- args$query
  files <- args$files

  name <- static_string(name)
  files <- static_character_vector(files, TRUE)

  if (is.character(args$query)) {
    query <- static_string(query)
  } else if (is_call(args$query, "quote") && length(args$query) == 2) {
    ## For now, just convert to string; we don't yet do much with this though
    query <- deparse1(args$query[[2]])
  } else {
    query <- NULL
  }

  if (is.null(name) || is.null(files) || is.null(query)) {
    return(NULL)
  }
  list(name = name, query = query, files = files)
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
##' @param ... Named arguments corresponding to shared resources to
##'   copy. The name will be the destination filename, while the value
##'   is the filename within the shared resource directory.
##'
##' @return Undefined
##' @export
orderly_shared_resource <- function(...) {
  files <- validate_shared_resource(list(...), environment())
  ctx <- orderly_context(rlang::caller_env())

  files <- copy_shared_resource(ctx$root, ctx$path, ctx$config, files)
  if (ctx$is_active) {
    outpack_packet_file_mark(ctx$packet, files$here, "immutable")
    ctx$packet$orderly2$shared_resources <-
      rbind(ctx$packet$orderly2$shared_resources, files)
  }

  invisible()
}


validate_shared_resource <- function(args, call) {
  if (length(args) == 0) {
    cli::cli_abort("'orderly_shared_resource' requires at least one argument",
                   call = call)
  }
  assert_named(args, unique = TRUE)
  is_invalid <- !vlapply(args, function(x) is.character(x) && length(x) == 1)
  if (any(is_invalid)) {
    cli::cli_abort(
      sprintf(
        "Invalid shared resource %s: entries must be strings",
        paste(squote(names(args)[is_invalid]), collapse = ", ")),
      call = call)
  }
  list_to_character(args)
}


copy_shared_resource <- function(path_root, path_dest, config, files) {
  ## This used to be configurable in orderly1, but almost everyone
  ## just kept it as 'global'. We might make it configurable later.
  shared_dir <- "shared"
  shared_path <- file.path(path_root, shared_dir)
  if (!is_directory(shared_path)) {
    cli::cli_abort(sprintf(
      "The shared resources directory '%s' does not exist at orderly's root",
      shared_dir))
  }

  here <- names(files)
  there <- unname(files)

  assert_file_exists(
    there, workdir = shared_path,
    name = sprintf("Shared resources in '%s'", shared_path))
  src <- file.path(shared_path, there)
  dst <- file.path(path_dest, here)

  is_dir <- is_directory(file.path(shared_path, there))
  fs::dir_create(file.path(path_dest, dirname(here)))
  if (any(is_dir)) {
    fs::dir_copy(src[is_dir], dst[is_dir])
    ## Update the names that will be used in the metadata:
    files <- lapply(src[is_dir], dir)
    here <- replace_ragged(here, is_dir, Map(file.path, here[is_dir], files))
    there <- replace_ragged(there, is_dir, Map(file.path, there[is_dir], files))
  }
  if (any(!is_dir)) {
    fs::file_copy(src[!is_dir], dst[!is_dir], overwrite = TRUE)
  }

  data_frame(here = here, there = there)
}


static_orderly_shared_resource <- function(args) {
  unlist(lapply(args, static_character_vector, TRUE), FALSE, TRUE)
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



static_eval <- function(fn, call) {
  if (is_call(call[[1]], "::")) {
    call[[1]] <- call[[1]][[3]]
  }
  args <- as.list(match.call(match.fun(call[[1]]), call)[-1])
  fn(args)
}


current <- new.env(parent = emptyenv())

get_active_packet <- function() {
  current[[getwd()]]
}


prevent_multiple_calls <- function(packet, name, call) {
  if (!is.null(packet$orderly2[[name]])) {
    cli::cli_abort(
      c("Only one call to 'orderly2::orderly_{name}' is allowed",
        i = "You have already called this function earlier in your orderly.R"),
      call = call)
  }
}
