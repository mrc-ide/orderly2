##' Run a report.  This will create a new directory in
##' `drafts/<reportname>`, copy your declared resources there, run
##' your script and check that all expected artefacts were created.
##'
##' @section Locations used in dependency resolution:
##'
##' If your packet depends on other packets, you will want to control
##'   the locations that are used to find appropriate packets. The
##'   control for this is passed through this function and *not* as an
##'   argument to [orderly2::orderly_dependency] because this is a
##'   property of the way that a packet is created and not of a packet
##'   itself; importantly different users may have different names for
##'   their locations so it makes little sense to encode the location
##'   name into the source code. Alternatively, you want to use
##'   different locations in different contexts (initial development
##'   where you want to include local copies packets as possible
##'   dependencies vs resolving dependencies only as they would be
##'   resolved on one of your locations!
##'
##' Similarly, you might want to include packets that are known by
##'   other locations but are not currently downloaded onto this
##'   machine - pulling these packets in could take anything from
##'   seconds to hours depending on their size and the speed of your
##'   network connection (but *not* pulling in the packets could mean
##'   that your packet fails to run).
##'
##' To allow for control over this you can pass in an argument
##'   `search_options`, which is a [orderly2::orderly_search_options]
##'   object, and allows control over the names of the locations to
##'   use, whether metadata should be refreshed before we pull
##'   anything and if packets that are not currently downloaded should
##'   be considered candidates.
##'
##' This has no effect when running interactively, in which case you
##'   can specify the search options (root specific) with
##'   [orderly2::orderly_interactive_set_search_options]
##'
##' @section Which packets might be selected from locations?:
##'
##' The `search_options` argument controls where outpack searches for
##'   packets with the given query and if anything might be moved over
##'   the network (or from one outpack archive to another). By default
##'   everything is resolved locally only; that is we can only depend
##'   on packets that are unpacked within our current archive.  If you
##'   pass a `search_options` argument that contains `allow_remote =
##'   TRUE` (see [orderly2::orderly_search_options] then packets
##'   that are known anywhere are candidates for using as dependencies
##'   and *if needed* we will pull the resolved files from a remote
##'   location. Note that even if the packet is not locally present
##'   this might not be needed - if you have the same content anywhere
##'   else in an unpacked packet we will reuse the same content
##'   without re-fetching.
##'
##' If `pull_metadata = TRUE`, then we will refresh location metadata
##'   before pulling, and the `location` argument controls which
##'   locations are pulled from.
##'
##' @section Equivalence to the old `use_draft` option:
##'
##' The above location handling generalises orderly (v1)'s old
##'   `use_draft` option, in terms of the `location` argument to
##'   orderly2::orderly_search_options`:
##'
##' * `use_draft = TRUE` is `location = "local"`
##' * `use_draft = FALSE` is `location = c(...)` where you should provide
##'   all locations *except* local
##'   (`setdiff(orderly2::outpack_location_list(), "local")`)
##' * `use_draft = "newer"` is `location = NULL`
##'
##' (this last option was the one most people preferred so is the new
##'   default behaviour). In addition, you could resolve dependencies
##'   as they currently exist on production right now with the options:
##'
##' ```
##' location = "production", pull_metadata = TRUE, require_unpacked = FALSE
##' ```
##'
##' which updates your current metadata from production, then runs
##'   queries against only packets known on that remote, then depends
##'   on them even if you don't (yet) have them locally.  This
##'   functionality was never available in orderly version 1, though
##'   we had intended to support it.
##'
##' @title Run a report
##'
##' @param name Name of the report to run
##'
##' @param parameters Parameters passed to the report. A named list of
##'   parameters declared in the `orderly.yml`.  Each parameter
##'   must be a scalar character, numeric, integer or logical.
##'
##' @param envir The environment that will be used to evaluate the
##'   report script; by default we use the global environment, which
##'   may not always be what is wanted.
##'
##' @param logging_console Optional logical to control printing logs
##'   to the console, overriding any default configuration set at the
##'   root.
##'
##' @param logging_threshold Optional logging threshold to control the
##'   amount of detail in logs printed during running, overriding any
##'   default configuration set at the root. If given, must be one of
##'   `info`, `debug` or `trace` (in increasing order of
##'   verbosity). Practically this has no effect at present as we've
##'   not added any fine-grained logging.
##'
##' @param search_options Optional control over locations, when used with
##'   [orderly2::orderly_dependency]; see Details.
##'
##' @param root The path to an orderly root directory, or `NULL`
##'   (the default) to search for one from the current working
##'   directory if `locate` is `TRUE`.
##'
##' @param locate Logical, indicating if the configuration should be
##'   searched for.  If `TRUE` and `config` is not given,
##'   then orderly looks in the working directory and up through its
##'   parents until it finds an `.outpack` directory
##'
##' @return The id of the created report (a string)
##'
##' @export
orderly_run <- function(name, parameters = NULL, envir = NULL,
                        logging_console = NULL, logging_threshold = NULL,
                        search_options = NULL, root = NULL, locate = TRUE) {
  root <- orderly_root(root, locate, environment())
  validate_orderly_directory(name, root, environment())

  envir <- envir %||% .GlobalEnv
  assert_is(envir, "environment")

  src <- file.path(root$path, "src", name)
  dat <- orderly_read(src)

  parameters <- check_parameters(parameters, dat$parameters,
                                 environment())

  orderly_validate(dat, src)

  id <- outpack_id()

  stopifnot(fs::is_absolute_path(root$path))
  path <- file.path(root$path, "draft", name, id)
  fs::dir_create(path)
  ## Slightly peculiar formulation here; we're going to use 'path' as
  ## the key for storing the active packet and look it up later with
  ## getwd(); this means we definitely will match. Quite possibly path
  ## normalisation would do the same fix. This will likely change in
  ## future if we need to support working within subdirectories of
  ## path too, in which case we use something from fs.
  path <- withr::with_dir(path, getwd())

  if (dat$strict$enabled) {
    inputs_info <- NULL
    fs::file_copy(file.path(src, "orderly.R"), path)
  } else {
    inputs_info <-
      copy_resources_implicit(src, path, dat$resources, dat$artefacts)
  }

  p <- outpack_packet_start(path, name, parameters = parameters,
                                     id = id, logging_console = logging_console,
                                     logging_threshold = logging_threshold,
                                     root = root$outpack)
  withCallingHandlers({
    outpack_packet_file_mark(p, "orderly.R", "immutable")
    p$orderly2 <- list(config = root$config, envir = envir, src = src,
                       strict = dat$strict, inputs_info = inputs_info,
                       search_options = search_options)
    current[[path]] <- p
    on.exit(current[[path]] <- NULL, add = TRUE, after = TRUE)

    if (!is.null(parameters)) {
      list2env(parameters, envir)
    }

    result <- outpack_packet_run(p, "orderly.R", envir)
    orderly_packet_cleanup_success(p)
  }, error = function(e) {
    orderly_packet_cleanup_failure(p)
  })
  id
}


custom_metadata <- function(dat) {
  global <- dat$global_resources %||% list()
  role <- data_frame(
    path = c(dat$resources, global$here),
    role = c(rep_along("resource", dat$resources),
             rep_along("global", global$here)))
  artefacts <- lapply(dat$artefacts, function(x) {
    list(description = scalar(x$description),
         paths = x$files)
  })

  if (is.null(dat$plugins)) {
    plugins <- NULL
  } else {
    plugins <- list()
    for (nm in names(dat$plugins)) {
      plugins[[nm]] <- dat$config$plugins[[nm]]$serialise(dat$plugins[[nm]])
    }
  }

  if (is.null(dat$description$custom)) {
    custom <- NULL
  } else {
    custom <- lapply(dat$description$custom, scalar)
  }
  description <- list(display = scalar(dat$description$display),
                      long = scalar(dat$description$long),
                      custom = custom)

  session <- orderly_session_info(utils::sessionInfo())

  list(artefacts = artefacts,
       role = role,
       description = description,
       global = global,
       session = session,
       plugins = plugins)
}


check_produced_artefacts <- function(path, artefacts) {
  if (is.null(artefacts)) {
    return()
  }
  expected <- unlist(lapply(artefacts, "[[", "files"), FALSE, FALSE)
  found <- file_exists(expected, workdir = path)
  if (any(!found)) {
    stop("Script did not produce expected artefacts: ",
         paste(squote(expected[!found]), collapse = ", "))
  }

  for (i in seq_along(artefacts)) {
    artefacts[[i]]$files <- expand_dirs(artefacts[[i]]$files, path)
  }

  artefacts
}


## Same logic as orderly1; has worked well in practice. We might want
## to relax additional parameters here later, but that requires some
## thinking about what to do with them (do they get passed into the
## environment etc or not? do they get validated?)
check_parameters <- function(given, spec, call) {
  if (length(given) > 0) {
    assert_named(given, unique = TRUE)
  }

  if (length(given) > 0 && is.null(spec)) {
    cli::cli_abort(
      c("Parameters given, but none declared",
        i = "Did you forget 'orderly2::orderly_parameter()"),
      call = call)
  }

  is_required <- vlapply(spec, is.null)
  msg <- setdiff(names(spec)[is_required], names(given))
  extra <- setdiff(names(given), names(spec))

  if (length(msg) > 0L) {
    cli::cli_abort(
      error_near_match(
        "Missing parameters",
        msg,
        "You have extra parameters, possibly misspelt?",
        "could be your",
        extra),
      call = call)
  }

  if (length(extra) > 0L) {
    unused <- setdiff(names(spec), names(given))
    cli::cli_abort(
      error_near_match(
        "Extra parameters",
        extra,
        "You have extra parameters, possibly misspelt?",
        "should perhaps be",
        unused),
      call = call)
  }

  if (length(spec) == 0) {
    return(NULL)
  }

  check_parameter_values(given, FALSE)

  use_default <- setdiff(names(spec), names(given))
  if (length(use_default) > 0) {
    given[use_default] <- spec[use_default]
  }
  given[names(spec)]
}


check_parameter_values <- function(given, defaults) {
  name <- if (defaults) "parameter defaults" else "parameters"
  if (defaults) {
    given <- given[!vlapply(given, is.null)]
  }

  nonscalar <- lengths(given) != 1
  if (any(nonscalar)) {
    stop(sprintf(
      "Invalid %s: %s - must be scalar",
      name, paste(squote(names(nonscalar[nonscalar])), collapse = ", ")))
  }

  err <- !vlapply(given, is_simple_atomic)
  if (any(err)) {
    stop(sprintf(
      "Invalid %s: %s - must be character, numeric or logical",
      name, paste(squote((names(err[err]))), collapse = ", ")))
  }
}


check_parameters_interactive <- function(env, spec) {
  if (length(spec) == 0) {
    return()
  }

  is_required <- vlapply(spec, is.null)

  msg <- setdiff(names(spec)[is_required], names(env))
  if (length(msg) > 0L) {
    ## This will change, but we'll need some interactive prompting
    ## better done in another ticket. See
    ## https://github.com/r-lib/cli/issues/228 and
    ## https://github.com/r-lib/cli/issues/488 for context here.
    ##
    ## There will be other "interactive mode" functions too that we'll
    ## try and get a unified interface on.
    stop("Missing parameters: ", paste(squote(msg), collapse = ", "))
  }

  ## Set any missing values into the environment:
  list2env(spec[setdiff(names(spec), names(env))], env)

  ## We might need a slightly better error message here that indicates
  ## that we're running in a pecular mode so the value might just have
  ## been overwritten
  found <- set_names(lapply(names(spec), function(v) env[[v]]), names(spec))
  check_parameter_values(found[!vlapply(found, is.null)], FALSE)
  invisible(found)
}


check_files_strict <- function(path, known, artefacts) {
  all_known <- c(unlist(lapply(known, names), FALSE, FALSE),
                 unlist(lapply(artefacts, "[[", "files"), TRUE, FALSE))
  all_files_end <- dir_ls_local(path, all = TRUE, type = "file")
  unknown <- setdiff(all_files_end, all_known)
  if (length(unknown) > 0) {
    ## TODO: better once we have logging merged, I think
    ##
    ## TODO: provide workaround to ignore too (either to exclude
    ## or ignore); see VIMC-7093
    message(paste(
      "orderly produced unexpected files:",
      sprintf("  - %s", unknown),
      "Consider using orderly2::orderly_artefact() to describe them",
      sep = "\n"))
  }
}


check_files_relaxed <- function(path, inputs_info) {
  inputs_info_end <- withr::with_dir(path, fs::file_info(inputs_info$path))
  i <- inputs_info_end$size != inputs_info$size |
    inputs_info_end$modification_time != inputs_info$modification_time
  if (any(i)) {
    message(paste(
      "inputs modified; these are probably artefacts:",
      sprintf("  - %s", inputs_info$path[i]),
      "Consider using orderly2::orderly_artefact() to describe them",
      sep = "\n"))
  }
}


copy_resources_implicit <- function(src, dst, resources, artefacts) {
  to_copy <- dir_ls_local(src, all = TRUE, type = "file", recurse = TRUE)
  ## Here we just seek to exclude any artefacts that are not
  ## explicitly listed as resources, if they already exist.
  artefact_files <- unlist(lapply(artefacts, "[[", "files"), TRUE, FALSE)
  if (length(artefact_files) > 0) {
    i <- file_exists(artefact_files, workdir = src)
    if (any(i)) {
      exclude <- setdiff(
        expand_dirs(artefact_files[i], workdir = src),
        expand_dirs(resources, workdir = src))
      to_copy <- setdiff(to_copy, exclude)
    }
  }
  fs::dir_create(unique(file.path(dst, dirname(to_copy))))
  fs::file_copy(file.path(src, to_copy),
                file.path(dst, to_copy),
                overwrite = TRUE)
  withr::with_dir(dst, fs::file_info(to_copy))
}


## All the cleanup bits for the happy exit (where we do the validation etc)
orderly_packet_cleanup_success <- function(p) {
  path <- p$path

  plugin_run_cleanup(path, p$orderly2$config$plugins)
  p$orderly2$artefacts <- check_produced_artefacts(path, p$orderly2$artefacts)
  if (p$orderly2$strict$enabled) {
    check_files_strict(path, p$files, p$orderly2$artefacts)
  } else {
    check_files_relaxed(path, p$orderly2$inputs_info)
  }
  custom_metadata_json <- to_json(custom_metadata(p$orderly2), NULL)
  schema <- custom_metadata_schema(p$orderly2$config)
  outpack_packet_add_custom(p, "orderly", custom_metadata_json, schema)

  outpack_packet_end(p)
  unlink(path, recursive = TRUE)
}


orderly_packet_cleanup_failure <- function(p) {
  ignore_errors(plugin_run_cleanup(p$path, p$orderly2$config$plugins))
  custom_metadata_json <- to_json(custom_metadata(p$orderly2))
  outpack_packet_add_custom(p, "orderly", custom_metadata_json)
  outpack_packet_end(p, insert = FALSE)
}


validate_orderly_directory <- function(name, root, call) {
  assert_scalar_character(name)
  if (!file_exists(file.path(root$path, "src", name, "orderly.R"))) {
    src <- file.path(root$path, "src", name)
    err <- sprintf("Did not find orderly report '%s'", name)
    if (!file_exists(src)) {
      detail <- sprintf("The path 'src/%s' does not exist", name)
    } else if (is_directory(src)) {
      detail <- sprintf(
        "The path 'src/%s' exists but does not contain 'orderly.R'",
        name)
    } else {
      detail <- sprintf(
        "The path 'src/%s' exists but is not a directory",
        name)
    }
    err <- c(err, x = detail)
    near <- near_match(name, orderly_list_src(root$path))
    if (length(near) > 0) {
      hint <- sprintf("Did you mean %s",
                      paste(squote(near), collapse = ", "))
      err <- c(err, i = hint)
    }
    err <- c(err, i = sprintf("Looked relative to orderly root at '%s'",
                              root$path))
    cli::cli_abort(err, call = call)
  }
}


orderly_session_info <- function(info) {
  ## TODO: we might also add some host information here too; orderly
  ## has some of that for us.
  assert_is(info, "sessionInfo")
  platform <- list(version = scalar(info$R.version$version.string),
                   os = scalar(info$running),
                   system = scalar(info$R.version$system))

  ## TODO: Where available, we might also include Remotes info, or
  ## whatever renv uses?
  pkgs <- c(info$otherPkgs, info$loadedOnly)
  n <- c(length(info$otherPkgs), length(info$loadedOnly))
  packages <- data_frame(
    package = vcapply(pkgs, "[[", "Package", USE.NAMES = FALSE),
    version = vcapply(pkgs, "[[", "Version", USE.NAMES = FALSE),
    attached = rep(c(TRUE, FALSE), n))

  list(platform = platform,
       packages = packages)
}
