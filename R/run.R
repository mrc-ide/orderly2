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
##' To allow for control over this you can pass in an arguments to
##'   control the names of the locations to use, whether metadata
##'   should be refreshed before we pull anything and if packets that
##'   are not currently downloaded should be considered candidates.
##'
##' This has no effect when running interactively, in which case you
##'   can specify the search options (root specific) with
##'   [orderly2::orderly_interactive_set_search_options]
##'
##' @section Which packets might be selected from locations?:
##'
##' The arguments `location`, `allow_remote` and `fetch_metadata`
##'   control where outpack searches for packets with the given query
##'   and if anything might be moved over the network (or from one
##'   outpack archive to another). By default everything is resolved
##'   locally only; that is we can only depend on packets that are
##'   unpacked within our current archive.  If you pass `allow_remote
##'   = TRUE`, then packets that are known anywhere are candidates for
##'   using as dependencies and *if needed* we will pull the resolved
##'   files from a remote location. Note that even if the packet is
##'   not locally present this might not be needed - if you have the
##'   same content anywhere else in an unpacked packet we will reuse
##'   the same content without re-fetching.
##'
##' If `fetch_metadata = TRUE`, then we will refresh location metadata
##'   before pulling, and the `location` argument controls which
##'   locations are pulled from.
##'
##' @section Equivalence to the old `use_draft` option:
##'
##' The above location handling generalises orderly (v1)'s old
##'   `use_draft` option, in terms of the new `location` argument:
##'
##' * `use_draft = TRUE` is `location = "local"`
##' * `use_draft = FALSE` is `location = c(...)` where you should provide
##'   all locations *except* local
##'   (`setdiff(orderly2::orderly_location_list(), "local")`)
##' * `use_draft = "newer"` is `location = NULL`
##'
##' (this last option was the one most people preferred so is the new
##'   default behaviour). In addition, you could resolve dependencies
##'   as they currently exist on production right now with the options:
##'
##' ```
##' location = "production", fetch_metadata = TRUE
##' ```
##'
##' which updates your current metadata from production, then runs
##'   queries against only packets known on that remote, then depends
##'   on them even if you don't (yet) have them locally.  This
##'   functionality was never available in orderly version 1, though
##'   we had intended to support it.
##'
##' @section Running with a source tree separate from outpack root:
##'
##' Sometimes it is useful to run things from a different place on
##'   disk to your outpack root. We know of two cases where this has
##'   come up:
##'
##' * when running reports within a runner on a server, we make a
##'   clean clone of the source tree at a particular git reference
##'   into a new temporary directory and then run the report there,
##'   but have it insert into an orderly repo at a fixed and
##'   non-temporary location.
##' * we have a user for whom it is more convenient torun their report
##'   on a hard drive but store the archive and metadata on a (larger)
##'   shared drive.
##'
##' In the first instance, we have a source path at `<src>` which
##'   contains the file `orderly_config.yml` and the directory `src/`
##'   with our source reports, and a separate path `<root>` which
##'   contains the directory `.outpack/` with all the metadata - it
##'   may also have an unpacked archive, and a `.git/` directory
##'   depending on the configuration. (Later this will make more sense
##'   once we support a "bare" outpack layout.)
##'
##' @section Manually setting report source directory:
##'
##' To manually set the report source directory, you will need to set
##'   the path of the directory as the `ORDERLY_REPORT_SRC` environment
##'   variable.
##'
##' @title Run a report
##'
##' @param name Name of the report to run. Any leading `./` `src/` or
##'   trailing `/` path parts will be removed (e.g., if added by
##'   autocomplete).
##'
##' @param parameters Parameters passed to the report. A named list of
##'   parameters declared in the `orderly.yml`.  Each parameter
##'   must be a scalar character, numeric, integer or logical.
##'
##' @param envir The environment that will be used to evaluate the
##'   report script; by default we use the global environment, which
##'   may not always be what is wanted.
##'
##' @param echo Optional logical to control printing output from
##'   `source()` to the console.
##'
##' @inheritParams orderly_search
##'
##' @param search_options **DEPRECATED**. Please don't use this any
##'   more, and instead use the arguments `location`, `allow_remote`
##'   and `fetch_metadata` directly.
##'
##' @param root The path to the root directory, or `NULL` (the
##'   default) to search for one from the current working
##'   directory. This function **does** require that the directory is
##'   configured for orderly, and not just outpack (see
##'   [orderly2::orderly_init] for details).
##'
##' @return The id of the created report (a string)
##'
##' @export
##' @examples
##' # Create a simple example:
##' path <- orderly2::orderly_example("default")
##'
##' # Run the 'data' task:
##' orderly2::orderly_run("data", root = path)
##'
##' # After running, a finished packet appears in the archive:
##' fs::dir_tree(path)
##'
##' # and we can query the metadata:
##' orderly2::orderly_metadata_extract(name = "data", root = path)
##'
##' # Cleanup
##' fs::dir_delete(path)
orderly_run <- function(name, parameters = NULL, envir = NULL, echo = TRUE,
                        location = NULL, allow_remote = NULL,
                        fetch_metadata = FALSE, search_options = NULL,
                        root = NULL) {
  env_root_src <- Sys.getenv("ORDERLY_SRC_ROOT", NA_character_)
  root <- root_open(root, require_orderly = is.na(env_root_src),
                    call = environment())
  compatibility_fix_options(search_options, "orderly_run")

  if (is.na(env_root_src)) {
    root_src <- root$path
  } else {
    root_src <- orderly_src_root(env_root_src)
  }

  name <- validate_orderly_directory(name, root_src, environment())

  envir <- envir %||% .GlobalEnv
  assert_is(envir, "environment", call = environment())

  src <- file.path(root_src, "src", name)
  dat <- orderly_read(src, environment())
  entrypoint_filename <- dat$entrypoint_filename
  parameters <- check_parameters(parameters, dat$parameters, environment())
  orderly_validate(dat, src)

  search_options <- build_search_options(location = location,
                                         allow_remote = allow_remote,
                                         fetch_metadata = fetch_metadata)

  id <- outpack_id()
  path <- file.path(root_src, "draft", name, id)
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
    copy_files(file.path(src, entrypoint_filename),
               file.path(path, entrypoint_filename))
  } else {
    inputs_info <- copy_resources_implicit(src, path, dat$resources,
                                           dat$artefacts)
  }
  p <- outpack_packet_start(path, name, parameters = parameters,
                            id = id, root = root)
  outpack_packet_file_mark(p, entrypoint_filename, "immutable")
  p$orderly2 <- list(config = root$config$orderly, envir = envir, src = src,
                     root = root_src, strict = dat$strict,
                     inputs_info = inputs_info, search_options = search_options)
  current[[path]] <- p
  on.exit(current[[path]] <- NULL, add = TRUE, after = TRUE)
  if (!is.null(parameters)) {
    list2env(parameters, envir)
  }

  info <- session_global_state()
  top <- rlang::current_env() # not quite right, but better than nothing
  local <- new.env(parent = emptyenv())
  local$warnings <- collector()
  res <- rlang::try_fetch(
    withr::with_dir(path, source_echo(entrypoint_filename, envir, echo)),
    warning = function(e) {
      local$warnings$add(e)
      rlang::zap()
    },
    error = function(e) {
      if (is.null(e$trace)) {
        e$trace <- rlang::trace_back(top)
      }
      local$error <- e
      NULL
    })

  info_end <- withr::with_dir(path, check_session_global_state(info))
  success <- is.null(local$error) && info_end$success

  if (success) {
    cli::cli_alert_success("Finished running {.file {entrypoint_filename}}")
  } else {
    cli::cli_alert_danger("Error running {.file {entrypoint_filename}}")
  }

  if (length(local$warnings$get()) > 0) {
    str_warnings <- vcapply(local$warnings$get(), conditionMessage)
    cli::cli_alert_warning("{length(str_warnings)} warning{?s} found:")
    cli::cli_li(str_warnings)
  }

  if (success) {
    orderly_packet_cleanup_success(p, environment())
  } else if (is.null(local$error)) {
    detail <- info_end$message
    cli::cli_abort(
      c("Script failed to balance a global resource stack",
        set_names(detail, rep("x", length(detail)))))
  } else {
    e <- local$error
    ## TODO: consider saving 'e' as a file within the directory.
    orderly_packet_cleanup_failure(p)
    cli::cli_abort("Failed to run report",
                   parent = e,
                   explanation = e$explanation)
  }

  id
}


custom_metadata <- function(dat) {
  entrypoint_filename <- find_entrypoint_filename(dat$src)
  shared <- dat$shared_resources %||% list()
  role <- data_frame(
    path = c(entrypoint_filename,  dat$resources, shared$here),
    role = c("orderly",
             rep_along("resource", dat$resources),
             rep_along("shared", shared$here)))
  artefacts <- lapply(dat$artefacts, function(x) {
    list(description = scalar(x$description),
         paths = x$files)
  })

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
       shared = shared,
       session = session)
}


check_produced_artefacts <- function(path, artefacts, call) {
  if (is.null(artefacts)) {
    return()
  }
  expected <- unlist(lapply(artefacts, "[[", "files"), FALSE, FALSE)
  found <- file_exists(expected, workdir = path)
  if (any(!found)) {
    cli::cli_abort(c("Script did not produce expected artefacts:",
                     set_names(expected[!found], rep("*", sum(!found)))),
                   call = call)
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
    assert_named(given, unique = TRUE, call = call)
  }

  if (length(given) > 0 && is.null(spec)) {
    cli::cli_abort(
      c("Parameters given, but none declared",
        i = "Did you forget 'orderly2::orderly_parameters()"),
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

  check_parameter_values(given, FALSE, call)

  use_default <- setdiff(names(spec), names(given))
  if (length(use_default) > 0) {
    given[use_default] <- spec[use_default]
  }
  given[names(spec)]
}


check_parameter_values <- function(given, is_defaults, call) {
  if (is_defaults) {
    given <- given[!vlapply(given, is.null)]
  }

  nonscalar <- lengths(given) != 1
  too_complex <- !vlapply(given, function(x) all(is_simple_atomic(x)))
  err <- nonscalar | too_complex

  if (any(err)) {
    name <- if (is_defaults) "default" else "value"
    title <- "Invalid parameter {name}{cli::qty(sum(err))}{?s}"
    if (any(nonscalar)) {
      msg_nonscalar <- c(
        "x" = "Values must be scalar, but were not for:",
        set_names(names(given)[nonscalar], rep("*", sum(nonscalar))))
    } else {
      msg_nonscalar <- NULL
    }
    if (any(too_complex)) {
      msg_too_complex <- c(
        "x" = "Values must be character, numeric or boolean, but were not for:",
        set_names(names(given)[too_complex], rep("*", sum(too_complex))))
    } else {
      msg_too_complex <- NULL
    }
    cli::cli_abort(c(title, msg_nonscalar, msg_too_complex), call = call)
  }
}


check_parameters_interactive <- function(envir, spec, target, call) {
  if (length(spec) == 0) {
    pars <- strict_list(.name = "parameters")
    if (!is.null(target)) {
      envir[[target]] <- pars
    }
    return(pars)
  }

  if (is.null(target)) {
    ## This is the old style version; we try and find parameters in
    ## the environment:
    required <- names(spec)[vlapply(spec, is.null)]
    get_missing_parameters_interactive(required, envir, call)
    list2env(spec[setdiff(names(spec), names(envir))], envir)

    ## We might need a slightly better error message here that indicates
    ## that we're running in a pecular mode so the value might just have
    ## been overwritten
    found <- set_names(lapply(names(spec), function(v) envir[[v]]), names(spec))
    check_parameter_values(found[!vlapply(found, is.null)], FALSE, call)

    as_strict_list(found, name = "parameters")
  } else {
    prev <- envir[[target]]
    if (inherits(prev, "strict_list")) {
      if (identical(names(prev), names(spec))) {
        cli::cli_alert_info("Reusing previous parameters in '{target}'")
        ## TODO: show parameters here
        return(prev)
      }
      cli::cli_alert_warning(
        "Ignoring parameters in '{target}' which are different to this report")
    }
    pars <- prompt_parameters_interactive(spec, call)
    envir[[target]] <- pars
    pars
  }
}


check_files_strict <- function(path, known, artefacts) {
  all_known <- c(unlist(lapply(known, names), FALSE, FALSE),
                 unlist(lapply(artefacts, "[[", "files"), TRUE, FALSE))
  all_files_end <- dir_ls_local(path, all = TRUE, type = "file")
  unknown <- setdiff(all_files_end, all_known)
  if (length(unknown) > 0) {
    ## TODO: provide workaround to ignore too (either to exclude
    ## or ignore); see VIMC-7093
    cli::cli_alert_warning("Report produced unexpected files:")
    cli::cli_ul(unknown)
    cli::cli_alert_info(paste("These are probably artefacts, consider using",
                              "orderly2::orderly_artefact to describe them"))
  }
}


check_files_relaxed <- function(path, inputs_info) {
  inputs_info_end <- withr::with_dir(path, fs::file_info(inputs_info$path))
  deleted <- is.na(inputs_info_end$type)

  modified <- !deleted &
    (inputs_info_end$size != inputs_info$size |
     inputs_info_end$modification_time != inputs_info$modification_time)

  if (any(modified)) {
    cli::cli_alert_warning("The following inputs were modified by the report:")
    cli::cli_ul(inputs_info$path[modified])
    cli::cli_alert_info(paste("These are probably artefacts, consider using",
                              "orderly2::orderly_artefact to describe them"))
  }
  if (any(deleted)) {
    cli::cli_alert_warning("The following inputs were deleted by the report:")
    cli::cli_ul(inputs_info$path[deleted])
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

  copy_files(file.path(src, to_copy),
             file.path(dst, to_copy))

  withr::with_dir(dst, fs::file_info(to_copy))
}


## All the cleanup bits for the happy exit (where we do the validation etc)
orderly_packet_cleanup_success <- function(p, call = NULL) {
  path <- p$path

  plugin_run_cleanup(path, p$orderly2$config$plugins)
  p$orderly2$artefacts <- check_produced_artefacts(path, p$orderly2$artefacts,
                                                   call)
  if (p$orderly2$strict$enabled) {
    check_files_strict(path, p$files, p$orderly2$artefacts)
  } else {
    check_files_relaxed(path, p$orderly2$inputs_info)
  }

  orderly_packet_add_metadata(p)
  outpack_packet_end(p)
  fs::dir_delete(path)
}


orderly_packet_cleanup_failure <- function(p) {
  ignore_errors(plugin_run_cleanup(p$path, p$orderly2$config$plugins))
  ignore_errors(orderly_packet_add_metadata(p))
  outpack_packet_end(p, insert = FALSE)
}


orderly_packet_add_metadata <- function(p) {
  json <- to_json(custom_metadata(p$orderly2), "orderly/orderly.json")
  outpack_packet_add_custom(p, "orderly", json)
  for (nm in names(p$plugins)) {
    cfg <- p$orderly2$config$plugins[[nm]]
    json_p <- to_json(cfg$serialise(p$plugins[[nm]]), cfg$schema)
    outpack_packet_add_custom(p, nm, json_p)
  }
}


validate_orderly_directory <- function(name, root_path, call) {
  assert_scalar_character(name, call = call)
  re <- "^(./)*(src/)?(.+?)/?$"
  name <- sub(re, "\\3", name)
  src <- file.path(root_path, "src", name)

  is_error <- FALSE
  if (!file_exists(src)) {
    is_error <- TRUE
    detail <- sprintf("The path 'src/%s' does not exist", name)
  } else if (!is_directory(src)) {
    is_error <- TRUE
    detail <- sprintf(
      "The path 'src/%s' exists but is not a directory",
      name
    )
  }

  if (is_error) {
    err <- c(
      sprintf("Did not find orderly report '%s'", name),
      x = detail
    )
    near <- near_match(name, orderly_list_src(root_path))
    if (length(near) > 0) {
      hint <- sprintf("Did you mean %s",
                      paste(squote(near), collapse = ", "))
      err <- c(err, i = hint)
    }
    err <- c(err, i = sprintf("Looked relative to orderly root at '%s'",
                              root_path))
    cli::cli_abort(err, call = call)
  }

  # Just being called for the deprecation warning
  # Should be removed once we deprecate the name
  find_entrypoint_filename(file.path(root_path, "src", name))

  name
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
