##' Run a report.  This will create a new directory in
##' `drafts/<reportname>`, copy your declared resources there, run
##' your script and check that all expected artefacts were created.
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
##' @param logging_console Optional logical, passed through to
##'   [outpack::outpack_packet_start] to control printing logs to the
##'   console, overriding any default configuration set at the root.
##'
##' @param logging_threshold Optional logging threshold, passed through to
##'   [outpack::outpack_packet_start] to control the amount of detail
##'   in logs printed during running, overriding any default
##'   configuration set at the root. If given, must be one of `info`,
##'   `debug` or `trace` (in increasing order of
##'   verbosity). Practically this has no effect at present as we've
##'   not added any fine-grained logging.
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
                        root = NULL, locate = TRUE) {
  root <- orderly_root(root, locate)
  src <- file.path(root$path, "src", name)

  envir <- envir %||% .GlobalEnv
  assert_is(envir, "environment")

  dat <- orderly_read(src)

  parameters <- check_parameters(parameters, dat$parameters)

  orderly_validate(dat, src)

  id <- outpack::outpack_id()

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

  p <- outpack::outpack_packet_start(path, name, parameters = parameters,
                                     id = id, logging_console = logging_console,
                                     logging_threshold = logging_threshold,
                                     root = root$outpack)
  withCallingHandlers({
    outpack::outpack_packet_file_mark(p, "orderly.R", "immutable")
    p$orderly3 <- list(config = root$config, envir = envir, src = src,
                       strict = dat$strict, inputs_info = inputs_info)
    current[[path]] <- p
    on.exit(current[[path]] <- NULL, add = TRUE, after = TRUE)

    if (!is.null(parameters)) {
      list2env(parameters, envir)
    }

    result <- outpack::outpack_packet_run(p, "orderly.R", envir)
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

  ## We can actually get this from reading the file, I think
  packages <- character(0)

  list(artefacts = artefacts,
       role = role,
       description = description,
       global = global,
       packages = packages,
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
}


## Same logic as orderly1; has worked well in practice. We might want
## to relax additional parameters here later, but that requires some
## thinking about what to do with them (do they get passed into the
## environment etc or not? do they get validated?)
check_parameters <- function(given, spec) {
  if (length(given) > 0) {
    assert_named(given, unique = TRUE)
  }

  is_required <- vlapply(spec, is.null)

  msg <- setdiff(names(spec)[is_required], names(given))
  if (length(msg) > 0L) {
    stop("Missing parameters: ", paste(squote(msg), collapse = ", "))
  }
  extra <- setdiff(names(given), names(spec))
  if (length(extra) > 0L) {
    stop("Extra parameters: ", paste(squote(extra), collapse = ", "))
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

  err <- !vlapply(given, function(x) {
    is.character(x) || is.numeric(x) || is.logical(x)
  })
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
  found <- lapply(names(spec), function(v) env[[v]])
  check_parameter_values(found[!vlapply(found, is.null)], FALSE)
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
      "Consider using orderly3::orderly_artefact() to describe them",
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
      "Consider using orderly3::orderly_artefact() to describe them",
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

  plugin_run_cleanup(path, p$orderly3$config$plugins)
  check_produced_artefacts(path, p$orderly3$artefacts)
  if (p$orderly3$strict$enabled) {
    check_files_strict(path, p$files, p$orderly3$artefacts)
  } else {
    check_files_relaxed(path, p$orderly3$inputs_info)
  }
  custom_metadata_json <- to_json(custom_metadata(p$orderly3))
  schema <- custom_metadata_schema(p$orderly3$config)
  outpack::outpack_packet_add_custom(p, "orderly", custom_metadata_json, schema)

  outpack::outpack_packet_end(p)
  unlink(path, recursive = TRUE)
}


orderly_packet_cleanup_failure <- function(p) {
  ignore_errors(plugin_run_cleanup(path, p$orderly3$config$plugins))
  custom_metadata_json <- to_json(custom_metadata(p$orderly3))
  outpack::outpack_packet_add_custom(p, "orderly", custom_metadata_json)
  outpack::outpack_packet_end(p, insert = FALSE)
}
