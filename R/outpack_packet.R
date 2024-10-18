##' Start a packet build (`outpack_packet_start`), end one
##' (`outpack_packet_cancel`, `outpack_packet_end`) and interact with
##' one (`outpack_packet_use_dependency`).
##'
##' @title Start, interact with, and end a packet build
##'
##' @param path Path to the build / output directory.
##'
##' @param name The name of the packet
##'
##' @param parameters Optionally, a named list of parameters.  The
##'   names must be unique, and the values must all be non-NA scalar
##'   atomics (logical, integer, numeric, character)
##'
##' @param id Optionally, an outpack id via [orderly2::outpack_id]. If
##'   not given a new id will be generated.
##'
##' @inheritParams outpack_metadata
##'
##' @return Invisibly, a copy of the packet data; this can be passed
##'   as the `packet` argument.
##'
##' @noRd
outpack_packet_start <- function(path, name, parameters = NULL, id = NULL,
                                 root = NULL) {
  root <- root_open(root, require_orderly = FALSE)

  assert_scalar_character(name)
  assert_is_directory(path)
  validate_parameters(parameters, NULL)

  if (is.null(id)) {
    id <- outpack_id()
  } else {
    validate_outpack_id(id)
  }

  time <- list(start = Sys.time())

  packet <- structure(
    list2env(
      list(
        name = name,
        id = id,
        path = path,
        parameters = parameters,
        files = list(),
        time = time,
        root = root),
      parent = emptyenv()),
    class = "outpack_packet")

  ## Human readable logging:
  cli::cli_alert_info(
    "Starting packet '{.pkg {name}}' {.code {id}} at {time$start}")
  if (length(parameters) > 0) {
    cli::cli_alert_info("Parameters:")
    cli::cli_li(sprintf("{.bold %s}: %s",
                        names(parameters), unname(parameters)))
  }

  invisible(packet)
}


outpack_packet_cancel <- function(packet) {
  packet <- check_current_packet(packet)
  cli::cli_alert_danger("Cancelling {packet$id}")
  outpack_packet_finish(packet)
}


##' @param insert Logical, indicating if we should insert the packet
##'   into the store. This is the default and generally what you
##'   want. The use-case we have for `insert = FALSE` is where you
##'   want to write out all metadata after a failure, and in this case
##'   you would not want to do a final insertion into the outpack
##'   archive. When `insert = FALSE`, we write out the json metadata
##'   that would have been written as `outpack.json` within the packet
##'   working directory.  Note that this skips a lot of validation
##'   (for example, validating that all files exist and that files
##'   marked immutable have not been changed)
##'
##' @noRd
outpack_packet_end <- function(packet, insert = TRUE) {
  packet <- check_current_packet(packet)
  packet$time$end <- Sys.time()
  hash_algorithm <- packet$root$config$core$hash_algorithm
  elapsed_str <- format(packet$time$end - packet$time$start)
  cli::cli_alert_info(
    "Finished {packet$id} at {packet$time$end} ({elapsed_str})")
  json <- outpack_metadata_create(packet$path, packet$name, packet$id,
                                  packet$time,
                                  files = NULL,
                                  depends = packet$depends,
                                  parameters = packet$parameters,
                                  custom = packet$custom,
                                  file_hash = packet$files$immutable,
                                  file_ignore = packet$files$ignored,
                                  hash_algorithm = hash_algorithm)
  if (insert) {
    outpack_insert_packet(packet$path, json, packet$root)
  } else {
    ## TODO: I am not sure what the best filename to use here is -
    ## good options feel like 'outpack.json' and `<id>` or '<id>.json'
    ## (totally collision resistant)
    writeLines(json, file.path(packet$path, "outpack.json"))
  }
  outpack_packet_finish(packet)
}


##' @param query An [orderly2::orderly_query] object, or something
##'   (e.g., a string) that can be trivially converted into one.
##'
##' @param files Files to copy from the other packet. This can be (1)
##'   a character vector, in which case files are copied over without
##'   changing their names, (2) a **named** character vector, in which
##'   case the name will be used as the destination name, or (3) a
##'   [data.frame] (including `tbl_df`, or `data.frame` objects)
##'   containing columns `from` and `to`, in which case the files
##'   `from` will be copied with names `to`.
##'
##' In all cases, if you want to import a directory of files from a
##'   packet, you must refer to the source with a trailing slash
##'   (e.g., `c(here = "there/")`), which will create the local
##'   directory `here/...` with files from the upstream packet
##'   directory `there/`. If you omit the slash then an error will be
##'   thrown suggesting that you add a slash if this is what you
##'   intended.
##'
##' You can use a limited form of string interpolation in the names of
##'   this argument; using `${variable}` will pick up values from
##'   `envir` and substitute them into your string.  This is similar
##'   to the interpolation you might be familiar with from
##'   `glue::glue` or similar, but much simpler with no concatenation
##'   or other fancy features supported.
##'
##' Note that there is an unfortunate, but (to us) avoidable
##'   inconsistency here; interpolation of values from your
##'   environment in the query is done by using `environment:x` and in
##'   the destination filename by doing `${x}`.
##'
##' @param search_options Optional search options for restricting the
##'   search (see [orderly2::orderly_search] for details)
##'
##' @param envir Optional environment for `environment:` lookups in
##'   `query`, and for interpolating filenames in `files`; the default
##'   is to use the parent frame, but other suitable options are the
##'   global environment or the environment of the script you are
##'   running (this only relevant if you have `environment:` lookups
##'   in `query`).
##'
##' @param overwrite Overwrite files at the destination; this is
##'   typically what you want, but set to `FALSE` if you would prefer
##'   that an error be thrown if the destination file already exists.
##'
##' @noRd
outpack_packet_use_dependency <- function(packet, query, files,
                                          envir = parent.frame(),
                                          search_options = NULL,
                                          overwrite = TRUE) {
  packet <- check_current_packet(packet)
  query <- as_orderly_query(query, arg = "query")
  search_options <- as_orderly_search_options(search_options)

  if (!query$info$single) {
    stop(paste(
      "The provided query is not guaranteed to return a single value:",
      squote(deparse_query(query$value$expr, NULL, NULL)),
      "Did you forget latest(...)?"))
  }

  id <- orderly_search(query,
                       parameters = packet$parameters,
                       envir = envir,
                       options = search_options,
                       root = packet$root)
  if (is.na(id)) {
    explanation <- orderly_query_explain(
      query, parameters = packet$parameters, envir = envir,
      location = search_options$location,
      allow_remote = search_options$allow_remote,
      root = packet$root)
    cli::cli_abort(
      c("Failed to find packet for query '{format(query)}'",
        i = "See 'rlang::last_error()$explanation' for details"),
      explanation = explanation)
  }

  needs_pull <- search_options$allow_remote &&
    packet$root$config$core$require_complete_tree &&
    !(id %in% packet$root$index$unpacked())
  if (needs_pull) {
    orderly_location_pull_packet(id, root = packet$root)
  }

  result <- orderly_copy_files(id, files = files, dest = packet$path,
                               location = search_options$location,
                               allow_remote = search_options$allow_remote,
                               pull_metadata = search_options$pull_metadata,
                               overwrite = overwrite,
                               envir = envir,
                               root = packet$root)

  query_str <- deparse_query(query$value$expr,
                             lapply(query$subquery, "[[", "expr"),
                             envir)

  ## Only update packet information after success, to reflect new
  ## metadata
  depends <- list(
    packet = id,
    query = query_str,
    files = result$files)
  packet$depends <- c(packet$depends, list(depends))

  outpack_packet_file_mark(packet, result$files$here, "immutable")

  invisible(result)
}


##' @section Custom metadata:
##'
##' The `outpack_packet_add_custom` function adds arbitrary
##'   additional metadata into a packet. It is primarily designed for
##'   use with applications that build on outpack to provide
##'   additional information beyond the minimal set provided by
##'   outpack.
##'
##' For example, orderly tracks "artefacts" which collect groups of
##'   file outputs into logical bundles.  To support this it needs to
##'   register additional data for each artefact with:
##'
##' * the description of the artefact (a short phrase)
##' * the format of the artefact (a string describing the data type)
##' * the contents of the artefact (an array of filenames)
##'
##' JSON for this might look like:
##'
##' ```json
##' {
##'   "artefacts": [
##'     {
##'       "description": "Data for onward use",
##'       "format": "data",
##'       "contents": ["results.rds", "summary.rds"]
##'     },
##'     {
##'       "description": "Diagnostic figures",
##'       "format": "staticgraph",
##'       "contents": ["fits.png", "inputs.png"]
##'     }
##'   ]
##' }
##' ```
##'
##' Here, we describe two artefacts, together collecting four files.
##'
##' We need to store these in outpack's final metadata, and we want to
##'   do this in a way that allows easy querying later on while
##'   scoping the data to your application.  To allow for this we
##'   group all data your application adds under an application key
##'   (e.g., `orderly`).  You can then store whatever data you want
##'   beneath this key.
##'
##' **NOTE1**: A limitation here is that the filenames above cannot be
##'   checked against the outpack list of files because outpack does
##'   not know that `contents` here refers to filenames.
##'
##' **NOTE2**: To allow for predictable serialisation to JSON, you
##'   must serialise your own data before passing through to
##'   `outpack_packet_add_custom`.
##'
##' @noRd
##'
##' @param application The name of the application (used to organise
##'   the data and query it later, see Details)
##'
##' @param data Additional metadata to add to the packet. This must be
##'   a string representing already-serialised json data.
outpack_packet_add_custom <- function(packet, application, data) {
  p <- check_current_packet(packet)

  assert_scalar_character(application)
  assert_scalar_character(data)

  parse_json(data, name = "custom metadata")

  if (application %in% vcapply(packet$custom, "[[", "application")) {
    stop(sprintf("metadata for '%s' has already been added for this packet",
                 application))
  }

  custom <- list(application = application, data = data)
  packet$custom <- c(packet$custom, list(custom))
  invisible()
}


##' Mark file within an in-progress packet. This will store the hash
##' of the file within the internal outpack structures and force an
##' error if the file is changed or deleted later.  The function
##' [orderly2::outpack_packet_file_list()] will report on which files
##' are marked (or unmarked) within the directory.
##'
##' @title Mark files during packet run
##'
##' @param packet The current packet
##'
##' @param files A character vector of relative paths
##'
##' @param status A status to mark the file with. Must be "immutable"
##'   or "ignored"
##'
##' @return Depending on function
##'
##' * `outpack_packet_file_mark` returns nothing
##' * `outpack_packet_file_list` returns a [data.frame] with columns
##'   `path` and `status` (`immutable`, `ignored` or `unknown`)
##'
##' @noRd
outpack_packet_file_mark <- function(packet, files, status) {
  status <- match_value(status, c("immutable", "ignored"))
  packet <- check_current_packet(packet)

  assert_file_exists_relative(files, workdir = packet$path, name = "File",
                              call = environment())

  ## TODO: these are exclusive categories because we later return a
  ## 1:1 mapping of file to status
  if (status == "immutable") {
    hash_algorithm <- packet$root$config$core$hash_algorithm
    value <- withr::with_dir(packet$path,
                             hash_files(files, hash_algorithm, named = TRUE))

    if (any(files %in% packet$files$ignored)) {
      err <- intersect(files, packet$files$ignored)
      cli::cli_abort(c("Cannot mark ignored files as immutable",
                       set_names(err, "x")))
    }

    prev <- names(packet$files$immutable)
    validate_hashes(value, packet$files$immutable[intersect(files, prev)],
                    call = environment())
    value <- value[setdiff(names(value), prev)]
    packet$files$immutable <- c(packet$files$immutable, value)
  } else if (status == "ignored") {
    if (any(files %in% names(packet$files$immutable))) {
      err <- intersect(files, names(packet$files$immutable))
      cli::cli_abort(c("Cannot mark immutable files as ignored",
                       set_names(err, "x")))
    }

    packet$files$ignored <- union(packet$files$ignored, files)
  }
  invisible()
}


outpack_packet_file_list <- function(packet) {
  packet <- check_current_packet(packet)
  files <- withr::with_dir(packet$path,
                           dir(all.files = TRUE, recursive = TRUE, no.. = TRUE))
  status <- rep("unknown", length(files))
  status[files %in% names(packet$files$immutable)] <- "immutable"
  status[files %in% packet$files$ignored] <- "ignored"
  data_frame(path = files, status = status)
}



outpack_packet_finish <- function(packet) {
  packet$complete <- TRUE
}


## This is used in each of the functions that accept either 'packet'
## as an argument and which will fall back onto the global active
## packet.
check_current_packet <- function(packet) { # TODO: rename
  assert_is(packet, "outpack_packet")
  if (isTRUE(packet$complete)) {
    stop(sprintf("Packet '%s' is complete", packet$id))
  }
  packet
}
