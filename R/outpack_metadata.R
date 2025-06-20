##' Read metadata for a particular id. You may want to use
##' [orderly2::orderly_search] to find an id corresponding to a
##' particular query.
##'
##' @title Read outpack metadata
##'
##' @param id The id to fetch metadata for. An error will be thrown if
##'   this id is not known
##'
##' @param root The path to the root directory, or `NULL` (the
##'   default) to search for one from the current working
##'   directory. This function does not require that the directory is
##'   configured for orderly, and can be any `outpack` root (see
##'   [orderly2::orderly_init] for details).
##'
##' @return A list of metadata. See the outpack schema for details
##'   (https://github.com/mrc-ide/outpack)
##'
##' @export
orderly_metadata <- function(id, root = NULL) {
  validate_outpack_id(id, call = environment())
  root <- root_open(root, require_orderly = FALSE)
  path_metadata <- file.path(root$path, ".outpack", "metadata", id)
  if (!file.exists(path_metadata)) {
    cli::cli_abort("Packet '{id}' not found in outpack index")
  }
  outpack_metadata_load(file(path_metadata), root$config$orderly$plugins)
}


##' Low-level function for reading metadata and deserialising it. This
##' function can be used to directly read a metadata json file without
##' reference to a root which contains it. It may be useful in the
##' context of reading a metadata file written out as part of a failed
##' run.
##'
##' Custom metadata saved by plugins may not be deserialised as
##' expected when called with this function, as it is designed to
##' operate separately from a valid orderly root (i.e., it will load
##' data from any file regardless of where it came from). If `plugins`
##' is `TRUE` (the default) then we will deserialise all data that
##' matches any loaded plugin.  This means that the behaviour of this
##' function depends on if you have loaded the plugin packages. You
##' can force this by running `orderly2::orderly_config()` within any
##' orderly directory, which will load any declared plugins.
##'
##' @title Read outpack metadata json file
##'
##' @param path Path to the json file
##'
##' @param plugins Try and deserialise data from all loaded plugins
##'   (see Details).
##'
##' @return A list of outpack metadata; see the schema for details. In
##'   contrast to reading the json file directly with
##'   `jsonlite::fromJSON`, this function will take care to convert
##'   scalar and length-one vectors into the expected types.
##'
##' @export
orderly_metadata_read <- function(path, plugins = TRUE) {
  assert_file_exists(path, call = environment())
  outpack_metadata_load(file(path), if (plugins) .plugins else NULL)
}

outpack_metadata_create <- function(path, name, id, time, files,
                                    depends, parameters, git, custom,
                                    file_hash, file_ignore, hash_algorithm) {
  assert_scalar_character(name)
  assert_scalar_character(id)

  assert_is(time, "list")
  assert_is(time$start, "POSIXt")
  assert_is(time$end, "POSIXt")
  time$start <- scalar(time_to_num(time$start))
  time$end <- scalar(time_to_num(time$end))

  if (!is.null(parameters)) {
    validate_parameters(parameters, NULL)
    parameters <- lapply(parameters, scalar)
  }

  if (is.null(files)) {
    files <- dir(path, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  } else {
    assert_file_exists_relative(files, name = "File", workdir = path)
  }

  if (length(file_ignore) > 0) {
    files <- setdiff(files, file_ignore)
  } else {
    files <- unique(files)
  }

  ## In the most simple case we could just do nothing about inputs vs
  ## outputs; we don't even need a list of them (we just have files
  ## and that's all there is to it).  I am not 100% sure if that's
  ## sensible, but it will be easy enough to extend this later.  For
  ## orderly we can handle this via additional data in 'custom'.  Not
  ## having this distinction will make doing output-only packets
  ## easier of course.
  files <- withr::with_dir(
    path,
    data_frame(
      path = clean_path(files),
      size = file.size(files),
      hash = vcapply(files, hash_file, hash_algorithm, USE.NAMES = FALSE)))

  if (!is.null(file_hash)) {
    validate_hashes(set_names(files$hash, files$path), file_hash, call = NULL)
  }

  ## TODO: best to validate here that all elements of depends are
  ## really found in the inputs list; more generally we might verify
  ## that they really exist at all?

  ## We capture the idea of a single dependency "event" with
  ## potentially several files coming from it.
  if (is.null(depends)) {
    depends <- list()
  } else {
    for (i in seq_along(depends)) {
      depends[[i]]$packet <- scalar(depends[[i]]$packet)
      depends[[i]]$query <- scalar(depends[[i]]$query)
      depends[[i]]$files$here <- clean_path(depends[[i]]$files$here)
      depends[[i]]$files$there <- clean_path(depends[[i]]$files$there)
    }
    ## TODO: Additional checks could be required, but will require a
    ## root.  We do some of these on insert and via
    ## outpack_packet_use_dependency at the moment
    ##
    ## 1. is the id known to the system?
    ## 2. is names(depends[[i]]$files$here) present in 'path' (for all i)?
    ## 3. is unname(depends[[i]]$files$there) present in 'id' (for all i)?
    ## 4. is the file unchanged since import?
    ##
    ## 1, 3 and 4 require that we have the root active as they will
    ## require us to query the index, but we could do '2' here as it
    ## must be consistent within the metadata.
  }

  if (!is.null(custom)) {
    ## There's no obvious way of adding the schema information here
    ## because we probably hold either a machine-specific filename or
    ## the entire schema object, neither of which is useful to
    ## serialise.
    custom <- set_names(lapply(custom, function(x) set_class(x$data, "json")),
                        vcapply(custom, "[[", "application"))
  }

  if (!is.null(git)) {
    v <- c("sha", "branch")
    git[v] <- lapply(git[v], scalar)
    git$url <- git$url %||% character()
  }

  ret <- list(schema_version = scalar(outpack_schema_version()),
              name = scalar(name),
              id = scalar(id),
              time = time,
              parameters = parameters,
              files = files,
              depends = depends,
              git = git,
              custom = custom)

  to_json(ret, "outpack/metadata.json")
}


## This does a caching read via the index; under the hood it will load
## that that was read with the outpack_metadata_core_read
## function. Only the core fields will be deserialised and stored,
## which should save time and space.
outpack_metadata_core <- function(id, root, call = NULL) {
  root$index$metadata(id, call)
}


metadata_core_names <- c("id", "name", "parameters", "time", "files", "depends")
outpack_metadata_core_load <- function(json) {
  data <- parse_json(json)[metadata_core_names]
  outpack_metadata_core_deserialise(data)
}


outpack_metadata_core_deserialise <- function(data) {
  data$time <- lapply(data$time, num_to_time)
  data$files <- data_frame(path = vcapply(data$files, "[[", "path"),
                           size = vnapply(data$files, "[[", "size"),
                           hash = vcapply(data$files, "[[", "hash"))
  data$depends <- data_frame(
    packet = vcapply(data$depends, "[[", "packet"),
    query = vcapply(data$depends, "[[", "query"),
    files = I(lapply(data$depends, function(x) {
      data_frame(here = vcapply(x$files, "[[", "here"),
                 there = vcapply(x$files, "[[", "there"))
    })))
  if (!is.null(data$git)) {
    data$git$url <- list_to_character(data$git$url)
  }
  data
}


outpack_metadata_load <- function(json, plugins) {
  data <- parse_json(json)
  data <- outpack_metadata_core_deserialise(data)
  if (!is.null(data$custom$orderly)) {
    data$custom$orderly <- custom_metadata_deserialise(data$custom$orderly)
  }
  for (nm in intersect(names(data$custom), names(plugins))) {
    data$custom[[nm]] <- tryCatch(
      plugins[[nm]]$deserialise(data$custom[[nm]]),
      error = function(e) {
        cli::cli_warn(
          c("Deserialising custom metadata '{nm}' for '{data$id}' failed",
            x = e$message))
        data$custom[[nm]]
      })
  }
  data
}


validate_hashes <- function(found, expected, call = NULL) {
  err <- found[names(expected)] != expected
  msg <- is.na(err)
  if (any(msg)) {
    cli::cli_abort(
      c("File{?s} {?was/were} deleted after being added {cli::qty(sum(msg))}",
        set_names(names(expected)[msg], "x")),
      call = call)
  }
  if (any(err)) {
    cli::cli_abort(
      c("File{?s} {?was/were} changed after being added {cli::qty(sum(err))}",
        set_names(names(expected)[err], "x")),
      call = call)
  }
}


get_metadata_hash <- function(packet_id, root) {
  dat <- root$index$location(local)
  hash <- dat$hash[dat$packet == packet_id]
  stopifnot(length(hash) == 1)
  hash
}


## see run.R for the inverse of this
custom_metadata_deserialise <- function(data) {
  data$artefacts <- data_frame(
    description = vcapply(data$artefacts,
                          function(x) x$description %||% NA_character_),
    paths = I(lapply(data$artefacts, "[[", "paths")))
  data$role <- data_frame(
    path = vcapply(data$role, "[[", "path"),
    role = vcapply(data$role, "[[", "role"))
  data$shared <- data_frame(here = vcapply(data$shared, "[[", "here"),
                            there = vcapply(data$shared, "[[", "there"))
  data$session$packages <- data_frame(
    package = vcapply(data$session$packages, "[[", "package"),
    version = numeric_version(vcapply(data$session$packages, "[[", "version")),
    attached = vlapply(data$session$packages, "[[", "attached"))
  data
}
