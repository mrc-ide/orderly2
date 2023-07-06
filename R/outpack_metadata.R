##' Low-level function for reading metadata and deserialising it. This
##' function can be used to directly read a metadata json file without
##' reference to a root which contains it. It may be useful in the
##' context of reading a metadata file written out as part of a failed
##' run (see the `insert = FALSE` argument to
##' [outpack::outpack_packet_end()])
##'
##' @title Read outpack metadata json file
##'
##' @param path Path to the json file
##'
##' @return A list of outpack metadata; see the schema for details. In
##'   contrast to reading the json file directly with
##'   `jsonlite::fromJSON`, this function will take care to convert
##'   scalar and length-one vectors into the expected types.
##'
##' @export
outpack_metadata_read <- function(path) {
  assert_file_exists(path)
  outpack_metadata_load(path)
}

outpack_metadata_create <- function(path, name, id, time, files,
                                    depends, parameters,
                                    script, custom, session,
                                    file_hash, file_ignore,
                                    hash_algorithm) {
  assert_scalar_character(name)
  assert_scalar_character(id)

  assert_is(time, "list")
  assert_is(time$start, "POSIXt")
  assert_is(time$end, "POSIXt")
  time$start <- scalar(time_to_num(time$start))
  time$end <- scalar(time_to_num(time$end))

  if (!is.null(parameters)) {
    validate_parameters(parameters)
    parameters <- lapply(parameters, scalar)
  }

  if (is.null(files)) {
    files <- dir(path, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  } else {
    assert_relative_path(files, no_dots = TRUE)
    assert_file_exists(files, path)
  }

  if (length(file_ignore) > 0) {
    files <- setdiff(files, file_ignore)
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
      path = files,
      size = file.size(files),
      hash = vcapply(files, hash_file, hash_algorithm, USE.NAMES = FALSE)))

  if (!is.null(file_hash)) {
    validate_hashes(set_names(files$hash, files$path), file_hash)
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

  if (is.null(script)) {
    script <- character()
  } else {
    assert_character(script)
  }

  if (is.null(session)) {
    session <- outpack_session_info(utils::sessionInfo())
  }

  if (!is.null(custom)) {
    ## There's no obvious way of adding the schema information here
    ## because we probably hold either a machine-specific filename or
    ## the entire schema object, neither of which is useful to
    ## serialise.
    custom <- set_names(lapply(custom, function(x) set_class(x$data, "json")),
                        vcapply(custom, "[[", "application"))
  }

  git <- git_info(path)
  if (!is.null(git)) {
    git$sha <- scalar(git$sha)
    git$branch <- scalar(git$branch)
  }

  ret <- list(schema_version = scalar(outpack_schema_version()),
              name = scalar(name),
              id = scalar(id),
              time = time,
              parameters = parameters,
              files = files,
              depends = depends,
              script = script,
              session = session,
              git = git,
              custom = custom)

  to_json(ret, "metadata")
}


outpack_metadata_load <- function(json) {
  if (!inherits(json, "json")) { # could use starts with "{"
    json <- read_string(json)
  }

  data <- jsonlite::parse_json(json)
  data$hash <- hash_data(json, "sha256")
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
    data$git$url <- vcapply(data$git$url, identity)
  }

  data
}


outpack_session_info <- function(info) {
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


outpack_metadata_index_read <- function(path) {
  keep <- c("name", "id", "parameters", "files", "depends")
  outpack_metadata_load(path)[keep]
}


validate_hashes <- function(found, expected) {
  err <- found[names(expected)] != expected
  msg <- is.na(err)
  if (any(msg)) {
    stop(sprintf("File was deleted after being added: %s",
                 paste(squote(names(expected)[msg]), collapse = ", ")))
  }
  if (any(err)) {
    stop(sprintf("File was changed after being added: %s",
                 paste(squote(names(expected)[err]), collapse = ", ")))
  }
}


get_metadata_hash <- function(packet_id, root) {
  index <- root$index()$location
  location_id <- local_location_id(root)
  i <- index$packet == packet_id & index$location == location_id
  hash <- index$hash[i]
  stopifnot(length(hash) == 1)
  hash
}
