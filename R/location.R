##' Add a new location - a place where other packets might be found
##' and pulled into your local archive.  Currently only file and http
##' based locations are supported, with limited support for custom
##' locations. Note that adding a location does *not* pull metadata
##' from it, you need to call
##' [orderly2::orderly_location_fetch_metadata] first.  The function
##' `orderly_location_add` can add any sort of location, but the other
##' functions documented here (`orderly_location_add_path`, etc) will
##' typically be much easier to use in practice.
##'
##' We currently support three types of locations - `path`, which points
##' to an outpack archive accessible by path (e.g., on the same
##' computer or on a mounted network share), `http`, which requires
##' that an outpack server is running at some url and uses an HTTP API
##' to communicate, and `packit`, which uses Packit as a web
##' server.  More types may be added later, and more configuration
##' options to these location types will definitely be needed in
##' future.
##'
##' Configuration options for different location types are described
##' in the arguments to their higher-level functions.
##'
##' **Path locations**:
##'
##' Use `orderly_location_add_path`, which accepts a `path` argument.
##'
##' **HTTP locations**:
##'
##' Accessing outpack over HTTP requires that an outpack server is
##'   running. The interface here is expected to change as we expand
##'   the API, but also as we move to support things like TLS and
##'   authentication.
##'
##' Use `orderly_location_add_http`, which accepts a `url` argument.
##'
##' **Packit locations**:
##'
##' Packit locations work over HTTPS, and include everything in an
##' outpack location but also provide authentication and later will
##' have more capabilities we think.
##'
##' Use `orderly_location_add_packit`, which accepts `url`, `token`
##' and `save_token` arguments.
##'
##' **Custom locations**:
##'
##' All outpack implementations are expected to support path and http
##' locations, with the standard arguments above.  But we expect that
##' some implementations will support custom locations, and that the
##' argument lists for these may vary between implementations. To
##' allow this, you can pass a location of type "custom" with a list
##' of arguments.  We expect an argument 'driver' to be present among
##' this list.  For an example of this in action, see the
##' [`outpack.sharepoint`](https://mrc-ide.github.io/outpack.sharepoint)
##' package.
##'
##' *Be warned that we may change this interface in future, in which
##' case you may need to update your configuration.*
##'
##' @section Warning:
##'
##' The API here may change as we move to support different types of
##'   locations.
##'
##' @title Add a new location
##'
##' @param name The short name of the location to use.  Cannot be in
##'   use, and cannot be one of `local` or `orphan`
##'
##' @param type The type of location to add. Currently supported
##'   values are `path` (a location that exists elsewhere on the
##'   filesystem) and `http` (a location accessed over outpack's http
##'   API).
##'
##' @param args Arguments to the location driver. The arguments here
##'   will vary depending on the type used, see Details.
##'
##' @param verify Logical, indicating if we should verify that the
##'   location can be used before adding.
##'
##' @inheritParams orderly_metadata
##'
##' @return Nothing
##' @export
orderly_location_add <- function(name, type, args, verify = TRUE, root = NULL) {
  root <- root_open(root, require_orderly = FALSE)
  assert_scalar_character(name)
  assert_scalar_logical(verify)

  if (name %in% location_reserved_name) {
    cli::cli_abort("Cannot add a location with reserved name '{name}'")
  }

  location_check_new_name(root, name, environment())
  match_value(type, setdiff(location_types, location_reserved_name))

  if (type == "path") {
    assert_scalar_character(args$path, name = "path")
    if (!fs::is_absolute_path(args$path)) {
      ## This only happens where the current working directory is not
      ## the same as the root.
      root_error <- !file.exists(file.path(root$path, args$path)) &&
        file.exists(args$path)
      if (root_error) {
        root_fix <- as.character(fs::path_rel(args$path, root$path))
        cli::cli_abort(
          c("'path' must be given relative to the orderly root",
            x = paste("You have provided the relative path '{args$path}',",
                      "which exists, but does not exist relatively to",
                      "'{root$path}', the root of your orderly archive"),
            i = "Consider passing '{root_fix}' instead"))
      }
    }
  } else if (type == "http") {
    assert_scalar_character(args$url, name = "url")
  } else if (type == "packit") {
    assert_scalar_character(args$url, name = "url")
    assert_scalar_character(args$token, name = "token", allow_null = TRUE)
    assert_scalar_logical(args$save_token, name = "save_token",
                          allow_null = TRUE)
    if (!is.null(args$token) && !is.null(args$save_token)) {
      cli::cli_abort("Cannot specify both 'token' and 'save_token'")
    }
    if (is.null(args$save_token)) {
      args$save_token <- is.null(args$token)
    }
  }
  loc <- new_location_entry(name, type, args, call = environment())

  if (verify) {
    cli_alert_info("Testing location")
    location_driver_create(type, args, root)$verify()
    cli_alert_success("Location configured successfully")
  }

  config <- root$config
  config$location <- rbind(config$location, loc)
  rownames(config$location) <- NULL
  config_update(config, root)
  cli_alert_success("Added location '{name}' ({type})")
  invisible()
}


##' @rdname orderly_location_add
##'
##' @param path The path to the other archive root. This can be a
##'   relative or absolute path, with different tradeoffs.  If you use
##'   an absolute path, then this location will typically work well on
##'   this machine, but it may behave poorly when the location is
##'   found on a shared drive **and** when you use your orderly root
##'   from more than one system.  This setup is common when using an
##'   HPC system.  If you use a relative path, then we will interpret
##'   it **relative to your orderly root** and not the directory that
##'   you evaluate this command from.  Typically your path should
##'   include leading dots (e.g. `../../somewhere/else`) as you should
##'   not nest orderly projects.  This approach should work fine on
##'   shared filesystems.
##'
##' @export
orderly_location_add_path <- function(name, path, verify = TRUE, root = NULL) {
  args <- list(path = path)
  orderly_location_add(name, "path", args, verify = verify, root = root)
}


##' @rdname orderly_location_add
##'
##' @param url The location of the server, including protocol, for
##'   example `http://example.com:8080`
##'
##' @export
orderly_location_add_http <- function(name, url, verify = TRUE, root = NULL) {
  args <- list(url = url)
  orderly_location_add(name, "http", args, verify = verify, root = root)
}


##' @rdname orderly_location_add
##'
##' @param token The value for your your login token (currently this
##'   is a GitHub token with `read:org` scope). If `NULL`, orderly2
##'   will perform an interactive authentication against GitHub to
##'   obtain one.
##'
##' @param save_token If no token is provided and interactive
##'   authentication is used, this controls whether the GitHub token
##'   should be saved to disk.  Defaults to `TRUE` if `NULL`.
##'
##' @export
orderly_location_add_packit <- function(name, url, token = NULL,
                                        save_token = NULL,
                                        verify = TRUE, root = NULL) {
  args <- list(url = url, token = token, save_token = save_token)
  orderly_location_add(name, "packit", args, verify = verify, root = root)
}


##' Rename an existing location
##'
##' @title Rename a location
##'
##' @param old The current short name of the location.
##' Cannot rename `local` or `orphan`
##'
##' @param new The desired short name of the location.
##' Cannot be one of `local` or `orphan`
##'
##' @inheritParams orderly_metadata
##'
##' @return Nothing
##' @export
orderly_location_rename <- function(old, new, root = NULL) {
  root <- root_open(root, require_orderly = FALSE)
  assert_scalar_character(new, call = call)

  if (old %in% location_reserved_name) {
    cli::cli_abort("Cannot rename default location '{old}'")
  }
  location_check_new_name(root, new, environment())
  location_check_exists(root, old, environment())

  config <- root$config
  config$location$name[config$location$name == old] <- new
  config_update(config, root)
  invisible()
}


##' Remove an existing location. Any packets from this location and
##' not known elsewhere will now be associated with the 'orphan'
##' location instead.
##'
##' @title Remove a location
##'
##' @param name The short name of the location.
##' Cannot remove `local` or `orphan`
##'
##' @inheritParams orderly_metadata
##'
##' @return Nothing
##' @export
orderly_location_remove <- function(name, root = NULL) {
  root <- root_open(root, require_orderly = FALSE)

  if (name %in% location_reserved_name) {
    cli::cli_abort("Cannot remove default location '{name}'")
  }
  location_check_exists(root, name, environment())

  index <- root$index$data()
  known_here <- index$location$packet[index$location$location == name]
  known_elsewhere <- index$location$packet[index$location$location != name]
  only_here <- setdiff(known_here, known_elsewhere)

  if (length(only_here) > 0) {
    cli::cli_alert_info("Orphaning {length(only_here)} packet{?s}")
    mark_packets_orphaned(name, only_here, root)
  }

  location_path <- file.path(root$path, ".outpack", "location", name)
  if (fs::dir_exists(location_path)) {
    fs::dir_delete(location_path)
  }
  root$index$rebuild()
  config <- root$config
  config$location <- config$location[config$location$name != name, ]
  config_update(config, root)
  invisible()
}


##' List known locations.  The special name `local` will always be
##' present within the output from this function (this is packets
##' known at the current root), though you will typically be
##' interested in *other* locations.
##'
##' @title List known pack locations
##'
##' @inheritParams orderly_metadata
##'
##' @param verbose Logical, indicating if we should return a
##'   data.frame that includes more information about the location.
##'
##' @return Depending on the value of `verbose`:
##'
##' * `verbose = FALSE`: A character vector of location names.  This is the
##'   default behaviour.
##' * `verbose = TRUE`: A data.frame with columns `name`, `type` and
##'   `args`.  The `args` column is a list column, with each element
##'   being the key-value pair arguments to the location.
##'
##' @seealso [orderly2::orderly_location_fetch_metadata], which can
##'   update your outpack index with metadata from any of the
##'   locations listed here.
##'
##' @export
orderly_location_list <- function(verbose = FALSE, root = NULL) {
  root <- root_open(root, require_orderly = FALSE)
  if (verbose) {
    root$config$location
  } else {
    root$config$location$name
  }
}


##' Fetch metadata from a location, updating the index.  This should
##' always be relatively quick as it updates only small files that
##' contain information about what can be found in remote packets.
##'
##' @title Fetch metadata from a location
##'
##' @param location The name of a location to pull from (see
##'   [orderly2::orderly_location_list] for possible values).  If not
##'   given, pulls from all locations.  The "local" and "orphan"
##'   locations are always up to date and pulling metadata from them
##'   does nothing.
##'
##' @inheritParams orderly_metadata
##'
##' @return Nothing
##'
##' @export
orderly_location_fetch_metadata <- function(location = NULL, root = NULL) {
  root <- root_open(root, require_orderly = FALSE)
  location_name <- location_resolve_valid(location, root,
                                          include_local = FALSE,
                                          include_orphan = FALSE,
                                          allow_no_locations = TRUE,
                                          environment())
  cli_alert_info(paste(
    "Fetching metadata from {length(location_name)} location{?s}:",
    "{squote({location_name})}"))
  for (name in location_name) {
    res <- location_fetch_metadata(name, root)
    if (res$total > 0) {
      cli_alert_success(paste(
        "Found {res$total} packet{?s} at '{name}', of which",
        "{res$new} {?is/are} new"))
    } else {
      cli_alert_warning("No metadata found at '{name}'")
    }
  }

  id_deorphan <- intersect(root$index$location(location_name)$packet,
                           root$index$location(orphan)$packet)
  if (length(id_deorphan) > 0) {
    cli::cli_alert_info("De-orphaning {length(id_deorphan)} packet{?s}")
    fs::file_delete(
      file.path(root$path, ".outpack", "location", orphan, id_deorphan))
    ## We could be lazier here, but this won't happen that often. The
    ## issue is that we need to tell the location data to notice
    ## deletion.
    root$index$rebuild()
  }
}


##' @rdname orderly2-deprecated
##' @keywords internal
##' @export
orderly_location_pull_metadata <- function(...) {
  deprecate_warn(
    "orderly_location_pull_metadata",
    "orderly_location_fetch_metadata")
  orderly_location_fetch_metadata(...)
}


##' Pull one or more packets (including all their files) into this
##' archive from one or more of your locations. This will make files
##' available for use as dependencies (e.g., with
##' [orderly2::orderly_dependency]).
##'
##' It is possible that it will take a long time to pull packets, if
##' you are moving a lot of data or if you are operating over a slow
##' connection.  Cancelling and resuming a pull should be fairly
##' efficient, as we keep track of files that are copied over even in
##' the case of an interrupted pull.
##'
##' @title Pull one or more packets from a location
##'
##' @param recursive If non-NULL, a logical, indicating if we should
##'   recursively pull all packets that are referenced by the packets
##'   specified in `id`.  This might copy a lot of data!  If `NULL`,
##'   we default to the value given by the the configuration option
##'   `require_complete_tree`.
##'
##' @inheritParams orderly_metadata
##' @inheritParams orderly_search
##' @inheritParams orderly_search_options
##'
##' @return Invisibly, the ids of packets that were pulled
##' @export
orderly_location_pull <- function(expr,
                                  name = NULL,
                                  location = NULL,
                                  fetch_metadata = FALSE,
                                  recursive = NULL,
                                  options = NULL,
                                  root = NULL) {
  root <- root_open(root, require_orderly = FALSE)
  compatibility_fix_options(options, "orderly_location_pull")

  if (expr_is_literal_id(expr, name)) {
    ids <- expr
  } else {
    ids <- orderly_search(expr,
                          name = name,
                          location = location,
                          allow_remote = TRUE,
                          fetch_metadata = fetch_metadata,
                          root = root)
  }

  if (length(ids) == 0 || (length(ids) == 1 && is.na(ids))) {
    if (!fetch_metadata) {
      hint <- c(i = paste("Did you forget to pull metadata? You can do this",
                          "by using the argument {.code fetch_metadata = TRUE}",
                          "in the call to 'orderly_location_pull()', or",
                          "by running 'orderly_location_fetch_metadata()'"))
    } else {
      hint <- NULL
    }
    cli::cli_abort(
      c("No packets found in query, so cannot pull anything",
        i = paste("Your query returned no packets, which is probably a mistake",
                  "so I'm erroring here."),
        hint))
  }

  cli_alert_info(
    "Pulling {length(ids)} packet{?s}: {squote(ids)}")

  plan <- location_build_pull_plan(ids, location, recursive, root,
                                   call = environment())

  if (plan$info$n_extra > 0) {
    cli_alert_info(paste(
      "Also pulling {plan$info$n_extra} packet{?s},",
      "dependencies of those requested"))
  }
  if (plan$info$n_skip > 0) {
    cli_alert_info(paste(
      "Skipping {plan$info$n_skip} of {plan$info$n_total} packet{?s}",
      "already unpacked"))
  }

  n_pull <- plan$info$n_total - plan$info$n_skip
  if (n_pull == 0) {
    cli_alert_success("Nothing to do, everything is available locally")
    return(plan$packet_id)
  }

  store <- location_pull_files(plan$files, root)

  use_archive <- !is.null(root$config$core$path_archive)
  if (use_archive) {
    n <- length(plan$packet_id)
    cli_progress_bar(
      format = paste(
        "{cli::pb_spin} Writing files for '{id}' (packet {i} / {n})",
        "| ETA: {cli::pb_eta} [{cli::pb_elapsed}]"),
      format_done = paste(
        "{cli::col_green(cli::symbol$tick)} Unpacked {n} packet{?s}",
        "in {cli::pb_elapsed}."),
      total = n,
      clear = FALSE)
  }
  for (i in seq_along(plan$packet_id)) {
    id <- plan$packet_id[[i]]
    if (use_archive) {
      cli_progress_update()
      location_pull_files_archive(id, store$value, root)
    }
    mark_packet_known(id, local, plan$hash[[id]], Sys.time(), root)
  }
  store$cleanup()

  cli_alert_success("Unpacked {n_pull} packet{?s}")

  invisible(plan$packet_id)
}


##' @rdname orderly2-deprecated
##' @keywords internal
##' @export
orderly_location_pull_packet <- function(...) {
  deprecate_warn(
    "orderly_location_pull_packet",
    "orderly_location_pull",
    c(i = paste("We like 'orderly_location_pull' as the inverse of",
                "'orderly_location_push'")))
  orderly_location_pull(...)
}


##' Push tree to location. This function works out what packets are
##' not known at the location and then what files are required to
##' create them. It then pushes all the files required to build all
##' packets and then pushes the missing metadata to the server. If the
##' process is interrupted it is safe to resume and will only transfer
##' files and packets that were missed on a previous call.
##'
##' @title Push tree to location
##'
##' @param expr An expression to search for.  Often this will be a
##'   vector of ids, but you can use a query here.
##'
##' @param location The name of a location to push to (see
##' [orderly2::orderly_location_list] for possible values).
##'
##' @param dry_run Logical, indicating if we should print a summary
##'   but not make any changes.
##'
##' @inheritParams orderly_metadata
##' @inheritParams orderly_search
##'
##' @return Invisibly, details on the information that was actually
##'   moved (which might be more or less than what was requested,
##'   depending on the dependencies of packets and what was already
##'   known on the other location).
##'
##' @export
orderly_location_push <- function(expr, location, name = NULL, dry_run = FALSE,
                                  root = NULL) {
  root <- root_open(root, require_orderly = FALSE)
  assert_scalar_logical(dry_run)
  location_name <- location_resolve_valid(location, root,
                                          include_local = FALSE,
                                          include_orphan = FALSE,
                                          allow_no_locations = FALSE,
                                          environment())
  if (expr_is_literal_id(expr, name)) {
    ids <- expr
    err <- setdiff(ids, root$index$unpacked())
    if (length(err)) {
      cli::cli_abort("Trying to push unknown packet{?s}: {squote(err)}")
    }
  } else {
    ids <- orderly_search(expr, name = name, root = root)
    if (length(ids) == 0) {
      cli_alert_warning("Query returned no packets to push")
    }
  }

  plan <- location_build_push_plan(ids, location_name, root)

  if (length(plan$files) == 0 && length(plan$packet_id) == 0) {
    cli_alert_success("Nothing to push, everything up to date")
  } else {
    cli_alert_info(
      paste("Pushing {length(plan$files)} file{?s} for",
            "{length(plan$packet_id)} packet{?s}"))
    if (dry_run) {
      cli_alert_info("Not making any changes, as 'dry_run = TRUE'")
    } else {
      driver <- location_driver(location_name, root)
      location_push_files(plan$files, driver, root)
      location_push_metadata(plan$packet_id, driver, root)
      orderly_location_fetch_metadata(location_name, root)
    }
  }

  invisible(plan)
}


location_push_files <- function(files, driver, root) {
  n_files <- length(files)
  if (n_files == 0) {
    cli_alert_info("No files needed, all are available at location")
  } else {
    size <- "(unknown)"
    cli_progress_bar(
      format = paste(
        "{cli::pb_spin} Pushing file {cli::pb_current} / {cli::pb_total}",
        "({size})"),
      format_done = paste(
        "{cli::col_green(cli::symbol$tick)} Uploaded {cli::pb_total} files",
        "in {cli::pb_elapsed}"),
      total = n_files,
      clear = FALSE)
    for (hash in files) {
      src <- find_file_by_hash(root, hash)
      if (is.null(src)) {
        cli::cli_abort(
          c("Did not find suitable file, can't push this packet",
            i = paste("The original file has been changed or deleted.",
                      "Details are above")))
      }
      size <- pretty_bytes(fs::file_size(src))
      cli_progress_update()
      driver$push_file(src, hash)
    }
  }
}


location_push_metadata <- function(ids, driver, root) {
  id <- "(unknown)"
  cli_progress_bar(
    format = paste(
      "{cli::pb_spin} Pushing packet {id} {cli::pb_current} / {cli::pb_total}"),
    format_done = paste(
      "{cli::col_green(cli::symbol$tick)} Uploaded {cli::pb_total} packets",
      "in {cli::pb_elapsed}"),
    total = length(ids),
    clear = FALSE)
  for (id in ids) {
    path <- file.path(root$path, ".outpack", "metadata", id)
    hash <- get_metadata_hash(id, root)
    cli_progress_update()
    driver$push_metadata(id, hash, path)
  }
}


location_driver <- function(location_name, root) {
  i <- match(location_name, root$config$location$name)
  type <- root$config$location$type[[i]]
  args <- root$config$location$args[[i]]
  location_driver_create(type, args, root)
}


location_driver_create <- function(type, args, root) {
  location <- switch(type,
                     path = orderly_location_path$new,
                     http = orderly_location_http$new,
                     packit = orderly_location_packit,
                     custom = orderly_location_custom)
  ## Set the workdir to the orderly root so that paths are interpreted
  ## relative to the root.
  withr::local_dir(root$path)
  do.call(location, args)
}


orderly_location_custom <- function(driver, ...) {
  driver <- check_symbol_from_str(driver, "args$driver")
  driver <- getExportedValue(driver$namespace, driver$symbol)
  if (inherits(driver, "R6ClassGenerator")) {
    driver <- driver$new
  }
  driver(...)
}


location_fetch_metadata <- function(location_name, root,
                                    call = parent.frame()) {
  index <- root$index$data()
  driver <- location_driver(location_name, root)

  hint_remove <- paste("Probably all you can do at this point is remove this",
                       "location from your configuration by running",
                       sprintf('orderly2::orderly_location_remove("%s")',
                               location_name))

  known_there <- driver$list()

  if (anyDuplicated(known_there$packet)) {
    dups <- unique(known_there$packet[duplicated(known_there$packet)])
    cli::cli_abort(
      c("Duplicate metadata reported from location '{location_name}'",
        x = "Duplicate data returned for packets {squote(dups)}",
        i = "This is a bug in your location server, please report it",
        i = hint_remove),
      call = call)
  }

  ## Things we've never heard of from any location:
  is_new <- !(known_there$packet %in% names(index$metadata))

  if (any(is_new)) {
    metadata <- driver$metadata(known_there$packet[is_new])
    id_new <- known_there$packet[is_new]
    expected_hash <- known_there$hash[is_new]
    path_metadata <- file.path(root$path, ".outpack", "metadata")
    fs::dir_create(path_metadata)
    filename <- file.path(path_metadata, id_new)
    for (i in seq_along(metadata)) {
      ## Ensure that the server is shipping data that matches what it
      ## says it is, if this is the first time we've seen this.
      ##
      ## This is not actually actionable, and it's not clear what can
      ## be done at present. The user should probably remove this
      ## location I think.
      hash_validate_data(
        metadata[[i]], expected_hash[[i]],
        sprintf("metadata for '%s' from '%s'", id_new[i], location_name),
        c(x = paste("This is bad news, I'm afraid. Your location is sending",
                    "data that does not match the hash it says it does.",
                    "Please let us know how this might have happened."),
          i = hint_remove),
        call)
      writeLines(metadata[[i]], filename[[i]], sep = "")
    }
  }

  seen_before <- intersect(known_there$packet, index$location$packet)
  hash_there <- known_there$hash[match(seen_before, known_there$packet)]
  hash_here <- index$location$hash[match(seen_before, index$location$packet)]
  err <- hash_there != hash_here
  if (any(err)) {
    cli::cli_abort(
      c("Location '{location_name}' has conflicting metadata",
        x = paste("This is {.strong really} bad news. We have been offered",
                  "metadata from '{location_name}' that has a different hash",
                  "to metadata that we have already imported from other",
                  "locations. I'm not going to import this new metadata, but",
                  "there's no guarantee that the older metadata is actually",
                  "what you want!"),
        i = "Conflicts for: {squote(seen_before[err])}",
        i = "We would be interested in this case, please let us know",
        i = hint_remove),
      call = call)
  }

  known_here <- index$location$packet[index$location$location == location_name]
  new_loc <- known_there[!(known_there$packet %in% known_here), ]
  for (i in seq_len(nrow(new_loc))) {
    mark_packet_known(new_loc$packet[[i]], location_name, new_loc$hash[[i]],
                      new_loc$time[[i]], root)
  }

  list(total = length(is_new), new = sum(is_new))
}


location_pull_hash_store <- function(files, location_name, driver, store) {
  ## Practically this is only ever called when files at least one row,
  ## so we ignore the corner case of trying to pull zero files.x
  total_size <- pretty_bytes(sum(files$size))
  withr::local_options(cli.progress_show_after = 0) # we need the end status
  cli::cli_progress_bar(
    format = paste(
      "{cli::pb_spin} Fetching file {i}/{nrow(files)}",
      "({pretty_bytes(files$size[i])}) from '{location_name}'",
      "| ETA: {cli::pb_eta} [{cli::pb_elapsed}]"),
    format_done = paste(
      "{cli::col_green(cli::symbol$tick)} Fetched {nrow(files)} file{?s}",
      "({total_size}) from '{location_name}' in {cli::pb_elapsed}."),
    total = sum(files$size),
    clear = FALSE)
  for (i in seq_len(nrow(files))) {
    cli::cli_progress_update(files$size[[i]])
    h <- files$hash[[i]]
    tmp <- driver$fetch_file(h, store$tmp())
    store$put(tmp, h, move = TRUE)
  }
}


location_pull_files_archive <- function(packet_id, store, root) {
  meta <- outpack_metadata_core(packet_id, root)
  dest <- file.path(root$path, root$config$core$path_archive, meta$name,
                    packet_id, meta$files$path)
  for (i in seq_len(nrow(meta$files))) {
    store$get(meta$files$hash[[i]], dest[[i]], overwrite = TRUE)
  }
}


location_resolve_valid <- function(location, root, include_local,
                                   include_orphan, allow_no_locations,
                                   call = NULL) {
  if (is.null(location)) {
    location <- orderly_location_list(root = root)
  } else if (is.character(location)) {
    valid <- orderly_location_list(root = root)
    err <- setdiff(location, orderly_location_list(root = root))
    if (length(err) > 0) {
      cli::cli_abort(c("Unknown location{?s}: {squote(err)}",
                       i = "Valid location{?s} are: {squote(valid)}"),
                     call = call)
    }
  } else {
    cli::cli_abort(
      "Invalid input for 'location'; expected NULL or a character vector")
  }

  ## In some cases we won't want local, make this easy to do:
  if (!include_local) {
    location <- setdiff(location, local)
  }
  if (!include_orphan) {
    location <- setdiff(location, orphan)
  }

  if (length(location) == 0 && !allow_no_locations) {
    cli::cli_abort("No suitable location found", call = call)
  }

  location
}


location_build_pull_plan <- function(packet_id, location, recursive, root,
                                     call = NULL) {
  packets <- location_build_pull_plan_packets(packet_id, recursive, root, call)
  info <- list(n_extra = length(packets$full) - length(packets$requested),
               n_skip = length(packets$skip),
               n_total = length(packets$full))

  location <- location_build_pull_plan_location(packets, location, root, call)
  files <- location_build_pull_plan_files(packets$fetch, location, root, call)

  ## Finally, get the hash of each of the pulled packets (this needs
  ## tidying up generally; see 'get_metadata_hash', which we can use
  ## as a base.
  tmp <- root$index$location(location)
  hash <- set_names(tmp$hash[match(packets$fetch, tmp$packet)], packets$fetch)

  list(packet_id = packets$fetch, files = files, hash = hash, info = info)
}


location_build_pull_plan_packets <- function(packet_id, recursive, root, call) {
  recursive <- recursive %||% root$config$core$require_complete_tree
  assert_scalar_logical(recursive, call = call)
  if (root$config$core$require_complete_tree && !recursive) {
    cli::cli_abort(
      c("'recursive' must be TRUE (or NULL) with your configuration",
        i = paste("Because 'core.require_complete_tree' is true, we can't",
                  "do a non-recursive pull, as this might leave an incomplete",
                  "tree")),
      call = call)
  }

  index <- root$index$data()
  if (recursive) {
    full <- find_all_dependencies(packet_id, index$metadata)
  } else {
    full <- packet_id
  }

  skip <- intersect(full, root$index$unpacked())
  fetch <- setdiff(full, skip)

  list(requested = packet_id, full = full, skip = skip, fetch = fetch)
}


location_build_pull_plan_location <- function(packets, location, root, call) {
  location_name <- location_resolve_valid(
    location, root, include_local = FALSE, include_orphan = FALSE,
    allow_no_locations = length(packets$fetch) == 0,
    call = call)
  ## Things that are found in suitable location:
  candidates <- root$index$location(location_name)
  missing <- setdiff(packets$fetch, candidates$packet)
  if (length(missing) > 0) {
    extra <- setdiff(missing, packets$requested)
    if (length(extra) > 0) {
      hint <- paste(
        "{length(extra)} missing packets were requested as dependencies of",
        "the ones you asked for: {squote(extra)}")
    } else {
      ## In the case where the above is used, we probably have
      ## up-to-date metadata so we don't display this.
      hint <- paste("Do you need to run",
                    "{.run orderly2::orderly_location_fetch_metadata()}?")
    }
    cli::cli_abort(c("Failed to find packet{?s} {squote(missing)}",
                     i = "Looked in location{?s} {squote(location_name)}",
                     i = hint),
                   call = call)
  }
  location_name
}


location_build_pull_plan_files <- function(packet_id, location, root, call) {
  meta <- root$index$data()$metadata[packet_id]
  packet_hash <- lapply(meta, function(x) x$files$hash)
  n_files <- vnapply(meta, function(x) nrow(x$files))
  if (sum(n_files) == 0) {
    files <- data_frame(hash = character(),
                        size = numeric(),
                        location = character())
  } else {
    if (length(location) == 1) {
      location_use <- location
    } else {
      ## Find the first location (within the provided set) to contain
      ## each packet:
      loc <- root$index$location(location)
      location_use <- vcapply(packet_id, function(id) {
        intersect(location, loc$location[loc$packet == id])[[1]]
      }, USE.NAMES = FALSE)
    }

    files <- Map(location_file_pull_meta, meta, location_use)
    files <- do.call(rbind.data.frame, files)
    ## Then we ensure we prefer to fetch from earlier-provided
    ## locations by ordering the list by locations and dropping
    ## duplicated hashes.
    if (length(location) > 1) {
      files <- files[order(match(files$location, location)), ]
    }
    files <- files[!duplicated(files$hash), ]
    rownames(files) <- NULL
  }
  files
}


location_file_pull_meta <- function(packet_meta, packet_location) {
  data_frame(
    hash = unlist(packet_meta$files$hash, FALSE, FALSE),
    size = unlist(packet_meta$files$size, FALSE, FALSE),
    location = packet_location)
}


location_build_push_plan <- function(packet_id, location_name, root) {
  driver <- location_driver(location_name, root)

  metadata <- root$index$data()$metadata
  packet_id <- sort(find_all_dependencies(packet_id, metadata))
  packet_id_msg <- driver$list_unknown_packets(packet_id)

  if (length(packet_id_msg) == 0) {
    files_msg <- character(0)
  } else {
    packet_id_msg <- sort(packet_id_msg)
    metadata <- metadata
    ## All files across all missing ids:
    files <- unique(unlist(
      lapply(packet_id_msg, function(i) metadata[[i]]$files$hash)))

    ## Which of these does the server not know about:
    files_msg <- driver$list_unknown_files(files)
  }

  list(packet_id = packet_id_msg, files = files_msg)
}


## This validation probably will need generalising in future as we add
## new types. The trick is going to be making sure that we can support
## different location types in different target languages effectively.
new_location_entry <- function(name, type, args, call = parent.frame()) {
  match_value(type, location_types)
  if (length(args) > 0) {
    assert_is(args, "list")
    assert_named(args)
  }
  if (type == "custom") {
    if (is.null(args$driver)) {
      cli::cli_abort("Field missing from args: 'driver'")
    }
    check_symbol_from_str(args$driver, "args$driver")
  }

  ## NOTE: make sure this matches the order in config_read
  data_frame(name = name,
             type = type,
             args = I(list(args)))
}


location_check_new_name <- function(root, name, call) {
  if (location_exists(root, name)) {
    cli::cli_abort("A location with name '{name}' already exists",
                   call = call)
  }
}


location_check_exists <- function(root, name, call) {
  if (!location_exists(root, name)) {
    valid <- orderly_location_list(root = root)
    cli::cli_abort(c("No location with name '{name}' exists",
                     i = "Possible location{?s} are: {squote(valid)}"),
                   call = call)
  }
}


location_exists <- function(root, name) {
  name %in% orderly_location_list(root = root)
}


mark_packets_orphaned <- function(location, packet_id, root) {
  if (!location_exists(root, "orphan")) {
    config <- root$config
    config$location <- rbind(
      config$location,
      new_location_entry(orphan, "orphan", NULL))
    rownames(config$location) <- NULL
    config_update(config, root)
  }
  src <- file.path(root$path, ".outpack", "location", location, packet_id)
  dest <- file.path(root$path, ".outpack", "location", "orphan", packet_id)
  fs::dir_create(dirname(dest))
  fs::file_move(src, dest)
}


drop_local_packet <- function(packet_id, root) {
  location <- root$index$location(NULL)
  known_at <- location$location[location$packet == packet_id]
  if (!any(known_at != local)) {
    mark_packets_orphaned(local, packet_id, root)
  }
}


## This approach may be suboptimal in the case where the user does not
## already have a file store, as it means that files will be copied
## around and hashed more than ideal:
##
## * hash the candidate file
## * rehash on entry into the file store
## * copy into the file store
## * copy from the file store into the final location
##
## So in the case where a hash is only present once in a chain of
## packets being pulled this will be one too many hashes and one too
## many copies.
##
## However, this approach makes the logic fairly easy to deal with,
## and copes well with data races and corruption of data on disk
## (e.g., users having edited files that we rely on, or editing them
## after we hash them the first time).
location_pull_files <- function(files, root) {
  if (root$config$core$use_file_store) {
    store <- root$files
    cleanup <- function() invisible()
    i <- store$exists(files$hash)
    if (any(i)) {
      cli_alert_success("Found {sum(i)} file{?s} in the file store")
      files <- files[!i, ]
    }
  } else {
    cli_alert_info("Looking for suitable files already on disk")
    store <- temporary_filestore(root)
    cleanup <- function() store$destroy()
    on_disk <- vlapply(files$hash, function(hash) {
      exists <- FALSE
      if (!is.null(path <- find_file_by_hash(root, hash))) {
        store$put(path, hash)
        exists <- TRUE
      }
      exists
    })
    if (any(on_disk)) {
      cli::cli_alert_success("Found {sum(on_disk)} file{?s} on disk")
      files <- files[!on_disk, , drop = FALSE]
    }
  }

  if (nrow(files) == 0) {
    cli_alert_success("All files available locally, no need to fetch any")
  } else {
    locations <- unique(files$location)
    cli_alert_info(paste(
      "Need to fetch {nrow(files)} file{?s} ({pretty_bytes(sum(files$size))})",
      "from {length(locations)} location{?s}"))
    for (loc in locations) {
      location_pull_hash_store(files[files$location == loc, ], loc,
                               location_driver(loc, root), store)
    }
  }
  list(value = store, cleanup = cleanup)
}


temporary_filestore <- function(root) {
  file_store$new(file.path(root$path, "orderly", "pull"))
}
