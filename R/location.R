##' Add a new location - a place where other packets might be found
##' and pulled into your local archive.  Currently only file-based
##' locations are supported.
##'
##' We currently support two types of locations - `path`, which points
##' to an outpack archive accessible by path (e.g., on the same
##' computer or on a mounted network share) and `http`, which requires
##' that an outpack server is running at some url and uses an HTTP API
##' to communicate. More types may be added later, and more
##' configuration options to these location types will definitely be
##' needed in future.
##'
##' Configuration options for different location types:
##'
##' **Path locations**:
##'
##' * `path`: The path to the other archive root. This should
##'   generally be an absolute path, or the behaviour of outpack will
##'   be unreliable.
##'
##' **HTTP locations**:
##'
##' Accessing outpack over HTTP requires that an outpack server is
##'   running. The interface here is expected to change as we expand
##'   the API, but also as we move to support things like TLS and
##'   authentication.
##'
##' * `url`: The location of the server, including protocol, for
##'   example `http://example.com:8080`
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
##' @inheritParams orderly_metadata
##'
##' @return Nothing
##' @export
orderly_location_add <- function(name, type, args, root = NULL, locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())
  assert_scalar_character(name)

  if (name %in% location_reserved_name) {
    stop(sprintf("Cannot add a location with reserved name '%s'",
                 name))
  }

  location_check_new_name(root, name)
  match_value(type, setdiff(location_types, location_reserved_name))

  loc <- new_location_entry(name, type, args)
  if (type == "path") {
    ## We won't be necessarily be able to do this _generally_ but
    ## here, let's confirm that we can read from the outpack archive
    ## at the requested path; this will just fail but without
    ## providing the user with anything actionable yet.
    assert_scalar_character(loc$args[[1]]$path, name = "args$path")
    root_open(loc$args[[1]]$path, locate = FALSE, require_orderly = FALSE)
  } else if (type == "http") {
    assert_scalar_character(loc$args[[1]]$url, name = "args$url")
  }

  config <- root$config
  config$location <- rbind(config$location, loc)
  rownames(config$location) <- NULL
  root$update_config(config)
  invisible()
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
orderly_location_rename <- function(old, new, root = NULL, locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())
  assert_scalar_character(new)

  if (old %in% location_reserved_name) {
    stop(sprintf("Cannot rename default location '%s'",
                 old))
  }
  location_check_new_name(root, new)
  location_check_exists(root, old)

  config <- root$config
  id <- lookup_location_id(old, root)
  config$location$name[config$location$id == id] <- new
  root$update_config(config)
  invisible()
}


##' Remove an existing location. Any packets from this location
##' will now be associated with the 'orphan' location instead.
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
orderly_location_remove <- function(name, root = NULL, locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())

  if (name %in% location_reserved_name) {
    stop(sprintf("Cannot remove default location '%s'",
                 name))
  }
  location_check_exists(root, name)

  index <- root$index()
  config <- root$config
  id <- lookup_location_id(name, root)
  known_here <- index$location$packet[index$location$location == id]
  known_elsewhere <- index$location$packet[index$location$location != id]
  only_here <- setdiff(known_here, known_elsewhere)

  if (length(only_here) > 0) {
    if (!location_exists(root, "orphan")) {
      config$location <- rbind(
        config$location,
        new_location_entry(orphan, "orphan", NULL))
      rownames(config$location) <- NULL
    }

    orphan_id <- config$location$id[match("orphan", config$location$name)]
    mark_packets_orphaned(id, only_here, orphan_id, root)
  }

  location_path <- file.path(root$path, ".outpack", "location", id)
  if (fs::dir_exists(location_path)) {
    fs::dir_delete(location_path)
  }
  root$index(skip_cache = TRUE)
  config$location <- config$location[config$location$name != name, ]
  root$update_config(config)
  invisible()
}


##' List known locations.
##'
##' @title List known pack locations
##'
##' @inheritParams orderly_metadata
##'
##' @return A character vector of location names. The special name
##'   `local` will always be present.
##'
##' @seealso [orderly2::orderly_location_pull_metadata], which can
##'   update your outpack index with metadata from any of the
##'   locations listed here.
##'
##' @export
orderly_location_list <- function(root = NULL, locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())
  root$config$location$name
}


##' Pull metadata from a location, updating the index.  This should
##' always be relatively quick as it updates only small files that
##' contain information about what can be found in remote packets.
##'
##' @title Pull metadata from a location
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
orderly_location_pull_metadata <- function(location = NULL, root = NULL,
                                           locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())
  location_id <- location_resolve_valid(location, root,
                                        include_local = FALSE,
                                        allow_no_locations = TRUE)
  for (id in location_id) {
    location_pull_metadata(id, root)
  }
}


##' Pull one or more packets (including all their files) into this
##' archive from one or more of your locations. This will make files
##' available for use as dependencies (e.g., with
##' [orderly2::orderly_dependency]).
##'
##' The behaviour of this function will vary depending on whether or
##' not the destination outpack repository (i.e., `root`) uses a file
##' store or not.  If it does, then we simply import the unknown files
##' into the store, and this will always be fairly efficient.  If no
##' file store is used then for the time being we pull all files from
##' the upstream location, even if this means copying a file we
##' already know about elsewhere in the outpack archive.  We will
##' improve this in a future version.
##'
##' @title Pull a single packet from a location
##'
##' @param ... Arguments passed through to
##'   [orderly2::orderly_search]. In the special case where the first
##'   argument is a character vector of ids *and* there are no named
##'   dot arguments, then we interpret this argument as a vector of
##'   ids directly. Be careful here, your query may pull a lot of data
##'   - in particular, passing `NULL` will match everything that every
##'   remote has!
##'
##' @param search_options Options passed to
##'   [orderly2::orderly_search]. Only the `location` option here is
##'   preseved, as `allow_remote` must be `TRUE`
##'
##' @param recursive If non-NULL, a logical, indicating if we should
##'   recursively pull all packets that are referenced by the packets
##'   specified in `id`.  This might copy a lot of data!  If `NULL`,
##'   we default to the value given by the the configuration option
##'   `require_complete_tree`.
##'
##' @inheritParams orderly_metadata
##'
##' @return Invisibly, the ids of packets that were pulled
##' @export
orderly_location_pull_packet <- function(..., recursive = NULL,
                                         root = NULL, locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())
  if (dots_is_literal_id(...)) {
    ids <- ..1
    options <- orderly_search_options(allow_remote = TRUE)
  } else {
    options <- list(...)$options
    if (is.null(options)) {
      options <- orderly_search_options(allow_remote = TRUE)
    } else if (!options$allow_remote) {
      ## we can't easily modify this arg in place, though with rlang
      ## this might be possible - see examples in
      ## https://rlang.r-lib.org/reference/list2.html, and in
      ## particular the '!!!' splicing operator.
      cli::cli_abort(
        "If specifying 'options' in '...', 'allow_remote' must be TRUE",
        i = "If FALSE, then we can't find a packet you don't already have :)")
    }
    ids <- orderly_search(..., options = options, root = root)
  }

  index <- root$index()

  recursive <- recursive %||% root$config$core$require_complete_tree
  assert_scalar_logical(recursive)
  if (root$config$core$require_complete_tree && !recursive) {
    stop("'recursive' must be TRUE (or NULL) with your configuration")
  }

  if (recursive) {
    ids <- find_all_dependencies(ids, index$metadata)
  }

  ## Later, it might be better if we did not skip over unpacked
  ## packets, but instead validate and/or repair them (see mrc-3052)
  ids <- setdiff(ids, index$unpacked)
  if (length(ids) == 0) {
    return(invisible(ids))
  }

  ## I think that this whole section here could be simplified a lot,
  ## because we just need to get the full list of files over the
  ## packets; i.e., the reverse situation of the push logic; do that
  ## in a separate PR.
  ##
  ## We could come up with a few heuristics about where to get files
  ## from - see plan_copy_files too for another shot at this that can
  ## then be tidied up.
  location_id <- location_resolve_valid(options$locations, root,
                                        include_local = FALSE,
                                        allow_no_locations = FALSE)
  plan <- location_build_pull_plan(ids, location_id, root)
  local_id <- local_location_id(root)

  ## At this point we should really be providing logging about how
  ## many packets, files, etc are being copied.  I've done this as a
  ## single loop, but there's also no real reason why we might not
  ## present this as a single update operation for pulling all files
  ## across all packets (within a single location where more than one
  ## is required).  This is the simplest implementation for now
  ## though.
  ##
  ## Even though we look across all locations for places we can find a
  ## packet, we don't look across all locations for a given file (that
  ## is, if a location fails to provide the expected file, we will
  ## error and not try and recover).  That's probably reasonable
  ## behaviour as this should be pretty rare if people have sensible
  ## workflows, but there's also an argument that we might try looking
  ## for a given file in any location at some point.
  for (i in seq_len(nrow(plan))) {
    ## See mrc-4351 (assumption is this was validated on insert).
    hash <- index$location$hash[index$location$packet == plan$packet[i] &
                                index$location$location == plan$location_id[i]]
    driver <- location_driver(plan$location_id[i], root)
    if (root$config$core$use_file_store) {
      location_pull_files_store(root, driver, plan$packet[i])
    }
    if (!is.null(root$config$core$path_archive)) {
      location_pull_files_archive(root, driver, plan$packet[i])
    }
    mark_packet_known(plan$packet[i], local_id, hash, Sys.time(), root)
  }

  invisible(ids)
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
##' @param packet_id One or more packets to push to the server
##'
##' @param location The name of a location to push to (see
##' [orderly2::orderly_location_list] for possible values).
##'
##' @inheritParams orderly_metadata
##'
##' @return Invisibly, details on the information that was actually
##'   moved (which might be more or less than what was requested,
##'   depending on the dependencies of packets and what was already
##'   known on the other location).
##'
##' @export
orderly_location_push <- function(packet_id, location, root = NULL,
                                  locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = TRUE,
                    call = environment())
  location_id <- location_resolve_valid(location, root,
                                        include_local = FALSE,
                                        allow_no_locations = FALSE)
  plan <- location_build_push_plan(packet_id, location_id, root)

  if (length(plan$files) > 0 || length(plan$packet_id) > 0) {
    driver <- location_driver(location_id, root)
    for (hash in plan$files) {
      driver$push_file(find_file_by_hash(root, hash), hash)
    }
    for (id in plan$packet_id) {
      driver$push_metadata(id, root)
    }
  }

  invisible(plan)
}


location_driver <- function(location_id, root) {
  i <- match(location_id, root$config$location$id)
  type <- root$config$location$type[[i]]
  args <- root$config$location$args[[i]]
  switch(type,
         path = orderly_location_path$new(args$path),
         http = orderly_location_http$new(args$url),
         custom = orderly_location_custom(args))
}


orderly_location_custom <- function(args) {
  driver <- check_symbol_from_str(args$driver, "args$driver")
  driver <- getExportedValue(driver$namespace, driver$symbol)
  if (inherits(driver, "R6ClassGenerator")) {
    driver <- driver$new
  }
  do.call(driver, args[names(args) != "driver"])
}


location_pull_metadata <- function(location_id, root) {
  index <- root$index()
  driver <- location_driver(location_id, root)

  known_there <- driver$list()

  ## Things we've never heard of from any location:
  new_id_metadata <- setdiff(known_there$packet, names(index$metadata))
  if (length(new_id_metadata) > 0) {
    metadata <- driver$metadata(new_id_metadata)
    path_metadata <- file.path(root$path, ".outpack", "metadata")
    fs::dir_create(path_metadata)
    filename <- file.path(path_metadata, new_id_metadata)
    for (i in seq_along(metadata)) {
      writeLines(metadata[[i]], filename[[i]])
    }
  }

  known_here <- index$location$packet[index$location$location == location_id]
  new_loc <- known_there[!(known_there$packet %in% known_here), ]

  for (i in seq_len(nrow(new_loc))) {
    mark_packet_known(new_loc$packet[[i]], location_id, new_loc$hash[[i]],
                      new_loc$time[[i]], root)
  }

  root$index()
}


location_pull_hash_store <- function(root, driver, hash) {
  hash_missing <- unique(hash[!root$files$exists(hash)])
  for (h in hash_missing) {
    tmp <- root$files$tmp()
    root$files$put(driver$fetch_file(h, tmp), h, move = TRUE)
  }
}


location_pull_files_store <- function(root, driver, packet_id) {
  hash <- root$metadata(packet_id)$files$hash
  location_pull_hash_store(root, driver, hash)
}


location_pull_hash_archive <- function(root, driver, hash, dest) {
  ## TODO: some special care needed here if we want to avoid
  ## downloading the same file twice from _this_ packet as we won't be
  ## able to use find_file_by_hash function to resolve that; instead
  ## this should loop over unique hashes ideally. Easy enough but
  ## complicates the code.
  fs::dir_create(dirname(dest))
  for (i in seq_along(hash)) {
    src <- find_file_by_hash(root, hash[[i]])
    if (is.null(src)) {
      driver$fetch_file(hash[[i]], dest[[i]])
    } else {
      fs::file_copy(src, dest[[i]], overwrite = TRUE)
    }
  }
}

location_pull_files_archive <- function(root, driver, packet_id) {
  meta <- root$metadata(packet_id)
  dest <- file.path(root$path, root$config$core$path_archive, meta$name,
                    packet_id, meta$files$path)
  if (root$config$core$use_file_store) {
    for (i in seq_len(nrow(meta$files))) {
      root$files$get(meta$files$hash[[i]], dest[[i]], overwrite = TRUE)
    }
  } else {
    location_pull_hash_archive(root, driver, meta$files$hash, dest)
  }
}


location_resolve_valid <- function(location, root, include_local,
                                   allow_no_locations) {
  if (is.null(location)) {
    location <- orderly_location_list(root)
  } else if (is.character(location)) {
    err <- setdiff(location, orderly_location_list(root))
    if (length(err) > 0) {
      stop(sprintf("Unknown location: %s", paste(squote(err), collapse = ", ")))
    }
  } else {
    stop("Invalid input for 'location'; expected NULL or a character vector")
  }

  ## In some cases we won't want local, make this easy to do:
  if (!include_local) {
    location <- setdiff(location, local)
  }

  ## We could throw nicer errors here if we included this check (and
  ## the setdiff) in every one of the above the three branches above,
  ## but that makes things pretty hard to follow. We'll do some work
  ## on nicer errors later once we get this ready for people to
  ## actually use.
  if (length(location) == 0 && !allow_no_locations) {
    stop("No suitable location found")
  }

  lookup_location_id(location, root)
}


location_build_pull_plan <- function(packet_id, location_id, root) {
  index <- root$index()

  ## Things that are found in any location:
  candidates <- index$location[index$location$location %in% location_id,
                               c("packet", "location")]

  ## Sort by location
  candidates <- candidates[order(match(candidates$location, location_id)), ]

  plan <- data_frame(
    packet = packet_id,
    location_id = candidates$location[match(packet_id, candidates$packet)])
  plan$location_name <- lookup_location_name(plan$location_id, root)

  if (anyNA(plan$location_id)) {
    ## This is going to want eventual improvement before we face
    ## users.  The issues here are that:
    ## * id or location might be vectors (and potentially) quite long
    ##   so formatting the message nicely is not
    ##   straightforward. Better would be to throw an error object
    ##   that takes care of formatting as we can test that more easily
    ## * the id above might include things that the user did not
    ##   directly ask for (but were included as dependencies) and we
    ##   don't capture that intent.
    ## * we might also want to include the human readable name of the
    ##   packet here too (we can get that easily from the index)
    ## * we don't report back how the set of candidate locations was
    ##   resolved (e.g., explicitly given, default)
    msg <- packet_id[is.na(plan$location_id)]
    src <- lookup_location_name(location_id, root)
    stop(sprintf("Failed to find %s at location %s: %s",
                 ngettext(length(msg), "packet", "packets"),
                 paste(squote(src), collapse = ", "),
                 paste(squote(msg), collapse = ", ")))
  }

  plan
}


location_build_push_plan <- function(packet_id, location_id, root) {
  driver <- location_driver(location_id, root)

  packet_id <- sort(find_all_dependencies(packet_id, root$index()$metadata))
  packet_id_msg <- driver$list_unknown_packets(packet_id)

  if (length(packet_id_msg) == 0) {
    files_msg <- character(0)
  } else {
    packet_id_msg <- sort(packet_id_msg)
    metadata <- root$index()$metadata
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
new_location_entry <- function(name, type, args) {
  match_value(type, location_types)
  required <- NULL
  if (type == "path") {
    required <- "path"
  } else if (type == "http") {
    required <- "url"
  } else if (type == "custom") {
    required <- "driver"
  }
  if (length(args) > 0) {
    assert_is(args, "list")
    assert_named(args)
  }
  msg <- setdiff(required, names(args))
  if (length(msg) > 0) {
    stop(sprintf("Fields missing from args: %s",
                 paste(squote(msg), collapse = ", ")))
  }

  if (type == "custom") {
    check_symbol_from_str(args$driver, "args$driver")
  }

  location_id <- paste(as.character(openssl::rand_bytes(4)), collapse = "")
  ## NOTE: make sure this matches the order in config_read
  data_frame(name = name,
             id = location_id,
             type = type,
             args = I(list(args)))
}


lookup_location_id <- function(name, root) {
  root$config$location$id[match(name, root$config$location$name)]
}


local_location_id <- function(root) {
  lookup_location_id(local, root)
}


lookup_location_name <- function(id, root) {
  root$config$location$name[match(id, root$config$location$id)]
}


location_check_new_name <- function(root, name) {
  if (location_exists(root, name)) {
    stop(sprintf("A location with name '%s' already exists",
                 name))
  }
}


location_check_exists <- function(root, name) {
  if (!location_exists(root, name)) {
    stop(sprintf("No location with name '%s' exists",
                 name))
  }
}


location_exists <- function(root, name) {
  name %in% orderly_location_list(root)
}


mark_packets_orphaned <- function(location_id, packet_id, orphan_id, root) {
  location <- file.path(root$path, ".outpack", "location", location_id,
                        packet_id)
  dest <- file.path(root$path, ".outpack", "location", orphan_id, packet_id)
  fs::dir_create(dirname(dest))
  fs::file_move(location, dest)
}
