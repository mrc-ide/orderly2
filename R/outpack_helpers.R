##' Copy files from a packet to anywhere. Similar to
##' [orderly2::orderly_dependency] except that this is not used in an
##' active packet context. You can use this function to pull files
##' from an outpack root to a directory outside of the control of
##' outpack, for example. Note that all arguments need must be
##' provided by name, not position, with the exception of the id or
##' query.
##'
##' You can call this function with an id as a string, in which case
##' we do not search for the packet and proceed regardless of whether
##' or not this id is present.  If called with any other arguments
##' (e.g., a string that does not match the id format, or a named
##' argument `name`, `subquery` or `parameters`) then we interpret the
##' arguments as a query and [orderly2::orderly_search] to find the
##' id. It is an error if this query does not return exactly one
##' packet id, so you probably want to use `latest()`.
##'
##' There are different ways that this might fail (or recover from
##' failure):
##'
##' * if `id` is not known in the metadata store (not known because
##'   it's not unpacked but also not known to be present in some other
##'   remote) then this will fail because it's impossible to resolve
##'   the files. Consider refreshing the metadata with
##'   [orderly2::orderly_location_metadata_fetch] to refresh this.
##' * if the `id` is not unpacked *and* no local copy of the files
##'   referred to can be found, we error by default (but see the next
##'   option). However, sometimes the file you refer to might also be
##'   present because you have downloaded a packet that depended on
##'   it, or because the content of the file is unchanged because from
##'   some other packet version you have locally.
##' * if the `id` is not unpacked, there is no local copy of the file
##'   and if `allow_remote` is `TRUE` we will try and request the file
##'   from whatever remote would be selected by
##'   [orderly2::orderly_location_pull] for this packet.
##'
##' Note that empty directories might be created on failure.
##'
##' @title Copy files from a packet
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
##' @param dest The directory to copy into
##'
##' @param overwrite Overwrite files at the destination; this is
##'   typically what you want, but set to `FALSE` if you would prefer
##'   that an error be thrown if the destination file already exists.
##'
##' @inheritParams orderly_search
##' @inheritParams orderly_search_options
##' @inheritParams orderly_metadata
##'
##' @return Nothing, invisibly. Primarily called for its side effect
##'   of copying files from a packet into the directory `dest`
##'
##' @export
orderly_copy_files <- function(expr, files, dest, overwrite = TRUE,
                               name = NULL, location = NULL,
                               allow_remote = NULL, pull_metadata = FALSE,
                               parameters = NULL, options = NULL,
                               envir = parent.frame(),
                               root = NULL) {
  root <- root_open(root, require_orderly = FALSE)
  compatibility_fix_options(options, "orderly_copy_files")
  ## Validate options here so we can refer to the computed value of
  ## allow_remote later in error messages.
  options <- build_search_options(location = location,
                                  allow_remote = allow_remote,
                                  pull_metadata = pull_metadata)

  ## Validate files and dest early; it gives a better error where this
  ## was not provided with names.
  files <- validate_file_from_to(files, envir)
  assert_scalar_character(dest, call = environment())

  if (expr_is_literal_id(expr, name)) {
    id <- expr
    if (length(id) != 1) {
      cli::cli_abort(
        "Expected a length 1 value for 'expr' if id (not {length(id)})",
        arg = expr)
    }
  } else {
    id <- orderly_search(expr,
                         name = name,
                         parameters = parameters,
                         location = options$location,
                         allow_remote = options$allow_remote,
                         pull_metadata = options$pull_metadata,
                         root = root)
    if (length(id) > 1) {
      cli::cli_abort(
        c("Query returned {length(id)} results, expected a single result",
          i = "Did you forget latest()?"))
    }
    if (length(id) == 0 || is.na(id)) {
      explanation <- orderly_query_explain(
        expr, name = name, parameters = parameters,
        location = options$location,
        allow_remote = options$allow_remote,
        envir = envir, root = root)
      cli::cli_abort(
        c("Query returned 0 results",
          i = "See 'rlang::last_error()$explanation' for details"),
        explanation = explanation)
    }
  }

  plan <- plan_copy_files(root, id, files, environment())
  name <- outpack_metadata_core(id, root)$name

  tryCatch(
    file_export(root, id, plan$there, plan$here, dest, overwrite),
    not_found_error = function(e) {
      if (id %in% root$index$unpacked()) {
        ## The most likely reason for things to have failed is that
        ## the user has deleted part of the archive.
        name <- outpack_metadata_core(id, root)$name
        packet_exists <- file.exists(
          file.path(root$path, root$config$core$path_archive, name, id))
        reason <- if (packet_exists) "locally modified" else "deleted"
        cmd <- sprintf(
          'orderly2::orderly_validate_archive("%s", action = "orphan")', id)
        cli::cli_abort(
          c("Unable to copy files, due to {reason} packet {id}",
            i = "Consider '{cmd}' to remove this packet from consideration"),
          parent = e)
      } else if (!options$allow_remote) {
        cli::cli_abort(
          c("Unable to copy files, as they are not available locally",
            i = paste("To fetch from a location, try again with",
                      "{.code allow_remote = TRUE}")),
          parent = e)
      }
      copy_files_from_remote(id, plan$there, plan$here, dest, overwrite, root,
                             environment())
    })

  invisible(list(id = id, name = name, files = plan))
}


plan_copy_files <- function(root, id, files, call = NULL) {
  assert_relative_path(files$there, name = "File", workdir = id, call = call)
  validate_packet_has_file(root, id, files$there, call)

  meta <- outpack_metadata_core(id, root)
  is_dir <- function(p) grepl("/$", p)
  list_files <- function(p) {
    j <- string_starts_with(p, meta$files$path)
    string_drop_prefix(p, meta$files$path[j])
  }
  expand_dirs_virtual(files, is_dir, list_files)
}


copy_files_from_remote <- function(id, there, here, dest, overwrite, root,
                                   call = NULL) {
  plan <- location_build_pull_plan(id, location = NULL, recursive = FALSE,
                                   root = root, call = call)
  meta <- outpack_metadata_core(id, root)
  hash <- meta$files$hash[match(there, meta$files$path)]
  here_full <- file.path(dest, here)
  store <- location_pull_files(plan$files[plan$files$hash %in% hash, ], root)
  store$value$get(hash, here_full, overwrite)
  store$cleanup()
}
