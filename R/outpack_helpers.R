##' Copy files from a packet to anywhere. Similar to
##' [orderly2::orderly_dependency] except that this is not used in an
##' active packet context. You can use this function to pull files
##' from an outpack root to a directory outside of the control of
##' outpack, for example.
##'
##' There are different ways that this might fail (or recover from
##' failure):
##'
##' * if `id` is not known in the metadata store (not known because
##'   it's not unpacked but also not known to be present in some other
##'   remote) then this will fail because it's impossible to resolve
##'   the files. Consider refreshing the metadata with
##'   [orderly2::outpack_location_pull_metadata] to refresh this.
##' * if the `id` is not unpacked *and* no local copy of the files
##'   referred to can be found, we error by default (but see the next
##'   option). However, sometimes the file you refer to might also be
##'   present because you have downloaded a packet that depended on
##'   it, or because the content of the file is unchanged because from
##'   some other packet version you have locally.
##' * if the `id` is not unpacked, there is no local copy of the file
##'   and if `allow_remote` is `TRUE` we will try and request the file
##'   from whatever remote would be selected by
##'   [orderly2::outpack_location_pull_packet] for this packet.
##'
##' Note that empty directories might be created on failure.
##'
##' @title Copy files from a packet
##'
##' @param id Id of the packet to copy from (will become a query, see
##'   mrc-4418)
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
##' @param allow_remote Logical, indicating if we should attempt to
##'   retrieve the file from any remote location if it cannot be found
##'   locally. If the file is large, this may take some time depending
##'   on the speed of the connection. If you use a file store, note
##'   that this does add the downloaded file into your file store,
##'   though associated with no packet so that it is subject to
##'   garbage collection (once we write support for that).
##'
##' @param overwrite Overwrite files at the destination; this is
##'   typically what you want, but set to `FALSE` if you would prefer
##'   that an error be thrown if the destination file already exists.
##'
##' @param envir An environment into which string interpolation may
##'   happen (see the `files` argument for details on the string
##'   interpolation).  The default here is to use the calling
##'   environment, which is typically reasonable, but may need
##'   changing in programmatic use.
##'
##' @inheritParams orderly_metadata
##'
##' @return Nothing, invisibly. Primarily called for its side effect
##'   of copying files from a packet into the directory `dest`
##'
##' @export
orderly_copy_files <- function(id, files, dest, allow_remote = FALSE,
                               overwrite = TRUE, envir = parent.frame(),
                               root = NULL, locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())

  files <- validate_file_from_to(files, envir)
  plan <- plan_copy_files(root, id, files$from, files$to)

  tryCatch(
    file_export(root, id, plan$there, plan$here, dest, overwrite),
    not_found_error = function(e) {
      if (!allow_remote) {
        stop(paste0(
          "Unable to copy files, as they are not available locally\n",
          "To fetch from a location, try again with 'allow_remote = TRUE'\n",
          "Original error:\n", e$message),
          call. = FALSE)
      }
      copy_files_from_remote(id, plan$there, plan$here, dest, overwrite, root)
    })

  invisible(plan)
}


plan_copy_files <- function(root, id, there, here) {
  assert_relative_path(there, no_dots = TRUE)
  validate_packet_has_file(root, id, there)
  meta <- root$metadata(id)
  is_dir <- grepl("/$", there)
  if (any(is_dir)) {
    files <- meta$files$path
    expanded <- lapply(which(is_dir), function(i) {
      p <- there[[i]]
      j <- string_starts_with(p, files)
      set_names(files[j],
                file.path(here[[i]], string_drop_prefix(p, files[j])))
    })

    there <- replace_ragged(there, is_dir, lapply(expanded, unname))
    here <- replace_ragged(here, is_dir, lapply(expanded, names))
  }
  data_frame(there, here)
}


## We don't want here to necessarily download all of these files; some
## might be found locally.
copy_files_from_remote <- function(id, there, here, dest, overwrite, root) {
  location_id <- location_resolve_valid(NULL, root,
                                        include_local = FALSE,
                                        allow_no_locations = FALSE)
  plan <- location_build_pull_plan(id, location_id, root)
  driver <- location_driver(plan$location_id[match(id, plan$packet)], root)

  meta <- root$metadata(id)
  hash <- meta$files$hash[match(there, meta$files$path)]
  here_full <- file.path(dest, here)

  if (root$config$core$use_file_store) {
    hash_msg <- hash[!root$files$exists(hash)]
    location_pull_hash_store(root, driver, hash_msg)
    root$files$get(hash, here_full, overwrite)
  } else {
    src <- lapply(hash, function(h) find_file_by_hash(root, h))
    is_missing <- vlapply(src, is.null)
    hash_msg <- hash[is_missing]
    location_pull_hash_archive(root, driver, hash[is_missing],
                               here_full[is_missing])
    fs::file_copy(list_to_character(src[!is_missing]),
                  here_full[!is_missing],
                  overwrite = overwrite)
  }
}
