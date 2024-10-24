##' Purge packets from your local orderly archive.  The interface
##' around this function is expected to change over the next few
##' versions, so please do send us your thoughts.  Here, we talk about
##' the act of deletion and purging interchangeably, and we use the
##' function name `orderly_purge` to emphasise that this should not be
##' the most common thing you do, and that it's quite destructive.
##'
##' In general, you should be mindful about the sorts of archive you
##' might delete from, and think about if deletion is the best
##' approach (see below for alternatives).  We can imagine situations
##' where deletion is quite common though:
##'
##' * Early on in a project where you are experimenting (though you
##'   can also just delete `.outpack` and `archive/` an start again if
##'   you really want a fresh start; see below)
##'
##' * Where your local machine (laptop, workstation, etc) is never the
##'   canonical source of packets and where everything of value ends
##'   up in a shared location (via packit, or a shared filesystem).
##'   In this case, you can always get back packets onto your machine
##'   if you needed them so it may be useful to clean up.
##'
##' * Where you are experimenting locally and you realise that you
##'   need to make sure that unwanted results are not pulled in by
##'   anyone else.
##'
##' Like git, deletion may not always do what you want; if someone
##' else has a copy of your packet, that packet may exist somewhere
##' forever.  `outpack_purge` affects only your local archive.  On the
##' other hand, deletion is quite permanent where a packet is not
##' known anywhere else - once purged it is gone and the operation
##' cannot be undone.
##'
##' # Incomplete dependency graphs
##'
##' By using `orderly_purge` you put yourself at risk of an incomplete
##' dependency graph.  That is, you may end up with packets whose
##' parents have been removed and therefore their dependencies cannot
##' be recovered.  This is generally an undesirable outcome and may
##' cause orderly to fail on some operations so we try and prevent it.
##'
##' The simplest way to prevent this is to specify `children = TRUE`
##' when using `orderly_purge` (this is the default) which will delete
##' all children of any deletred packet.  This leaves things
##' consistent but might be a bit blunt.  If your configuration has
##' `config.core.use_completre_tree = TRUE` set (see
##' [orderly_config_set]) then this is the only option available.
##'
##' Deletion with `children = TRUE` has the nice effect of letting you
##' remove (say) all packets that used some version of a data set that
##' you no longer want to trust.
##'
##' As an alternative, we might want to delete all child packets that
##' cannot be found elsewhere.  This may be useful if you are looking
##' to free up space locally but are using a server that you trust to
##' retain packets.  In this case we can be quite enthusiastic about
##' deleting things because you can always get them back.  But if you
##' have work that exists only on your machine you might want to
##' retain that.  **We do not support this at present**, but will do
##' in a future version.  Let us know if this would be useful.
##'
##' # Alternatives to deletion
##'
##' You may want to consider alterntives to this function, which may
##' be less daunting if this wall of text is making you nervous.
##'
##' ## Start a new root
##'
##' By far the simplest approach.  Just reclone your sources and start
##' again.  You might want to back up your old version in case there
##' was something useful there after all.
##'
##' ## Pull a sub-graph
##'
##' Sometimes you will end up a mix of packets that you like and
##' others that were experimentation and you might want to use
##' `orderly_purge` to remove the latter.  However, it can be easier
##' to instead pull the *good* packets into a new location and start
##' again there.  The basic idea is:
##'
##' 1. Clone your sources into a new directory
##'
##' 2. Set up your *old* archive as an orderly location with
##'   [orderly_location_add_path]
##'
##' 3. Use [orderly_location_pull_packet] with `recursive = TRUE` to
##'   pull ids of any packets that you want to keep.  You only have to
##'   reference leaf (terminal) nodes in your graph and all their
##'   dependencies will be copied over into your location.
##'
##' 4. Remove the location with [orderly_location_remove], after which
##'   you can safely delete (or save as a backup) your old directory.
##'
##' The end result of this process is a new archive that contains a
##' complete and consistent graph of outputs.
##'
##' # Planning your purges
##'
##' By default, `orderly_purge` only calculates what it might delete;
##' this is because the argument `dry_run` is `TRUE` by default.  This
##' is designed to give you some idea of what will happen and to avoid
##' you accidentally deleting everything (remember: there is no undo
##' once you go ahead with the deletion).
##'
##' In addition to printing a summary of deletions to the screen, we
##' will return a list with details about the deletion.  Because this
##' can be quite long it is not printed by default (i.e., it is
##' returned using [invisible()]), but you can save this to an object
##' and inspect it.  See `Value` for details on the format.
##'
##' Note that this summary is subject to race conditions - we will
##' compute a new plan on a second call to `orderly_purge` with
##' `dry_run = FALSE` and there is no guarantee that the deletion will
##' be the same.
##' 
##' @title Purge unwanted packets
##'
##' @inheritParams orderly_search
##'
##' @param parents Indicates if the removal should be recursive,
##'   looking backwards, deleting all packets that were **used** by
##'   the packets listed by `expr`.  This is `FALSE` by default.
##'
##' @param children Indicates if the removal should be recursive,
##'   looking forwards, deleting all packets that **use** the packets
##'   listed by `expr`.  This is `TRUE` by default, which leaves you
##'   with a complete graph (retaining children but deleting a parent
##'   means that we no longer have all the bits that went into a
##'   packet).
##' 
##' @param dry_run Logical, indicating if we should just print (and
##'   return) information about what would be deleted, but not
##'   actually make any changes.
##' 
##' @param force Logical, indicating if we should skip the interactive
##'   confirmation of deletion.
##' 
##' @return Invisibly, a list of information about what will be
##'   deleted.  This has elements:
##'
##' * `packets`: a `data.frame` of the packets to be deleted, with
##'   columns `id`, `name` and `location`.  The `location` column is a
##'   list column which lists other locations that this packet can be
##'   found at.
##'
##' * `archive`: a `data.frame` of files to be deleted from the
##'   archive.  This has columns `name` (the packet name), `id`
##'   (packet id), `path` (relative path within the packet), `size`
##'   (in bytes) and `hash`.  If you are not using an archive
##'   (`config.core.path_archive` is `NULL`) this element is `NULL`.
##'
##' * `store`: a `data.frame` of files to be deleted from the file
##'   store.  This has columns `hash` and `size`.  If you are not
##'   using a file store (`config.core.use_file_store` is `FALSE`)
##'   this element is `NULL`.
##' 
##' @export
orderly_purge <- function(expr, name = NULL, recursive = TRUE, children = TRUE,
                          dry_run = TRUE, force = FALSE, root = NULL) {
  root <- root_open(root, require_orderly = FALSE)
  if (!children && root$config$core$require_complete_tree) {
    cli::cli_abort(
      paste("Can't use `recursive = FALSE` as",
            "`config.core.require_complete_tree` is `TRUE`"))
  }

  info <- purge_plan(expr, name, recursive, root)
  purge_plan_describe(info, root)
  if (dry_run) {
    cli_alert_info("As 'dry_run = TRUE', nothing was deleted")
  } else {
    continue <- prompt_continue_yes_no(
      "OK to make this deletion? This action cannot be undone! [y/N] > ",
      force = force)
    if (continue) {
      purge_delete(info, root)
    } else {
      cli::cli_alert_danger("Cancelling, nothing was deleted")
    }
  }

  invisible(list(packets = info, files = file_info))
}


## I think for this we should be more clever - we should identify
## packets that are not known anywhere else.
purge_plan <- function(expr, name, recursive, root) {
  is_literal_id <- expr_is_literal_id(expr, name)
  if (expr_is_literal_id(expr, name)) {
    ids_requested <- expr
  } else {
    ids_requested <- orderly_search(expr, name = name, root = root)
  }

  index <- root$index$data()
  if (recursive) {
    ids <- find_all_dependencies(ids, index$metadata)
  }

  info <- orderly_metadata_extract(ids_all, root = root)
  files <- orderly_file_info(ids, root)

      path_full <- file.path(root$path, path_archive, files_info$name,
                             files_info$id, files_info$path)

  if (is.null(root$config$core$path_archive)) {
    delete_archive <- NULL
  } else {
    delete_archive <- file.path(root$config$core$path_archive,
                                files_info$name, files_info$id, files_info$path)
  }

  if (root$config$core$use_file_store) {
    data <- root$index$data()$metadata[setdiff(root$index$unpacked(), ids)]
    keep <- unique(unlist(lapply(data, function(x) x$files$hash), FALSE, FALSE))
    delete_store <- setdiff(unique(files_info$hash), keep)
  } else {
    delete_store <- NULL
  }
  
  list(ids = list(is_literal_id = is_literal_id,
                  recursive = recursive,
                  requested = ids_requested,
                  found = ids),
       info = info,
       files = files,
       delete = list(archive = delete_archive,
                     store = delete_store))
}


purge_describe <- function(info, root) {
  n_requested <- length(info$ids$requested)
  n_extra <- length(info$ids$found) - n_requested
  if (info$ids$is_literal_id) {
    cli_alert_info("Given {n_requested} packet{?s} to delete")
  } else {
    cli_alert_info("Search found {n_requested} packet{?s} to delete")
  }

  if (!info$ids$recursive) {
    cli_alert_info("Did not look for dependencies")
  } else if (n_extra == 0) {
    cli_alert_info("No extra packets found as dependencies")
  } else {
    cli_alert_info(
      "Also found {n_extra} packet{?s} to delete as dependencies")
  }

  if (!is.null(info$delete$archive)) {
    path_archive <- root$config$core$path_archive
    n_files_archive <- nrow(info$delete$archive)
    size_files_archive <- sum(info$delete$archive$size)
    cli_alert_info(paste(
      "Packets include {n_files_archive} file{?s} in '{path_archive}/'",
      "totalling {pretty_bytes(size_files_archive)}"))
  }

  if (!is.null(info$delete$store)) {
    n_files_store <- nrow(info$delete$store)
    size_files_store <- sum(info$delete$store$size)
    cli_alert_info(paste(
      "{n_files_store} distinct file{?s} can be removed from the store,",
      "totalling {pretty_bytes(size_files_store)}"))
  }
}


purge_delete <- function(info, root) {
    path_metadata <- file.path(root$path, ".outpack", "metadata", ids_all)
    fs::file_remove(path_metadata)
    cli_alert_success("Deleted {length(path_ids_all)} packet{?s}")

    if (!is.null(path_archive)) {
      path_full <- file.path(root$path, path_archive, files_info$name,
                             files_info$id, files_info$path)
      fs::file_remove(path_full)
      cli_alert_success("Deleted {length(path_full)} file{?s}")
    }
    if (use_file_store) {
      path_store <- root$files$filename(drop)
      fs::file_remove(path_store)
      cli_alert_success("Deleted {length(path_full)} file{?s}")
    }

    cli_alert_info("Rebuilding metadata store")
    root$index$rebuild()
    cli_alert_success("All done")
  }
}


orderly_file_info <- function(ids, root) {
  meta <- lapply(ids, outpack_metadata_core, root = root)
  files <- lapply(meta, "[[", "files")
  len <- vnapply(files, nrow)
  data_frame(
    packet = rep(ids, len),
    name = rep(vcapply(meta, "[[", "name"), len),
    path = unlist(lapply(files, "[[", "path")),
    hash = unlist(lapply(files, "[[", "hash")),
    size = unlist(lapply(files, "[[", "size")))
}
