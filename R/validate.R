##' Validate unpacked packets. Over time, expect this function to
##' become more fully featured, validating more.
##'
##' The actions that we can take on finding an invalid packet are:
##'
##' * `inform` (the default): just print information about the problem
##'
##' * `orphan`: mark the packet as orphaned within the metadata, but
##'   do not touch the files in your archive (by default the directory
##'   `archive/`) - this is a safe option and will leave you in a
##'   consistent state without deleting anything.
##'
##' * `delete`: in addition to marking the packet as an orphan, also
##'   delete the files from your archive.
##'
##' Later, we will add a "repair" option to try and fix broken
##' packets.
##'
##' The validation interacts with the option
##' `core.require_complete_tree`; if this option is `TRUE`, then a
##' packet is only valid if all its (recursive) dependencies are also
##' valid, so the action will apply to packets that have also had
##' their upstream dependencies invalidated.  This validation will
##' happen even if the query implied by `...` does not include these
##' packets if a complete tree is required.
##'
##' The validation will also interact with `core.use_file_store` once
##' repair is supported, as this becomes trivial.
##'
##' @title Validate unpacked packets.
##'
##' @param action The action to take on finding an invalid packet. See
##'   Details.
##'
##' @inheritParams orderly_metadata
##' @inheritParams orderly_search
##' @inheritParams orderly_search_options
##'
##' @return Invisibly, a character vector of repaired (or invalid)
##'   packets.
##'
##' @export
orderly_validate_archive <- function(expr = NULL, name = NULL,
                                     action = "inform", root = NULL) {
  root <- root_open(root, require_orderly = FALSE)
  action <- match_value(action, c("inform", "orphan", "delete", "repair"),
                        call = environment())

  if (is.null(root$config$core$path_archive)) {
    cli::cli_abort("You have no archive to validate")
  }

  if (expr_is_literal_id(expr, name)) {
    ids <- expr
  } else {
    ids <- orderly_search(expr,
                          name = name,
                          location = "local",
                          allow_remote = FALSE,
                          fetch_metadata = FALSE,
                          root = root)
  }

  cache <- new.env(parent = emptyenv())
  for (id in sort(ids)) {
    cache[[id]] <- orderly_validate_archive_packet(id, action, cache, root)
  }
  res <- as.list(cache)
  invalid <- sort(names(Filter(function(x) !x$valid, res)))
  if (length(invalid) > 0) {
    root$index$rebuild()
  }
  invisible(invalid)
}


orderly_validate_archive_packet <- function(id, action, cache, root) {
  if (!is.null(cache[[id]])) {
    return(cache[[id]])
  }

  res <- orderly_validate_archive_packet_check(id, action, cache, root)
  cache[[id]] <- res

  if (res$valid) {
    cli::cli_alert_success("{res$id} ({res$name}) is valid")
  } else {
    reason <- c(if (!all(res$files$valid)) "files",
                if (!all(res$depends$valid)) "upstream packets")
    cli::cli_alert_danger(
      "{res$id} ({res$name}) is invalid due to its {reason}")
    if (action == "orphan") {
      drop_local_packet(id, root)
    } else if (action == "delete") {
      drop_local_packet(id, root)
      fs::dir_delete(
        file.path(root$path, root$config$core$path_archive, res$name, id))
    }
  }

  res
}


orderly_validate_archive_packet_check <- function(id, action, cache, root) {
  meta <- outpack_metadata_core(id, root)
  name <- meta$name
  path <- file.path(root$path, root$config$core$path_archive, name, id)

  depends <- meta$depends
  depends$valid <- rep(TRUE, nrow(depends))
  if (root$config$core$require_complete_tree) {
    for (i in seq_len(nrow(depends))) {
      depends$valid[[i]] <- orderly_validate_archive_packet(
        depends$packet[[i]], action, cache, root)$valid
    }
  }

  hash_compare <- function(path, hash) {
    if (!file.exists(path)) NA_character_ else rehash(path, hash_file, hash)
  }
  files <- meta$files
  files$hash_found <- list_to_character(
    Map(hash_compare, file.path(path, files$path), files$hash))
  files$valid <- !is.na(files$hash_found) & files$hash_found == files$hash

  valid <- all(files$valid) && all(depends$valid)

  list(valid = valid, id = id, name = name, files = files, depends = depends)
}
