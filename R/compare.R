is_binary_file <- function(path, n = 1024) {
  # This is a pretty crude heuristic, but it seems good enough.
  # It is actually similar to what the `diff` tool does.
  data <- readBin(path, "raw", n)
  as.raw(0) %in% data
}

compare_metadata <- function(target, current) {
  # id and time will almost always differ, but not in any interesting way.
  # files may differ (especially the hashes), but we compare the files in
  # detail seperately.
  exclude <- c("id", "time", "files")

  target_filtered <- target[!(names(target) %in% exclude)]
  current_filtered <- current[!(names(current) %in% exclude)]
  if (!identical(target_filtered, current_filtered)) {
    diffobj::diffPrint(
      target_filtered,
      current_filtered,
      tar.banner = target$id,
      cur.banner = current$id,
      rds = FALSE,
      mode = "unified",
      style = list(pad = FALSE, wrap = FALSE),
      interactive = FALSE)
  } else {
    NULL
  }
}

compare_filesets <- function(target, current) {
  files <- merge(x = target, y = current, by = "path", all = TRUE)
  status <- ifelse(
    is.na(files$hash.x), "added",
    ifelse(is.na(files$hash.y), "removed",
           ifelse(files$hash.x == files$hash.y, "unchanged", "modified")))
  data.frame(path = files$path, status = status)
}

compare_files <- function(target, current, files, root, search_options) {
  if (is.null(files)) {
    return(NULL)
  }

  path_target <- withr::local_tempdir()
  path_current <- withr::local_tempdir()

  # Copying the files into a temporary directory is fairly wasteful and, as
  # long as the packet is unpacked already, we could read the files from the
  # archive or file store directly. Nevertheless this makes accessing the files
  # very straightforward, and covers the case where the file only exists
  # remotely transparent.

  orderly_copy_files(target, dest = path_target, files = files,
                     options = search_options, root = root)

  orderly_copy_files(current, dest = path_current, files = files,
                     options = search_options, root = root)

  readFile <- function(path) {
    # This tries to be robust even in the face of bad characters.
    iconv(readLines(path, warn = FALSE), "UTF-8", "UTF-8", sub="byte")
  }

  ret <- lapply(files, function(p) {
    if (is_binary_file(file.path(path_target, p)) ||
        is_binary_file(file.path(path_current, p))) {
      NULL
    } else {
      diffobj::diffChr(
        readFile(file.path(path_target, p)),
        readFile(file.path(path_current, p)),
        tar.banner = file.path(target, p),
        cur.banner = file.path(current, p),
        rds = FALSE,
        mode = "unified",
        style = list(pad = FALSE, wrap = FALSE),
        interactive = FALSE)
    }
  })
  names(ret) <- files

  ret
}

##' Compare the metadata and contents of two packets.
##'
##' Insignificant differences in the metadata (eg. different dates and packet
##' IDs) are excluded from the comparison.
##'
##' If either packet is not unpacked, but `search_options`'s `allow_remote` is
##' `TRUE`, we will try to request files from remote locations, as necessary.
##'
##' @title Compare two packets
##'
##' @param target The id of the packet to use in the comparison.
##' @param current The id of the other packet against which to compare.
##' @param what One of "everything" (the default), "metadata", "files" or
##'   "artefacts", retricting what components of the packet to compare. This is
##'   useful when it is known for example that the source code of a report what
##'   changed, and one is only interested in the effect on its output.
##' @param search_options Options for locating packet files. If there are no
##'   copies of the files locally, they can be downloaded automatically from a
##'   remote location on-demand if `allow_remote` is `TRUE`.
##'
##' @inheritParams orderly_metadata
##'
##' @return If the packets have identical contents, TRUE is returned. Otherwise
##'   an object detailing the differences is returned. While the object can be
##'   inspected, its contents is subject to change. In both cases, the returned
##'   value has class `orderly_compare_packets`, allowing a user friendly
##'   display of the result.
##'
##' @export
orderly_compare_packets <- function(
  target, current, what = c("everything", "metadata", "files", "artefacts"),
  search_options = NULL, root = NULL, locate = TRUE) {
  validate_outpack_id(target, call = environment())
  validate_outpack_id(current, call = environment())
  what <- rlang::arg_match(what)

  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())

  meta_target <- orderly_metadata(target, root = root)
  meta_current <- orderly_metadata(current, root = root)
  if (what %in% c("everything", "metadata")) {
    metadata_diff <- compare_metadata(meta_target, meta_current)
  } else {
    metadata_diff <- NULL
  }

  if (what == "artefacts") {
    if (is.null(meta_target$custom$orderly) ||
        is.null(meta_current$custom$orderly)) {
      cli::cli_abort("Cannot compare artefacts of non-orderly packets")
    }

    artefacts_target <- unlist(meta_target$custom$orderly$artefacts$paths)
    artefacts_current <- unlist(meta_current$custom$orderly$artefacts$paths)
    files <- compare_filesets(
      meta_target$files[meta_target$files$path %in% artefacts_target, ],
      meta_current$files[meta_current$files$path %in% artefacts_current, ])
  } else if (what %in% c("everything", "files")) {
    files <- compare_filesets(meta_target$files, meta_current$files)
  } else {
    files <- data.frame(path = NULL, status = NULL)
  }

  if (is.null(metadata_diff) && all(files$status == "unchanged")) {
    ret <- TRUE
  } else {
    idx <- files$status == "modified"
    files$diff[idx] <- compare_files(target, current, files[idx, ]$path,
                                     search_options = search_options,
                                     root = root)

    ret <- list(packets = c(target = target, current = current),
                metadata_diff = metadata_diff,
                files = files)
  }

  class(ret) <- "orderly_compare_packets"

  ret
}


#' @export
format.orderly_compare_packets <- function(x, ...) {
  cli::cli_format_method({
    if (isTRUE(x)) {
      cli::cli_alert_success("Packets are identical")
    } else {
      target <- x$packets[[1]]
      current <- x$packets[[2]]

      cli::cli_alert_info("Comparing packets {target} and {current}")

      if (!is.null(x$metadata_diff)) {
        cli::cli_alert_warning("Packet metadata differs:")
        cli::cli_div(theme = list(div = list("margin-left" = 2)))
        cli::cli_verbatim(as.character(x$metadata_diff))
        cli::cli_end()
      }

      removed <- x$files[x$files$status == "removed", ]
      if (nrow(removed) > 0) {
        cli::cli_alert_warning(
          "The following files only exist in packet {current}:")
        cli::cli_ul(removed$path)
      }

      added <- x$files[x$files$status == "added", ]
      if (nrow(added) > 0) {
        cli::cli_alert_warning(
          "The following files only exist in packet {target}:")
        cli::cli_ul(added$path)
      }

      modified <- x$files[x$files$status == "modified", ]
      if (nrow(modified) > 0) {
        cli::cli_alert_warning(paste("The following files exist in both",
                                     "packets but have different contents:"))
        cli::cli_ul()
        for (i in seq_len(nrow(modified))) {
          cli::cli_li("{modified$path[[i]]}")
          if (!is.null(modified$diff[[i]])) {
            cli::cli_div(theme = list(div = list("margin-left" = 2)))
            cli::cli_verbatim(as.character(modified$diff[[i]]))
            cli::cli_end()
          }
        }
        cli::cli_end()
      }
    }
  })
}


#' @export
print.orderly_compare_packets <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

#' @export
`[.orderly_compare_packets` <- function(x, paths) {
  x$files <- x$files[x$files$path %in% paths,, drop=FALSE]
  x
}

#' @export
summary.orderly_compare_packets <- function(object, ...) {
  object$files$diff <- c()
  object
}
