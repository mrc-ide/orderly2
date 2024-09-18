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

  ret <- lapply(files, function(p) {
    if (is_binary_file(file.path(path_target, p)) ||
        is_binary_file(file.path(path_current, p))) {
      NULL
    } else {
      diffobj::diffChr(
        read_file_lossy(file.path(path_target, p)),
        read_file_lossy(file.path(path_current, p)),
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
##' @param what One or more of "metadata", "files" and "artefacts", retricting
##'   what components of the packet to compare. This is useful when it is known
##'   for example that the source code of a report what changed, and one is only
##'   interested in the effect on its output.
##' @param search_options Options for locating packet files. If there are no
##'   copies of the files locally, they can be downloaded automatically from a
##'   remote location on-demand if `allow_remote` is `TRUE`.
##'
##' @inheritParams orderly_metadata
##'
##' @return An R6 object of class `orderly_packet_diff` is returned. Printing
##'   this object will show the difference in the two packets. Additionally, the
##'   `is_equal` method of the object return a logical indicating whether any
##'   difference was found.
##'
##' @export
orderly_compare_packets <- function(
  target, current, search_options = NULL, root = NULL, locate = TRUE,
  what = c("metadata", "files")) {
  what <- rlang::arg_match(what, multiple = TRUE,
                           values = c("metadata", "files", "artefacts"))
  if (length(what) == 0) {
    cli::cli_abort("{.code what} must not be empty")
  }
  if ("artefacts" %in% what && "files" %in% what) {
    cli::cli_abort('{.code what} must contain both "files" and "artefacts"')
  }

  validate_outpack_id(target, call = environment())
  validate_outpack_id(current, call = environment())

  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())

  meta_target <- orderly_metadata(target, root = root)
  meta_current <- orderly_metadata(current, root = root)

  if ("metadata" %in% what) {
    metadata <- compare_metadata(meta_target, meta_current)
  } else {
    metadata <- NULL
  }

  if ("files" %in% what) {
    files <- compare_filesets(meta_target$files, meta_current$files)
  } else if ("artefacts" %in% what) {
    if (is.null(meta_target$custom$orderly) ||
        is.null(meta_current$custom$orderly)) {
      cli::cli_abort("Cannot compare artefacts of non-orderly packets")
    }

    artefacts_target <- unlist(meta_target$custom$orderly$artefacts$paths)
    artefacts_current <- unlist(meta_current$custom$orderly$artefacts$paths)
    files <- compare_filesets(
      meta_target$files[meta_target$files$path %in% artefacts_target, ],
      meta_current$files[meta_current$files$path %in% artefacts_current, ])
  } else {
    files <- data.frame(path = character(0), status = character(0))
  }

  idx <- files$status == "modified"
  files$diff[idx] <- compare_files(target, current, files[idx, ]$path,
                                   search_options = search_options,
                                   root = root)

  orderly_packet_diff$new(what, target, current, metadata, files)
}


orderly_packet_diff <- R6::R6Class(
  "orderly_packet_diff",
  private = list(
    what = NULL,
    target = NULL,
    current = NULL,
    metadata = NULL,
    files = NULL,

    print_metadata = function() {
      if (!is.null(private$metadata)) {
        cli::cli_alert_warning("Packet metadata differs:")
        cli::cli_div(theme = list(div = list("margin-left" = 2)))
        cli::cli_verbatim(as.character(private$metadata))
        cli::cli_end()
      }
    },

    print_files = function(verbose) {
      name <- if ("artefacts" %in% private$what) {
        "artefacts"
      } else {
        "files"
      }

      removed <- private$files[private$files$status == "removed", ]
      if (nrow(removed) > 0) {
        cli::cli_alert_warning(
          "The following {name} only exist in packet {private$current}:")
        cli::cli_ul(removed$path)
      }

      added <- private$files[private$files$status == "added", ]
      if (nrow(added) > 0) {
        cli::cli_alert_warning(
          "The following {name} only exist in packet {private$target}:")
        cli::cli_ul(added$path)
      }

      modified <- private$files[private$files$status == "modified", ]
      if (nrow(modified) > 0) {
        binary <- sapply(modified$diff, is.null)

        cli::cli_alert_warning(
          paste("The following {name} exist in both packets but have",
                "different contents:"))

        cli::cli_ul()
        for (i in seq_len(nrow(modified))) {
          cli::cli_li("{modified$path[[i]]}")
          if (verbose) {
            if (!binary[[i]]) {
              cli::cli_div(theme = list(div = list("margin-left" = 2)))
              cli::cli_verbatim(as.character(modified$diff[[i]]))
              cli::cli_end()
            }
          }
        }
        if (verbose && any(binary)) {
          cli::cli_alert_warning(
            "Contents of binary file{?s} {modified$path[binary]} were omitted")
        }
        if (!verbose) {
          cli::cli_alert_info(paste(
            "Print the comparison with {.code verbose = TRUE} to display the",
            "differences in the {name}' contents"))
        }
        cli::cli_end()
      }
    }
  ),

  public = list(
    initialize = function(what, target, current, metadata, files) {
      private$what <- what
      private$target <- target
      private$current <- current
      private$metadata <- metadata
      private$files <- files
    },

    is_equal = function() {
      is.null(private$metadata) && all(private$files$status == "unchanged")
    },

    format = function(verbose = FALSE, ...) {
      target <- private$target
      current <- private$current

      cli::cli_format_method({
        if (self$is_equal()) {
          msg <- if (setequal(private$what, c("metadata", "files"))) {
            "Packets {target} and {current} are identical"
          } else if (identical(private$what, "metadata")) {
            "Metadata of packets {target} and {current} is identical"
          } else if (identical(private$what, "files")) {
            "Files of packets {target} and {current} are identical"
          } else if (identical(private$what, "artefacts")) {
            "Artefacts of packets {target} and {current} are identical"
          } else if (setequal(private$what, c("metadata", "artefacts"))) {
            paste("Metadata and artefacts of packets {target} and {current}",
                  "are identical")
          } else {
            stop("Unhandled combination of `what`")
          }
          cli::cli_alert_success(msg)
        } else {
          cli::cli_alert_info(
            "Comparing packets {private$target} and {private$current}")

          private$print_metadata()
          private$print_files(verbose = verbose)
        }
      })
    }
  )
)
