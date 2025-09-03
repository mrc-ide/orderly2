trivial_differences <- c("id", "time")
vectorized_identical <- function(left, right) {
  stopifnot(length(left) == length(right))
  vlapply(seq_along(left), function(i) {
    identical(left[[i]], right[[i]])
  })
}

is_binary_file <- function(path, n = 1024) {
  vlapply(path, function(p) {
    # This is a pretty crude heuristic, but it seems good enough.
    # It is actually similar to what the `diff` tool does.
    data <- readBin(p, "raw", n)
    as.raw(0) %in% data
  })
}

compare_filesets <- function(target, current) {
  files <- merge(x = target, y = current, by = "path", all = TRUE)
  status <- ifelse(
    is.na(files$hash.x), "added",
    ifelse(is.na(files$hash.y), "removed",
           ifelse(files$hash.x == files$hash.y, "unchanged", "modified")))
  data.frame(path = files$path, status = status)
}

##' Compare the metadata and contents of two packets.
##'
##' Insignificant differences in the metadata (e.g., different dates
##' and packet IDs) are excluded from the comparison.
##'
##' @param target The id of the packet to use in the comparison.
##' @param current The id of the other packet against which to compare.
##' @inheritParams orderly_search
##' @inheritParams orderly_metadata
##' @return An object of class orderly_comparison. The object can be printed to
##'  get a summary description of the differences, or passed to
##'  [orderly::orderly_comparison_explain] to display more details.
##'
##' @export
##' @examples
##'
##' # Here are two packets that are equivalent, differing only in id
##' # and times:
##' path <- orderly_example()
##' id1 <- orderly_run("data", root = path)
##' id2 <- orderly_run("data", root = path)
##' orderly_compare_packets(id1, id2, root = path)
##'
##' # A more interesting comparison:
##' id1 <- orderly_run("parameters", list(max_cyl = 6), root = path)
##' id2 <- orderly_run("parameters", list(max_cyl = 4), root = path)
##' cmp <- orderly_compare_packets(id1, id2, root = path)
##' cmp
##'
##' # A verbose comparison will show differences in the constituent
##' # components of each packet:
##' orderly_comparison_explain(cmp, verbose = TRUE)
orderly_compare_packets <- function(target, current,
                                    location = NULL,
                                    allow_remote = NULL,
                                    fetch_metadata = FALSE,
                                    root = NULL) {
  root <- root_open(root, require_orderly = FALSE)
  validate_outpack_id(target, call = environment())
  validate_outpack_id(current, call = environment())

  options <- build_search_options(location = location,
                                  allow_remote = allow_remote,
                                  fetch_metadata = fetch_metadata)

  if (options$fetch_metadata) {
    orderly_location_fetch_metadata(options$location, root = root)
  }

  target_metadata <- orderly_metadata(target, root = root)
  current_metdata <- orderly_metadata(current, root = root)

  ret <- list(
    root = root,
    target = target_metadata,
    current = current_metdata,
    status = compare_attribute_list(target_metadata, current_metdata),
    search_options = options)

  class(ret) <- "orderly_comparison"
  ret
}

compare_files <- function(cmp, verbose) {
  diff <- compare_filesets(cmp$target$files, cmp$current$files)

  modified <- diff[diff$status == "modified", ]
  target_only <- diff[diff$status == "removed", ]
  current_only <- diff[diff$status == "added", ]

  if (nrow(target_only) > 0) {
    cli::cli_alert_info(
      "The following files only exist in packet {cmp$target$id}")
    cli::cli_ul(target_only$path)
  }
  if (nrow(current_only) > 0) {
    cli::cli_alert_info(
      "The following files only exist in packet {cmp$current$id}")
    cli::cli_ul(current_only$path)
  }
  if (nrow(modified) > 0) {
    if (verbose) {
      compare_file_contents(cmp, modified$path)
    } else {
      cli::cli_alert_info(paste("The following files exist in both packets",
                                "but have different contents:"))
      cli::cli_ul(modified$path)

      hint <- "orderly_comparison_explain(..., \"files\", verbose = TRUE)"
      cli::cli_alert_info("Use {.code {hint}} to compare the files' contents.")
    }
  }
  if (nrow(modified) == 0 &&
      nrow(target_only) == 0 &&
      nrow(current_only) == 0) {
    cli::cli_alert_info("The files across the two packets are identical.")
  }
}

compare_file_contents <- function(cmp, files) {
  path_target <- withr::local_tempdir()
  path_current <- withr::local_tempdir()

  # Copying the files into a temporary directory is fairly wasteful and, as
  # long as the packet is unpacked already, we could read the files from the
  # archive or file store directly. Nevertheless this makes accessing the files
  # very straightforward, and covers the case where the file only exists
  # remotely transparent.
  #
  # fetch_metadata is intentionally hardcoded to FALSE, regardless of what the
  # user may have specified as an argument to orderly_compare_packets: the
  # latter would have already pulled the files once, and we don't need to do it
  # again.
  orderly_copy_files(cmp$target$id,
                     dest = path_target,
                     files = files,
                     location = cmp$search_options$location,
                     allow_remote = cmp$search_options$allow_remote,
                     fetch_metadata = FALSE,
                     root = cmp$root)

  orderly_copy_files(cmp$current$id,
                     dest = path_current,
                     files = files,
                     location = cmp$search_options$location,
                     allow_remote = cmp$search_options$allow_remote,
                     fetch_metadata = FALSE,
                     root = cmp$root)


  binary_files <- is_binary_file(file.path(path_target, files)) |
                  is_binary_file(file.path(path_current, files))

  for (f in files[!binary_files]) {
    cli::cli_verbatim(as.character(diffobj::diffChr(
      read_file_lossy(file.path(path_target, f)),
      read_file_lossy(file.path(path_current, f)),
      tar.banner = file.path(cmp$target$id, f),
      cur.banner = file.path(cmp$current$id, f),
      rds = FALSE,
      mode = "unified",
      style = list(wrap = FALSE),
      interactive = FALSE
    )))
  }

  if (any(binary_files)) {
    cli::cli_alert_warning(
      paste("The following files differ across packets, but could not be",
            "compared as their content is binary:"))
    cli::cli_ul(files[binary_files])
  }
}

compare_attribute <- function(cmp, attribute, verbose) {
  assert_is(cmp, "orderly_comparison")
  assert_scalar_character(attribute)
  assert_logical(verbose)

  if (attribute == "files") {
    compare_files(cmp, verbose)
  } else {
    cli::cli_alert_info("Comparing attribute {.code {attribute}}")
    cli::cli_verbatim(as.character(diffobj::diffPrint(
      cmp$target[[attribute]],
      cmp$current[[attribute]],
      tar.banner = sprintf("%s$%s", cmp$target$id, attribute),
      cur.banner = sprintf("%s$%s", cmp$current$id, attribute),
      rds = FALSE,
      mode = "unified",
      style = list(wrap = FALSE),
      interactive = FALSE
    )))
  }
}

compare_attribute_list <- function(target, current) {
  target_names <- names(target)
  current_names <- names(current)

  all_names <- union(target_names, current_names)
  status <- rep(NA_character_, length(all_names))

  in_target <- all_names %in% target_names
  in_current <- all_names %in% current_names
  in_both <- in_target & in_current

  status[!in_target] <- "removed"
  status[!in_current] <- "added"

  intersected_names <- all_names[in_both]
  status[in_both] <- ifelse(vectorized_identical(target[intersected_names],
                                                 current[intersected_names]),
                            "identical",
                            "different")

  names(status) <- all_names
  status
}

##' @export
format.orderly_comparison <- function(x, ...) {
  cli::cli_format_method({
    orderly_comparison_explain(x, verbose = "summary")
  })
}

##' @export
print.orderly_comparison <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

##' Print the details of a packet comparison.
##'
##' This function allows to select what part of the packet to compare, and in
##' how much details.
##'
##' @param cmp An orderly_comparison object, as returned by
##'  [orderly::orderly_compare_packets].
##' @param attributes A character vector of attributes to include in the
##'  comparison. The values are keys of the packets' metadata, such as
##'  `parameters` or `files`. If NULL, the default, all attributes are compared,
##'   except those that differ in trivial way (i.e., `id` and `time`).
##' @param verbose Control over how much information is printed. It can either
##'   be a logical, or a character scalar `silent` or `summary`.
##' @return Invisibly, a logical indicating whether the packets are equivalent,
##'   up to the given attributes.
##'
##' @export
##' @examples
##' path <- orderly_example()
##' id1 <- orderly_run("parameters", list(max_cyl = 6), root = path)
##' id2 <- orderly_run("parameters", list(max_cyl = 4), root = path)
##' cmp <- orderly_compare_packets(id1, id2, root = path)
##'
##' orderly_comparison_explain(cmp)
##' orderly_comparison_explain(cmp, verbose = TRUE)
##' orderly_comparison_explain(cmp, "parameters", verbose = TRUE)
orderly_comparison_explain <- function(cmp, attributes  = NULL,
                                       verbose = FALSE) {
  assert_is(cmp, "orderly_comparison")
  if (!is.null(attributes)) {
    assert_character(attributes)
  }

  if (is.null(attributes)) {
    status <- cmp$status[setdiff(names(cmp$status), trivial_differences)]
  } else {
    status <- cmp$status[intersect(attributes, names(cmp$status))]
  }

  if (verbose != "silent") {
    cli::cli_alert_info(
      "Comparing packets {cmp$target$id} and {cmp$current$id}...")

    # All packets we produce have the same attributes. This is really more about
    # future-proofing for a time where we may add new ones, and may want to
    # compare a packet produced before vs after the new attributes were added.
    if (any(status == "added")) {
      cli::cli_alert_info(
        "The following attributes only exist in packet {cmp$target$id}:")
      cli::cli_ul(names(which(status == "added")))
    }
    if (any(status == "removed")) {
      cli::cli_alert_info(
        "The following attributes only exist in packet {cmp$current$id}:")
      cli::cli_ul(names(which(status == "removed")))
    }
    if (any(status == "different")) {
      if (verbose == "summary") {
        cli::cli_alert_info(
          "The following attributes are different across the two packets:")
        cli::cli_ul(names(which(status == "different")))
        cli::cli_div()
        cli::cli_alert_info(paste(
          "Use {.code orderly_comparison_explain(...)} to examine the",
          "differences in more detail."))
        cli::cli_end()
      } else {
        for (n in names(which(status == "different"))) {
          compare_attribute(cmp, attribute = n, verbose = verbose)
        }
      }
    }
    if (all(status == "identical")) {
      if (is.null(attributes)) {
        if (all(cmp$status == "identical")) {
          cli::cli_alert_success("The two packets are identical.")
        } else {
          cli::cli_alert_success(
            "The two packets are equivalent, up to trivial differences.")
        }
      } else {
        cli::cli_alert_success(
          "The specified attributes are identical across the two packets.")
      }
    }
  }

  invisible(all(status == "identical"))
}
