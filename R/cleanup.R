##' Find, and delete, file that were generated by running a report.
##' Until you're comfortable with what this will do, you are strongly
##' recommended to run `orderly_cleanup_status` first to see what will
##' be deleted.
##'
##' After file deletion, we look through and remove all empty
##' directories; orderly2 has similar semantics here to git where
##' directories are never directly tracked.
##'
##' For recent `gert` we will ask git if files are ignored; if ignored
##' then they are good candidates for deletion! We encourage you to
##' keep a per-report `.gitignore` that lists files that will copy
##' into the source directory, and then we can use that same
##' information to clean up these files after generation.
##' Importantly, even if a file matches an ignore rule but has been
##' committed to your repository, it will no longer match the ignore
##' rule.
##'
##' @section Notes for user of orderly1:
##'
##' In orderly1 this function has quite different semantics, because
##'   the full set of possible files is always knowable from the yaml
##'   file. So there, we start from the point of view of the list of
##'   files then compare that with the directory.
##'
##' @title Clean up source directory
##'
##' @param name Name of the report directory to clean (i.e., we look
##'   at `src/<name>` relative to your orderly root
##'
##' @param dry_run Logical, indicating if we should *not* delete
##'   anything, but instead just print information about what we would
##'   do
##'
##' @inheritParams orderly_run
##'
##' @return An (currently unstable) object of class
##'   `orderly_cleanup_status` within which the element `delete`
##'   indicates files that would be deleted (for
##'   `orderly_cleanup_status`) or that were deleted (for
##'   `orderly_cleanup`)
##'
##' @export
##' @examples
##' # Create a simple example:
##' path <- orderly2::orderly_example("default")
##'
##' # We simulate running a packet interactively by using 'source';
##' # you might have run this line-by-line, or with the "Source"
##' # button in Rstudio.
##' source(file.path(path, "src/data/data.R"), chdir = TRUE)
##'
##' # Having run this, the output of the report is present in the
##' # source directory:
##' fs::dir_tree(path)
##'
##' # We can detect what might want cleaning up by running
##' # "orderly_cleanup_status":
##' orderly2::orderly_cleanup_status("data", root = path)
##'
##' # Soon this will print more nicely to the screen, but for now you
##' # can see that the status of "data.rds" is "derived", which means
##' # that orderly knows that it is subject to being cleaned up; the
##' # "delete" element shows what will be deleted.
##'
##' # Do the actual deletion:
##' orderly2::orderly_cleanup("data", root = path)
##'
##' fs::dir_delete(path)
orderly_cleanup <- function(name = NULL, dry_run = FALSE, root = NULL) {
  status <- orderly_cleanup_status(name, root)
  n <- length(status$delete)
  if (n == 0) {
    cli::cli_alert_success("Nothing to clean")
  } else {
    if (dry_run) {
      cli::cli_alert_info("I would delete {n} file{?s} from '{status$name}':")
    } else {
      cli::cli_alert_info("Deleting {n} file{?s} from '{status$name}':")
    }
    cli::cli_li(status$delete)
    if (!dry_run) {
      withr::with_dir(status$path, fs::file_delete(status$delete))
      p <- delete_empty_directories(status$path)
      if (length(p)) {
        cli::cli_alert_info("Also deleted {length(p)} empty director{?y/ies}:")
        cli::cli_li(paste0(p, "/"))
      }
    }
  }
  invisible(status)
}


##' @export
##' @rdname orderly_cleanup
orderly_cleanup_status <- function(name = NULL, root = NULL) {
  p <- get_active_packet()
  is_active <- !is.null(p)
  if (is_active) {
    cli::cli_abort(
      "Don't call 'orderly2::orderly_cleanup_status()' from a running packet",
      i = "The orderly_cleanup* functions are for interactive use only")
  }

  if (is.null(name) && is.null(root)) {
    path <- getwd()
    root_path <- detect_orderly_interactive_path(path)
    name <- basename(path)
  } else {
    root_path <- orderly_src_root(root)
    if (is.null(name)) {
      ## This situation would be very odd, just disallow it
      cli::cli_abort("If 'root' is given explicitly, 'name' is required")
    }
    name <- validate_orderly_directory(name, root_path, environment())
    path <- file.path(root_path, "src", name)
  }

  info <- orderly_read(path, call = environment())
  files <- withr::with_dir(
    path,
    dir(all.files = TRUE, recursive = TRUE, no.. = TRUE))

  ## Slightly tricky construction here as we need to match all files
  ## that are present as directory entries; this is explicit only for
  ## dependencies, but we need to work it out ourselves for the rest.
  matches_path <- function(x, path, add_slash = TRUE) {
    if (is.null(path)) {
      return(rep(FALSE, length(x)))
    }
    path_dir <- if (add_slash) with_trailing_slash(path) else path
    x %in% path |
      row_any(vapply(path_dir, function(p) string_starts_with(p, x),
                     logical(length(x))))
  }
  nms_resource <- info$resources
  nms_artefact <- unlist(lapply(info$artefacts, "[[", "files"))
  nms_dependency <- unlist(lapply(info$dependency, function(x) names(x$files)))
  nms_shared_resource <- names(info$shared_resource)
  entrypoint_filename <- find_entrypoint_filename(path)

  role <- cbind(orderly = files == entrypoint_filename,
                resource = matches_path(files, nms_resource),
                shared_resource = matches_path(files, nms_shared_resource),
                dependency = matches_path(files, nms_dependency, FALSE),
                artefact = matches_path(files, nms_artefact))
  rownames(role) <- files

  v_source <- c("orderly", "resource")
  v_derived <- c("shared_resource", "dependency", "artefact")

  is_source <- row_any(role[, v_source, drop = FALSE]) | files == ".gitignore"
  is_derived <- !is_source & row_any(role[, v_derived, drop = FALSE])
  is_ignored <- path_is_git_ignored(file.path("src", name, files), path)
  status <- cbind(source = is_source,
                  derived = is_derived,
                  ignored = is_ignored)
  rownames(status) <- files

  to_delete <- (is_derived | (!is.na(is_ignored) & is_ignored)) & !is_source
  delete <- files[to_delete]

  unknown <- files[!is_source & !to_delete]

  structure(list(name = name,
                 root = root_path,
                 path = path,
                 role = role,
                 status = status,
                 delete = delete,
                 unknown = unknown),
            class = "orderly_cleanup_status")
}


##' @export
print.orderly_cleanup_status <- function(x, ...) {
  if (all(x$status[, "source"])) {
    cli::cli_alert_success(
      "{.pkg {x$name}} is clean, nothing to delete, nothing unknown")
  } else {
    cli::cli_alert_danger("{.pkg {x$name}} is not clean:")
    if (length(x$delete) > 0) {
      cli::cli_alert_info(
        paste("{length(x$delete)} file{?s} can be deleted by running",
              "'orderly2::orderly_cleanup({dquote(x$name)})':"))
      cli::cli_ul()
      cli::cli_li(x$delete)
      cli::cli_end()
    }
    if (length(x$unknown) > 0) {
      cli::cli_alert_info(
        "{length(x$unknown)} file{?s} {?has/have} unknown status:")
      cli::cli_ul()
      cli::cli_li(x$unknown)
      cli::cli_end()
      cli::cli_alert_info(paste(
        "Mark these as resources to indicate that these files should not be",
        "cleaned up."), wrap = TRUE)
      cli::cli_alert_info(paste(
        "Mark these as artefacts, dependencies, or list in .gitignore to",
        "indicate that these files can be automatically cleaned up"),
        wrap = TRUE)
    }
  }
  invisible(x)
}
