##' Set configuration options. Not all can currently be set; this will
##' be expanded over time. See Details.
##'
##' Options are set in the order that they are provided.  Currently,
##' if setting one option fails, no further options will be processed
##' but previous ones will be (do not rely on this behaviour, it
##' may change).
##'
##' Currently you can set:
##'
##' * `core.require_complete_tree`
##'
##' See [orderly2::outpack_init] for description of these options.
##'
##' @title Set configuration options
##'
##' @param ... Named options to set (e.g., pass the argument
##'   `core.require_complete_tree = TRUE`)
##'
##' @param options As an alternative to `...`, you can pass a list of
##'   named options here (e.g., `list(core.require_complete_tree =
##'   TRUE)`).  This interface is typically easier to program against.
##'
##' @inheritParams outpack_location_list
##'
##' @return Nothing
##' @export
outpack_config_set <- function(..., options = list(...), root = NULL) {
  root <- outpack_root_open(root, locate = TRUE)
  if (!missing(options) && ...length() > 0) {
    stop("If 'options' is given, no dot arguments are allowed")
  }
  if (length(options) == 0) {
    return(invisible())
  }

  assert_is(options, "list")
  assert_named(options)

  setters <- list(
    "core.require_complete_tree" = config_set_require_complete_tree,
    "core.use_file_store" = config_set_use_file_store,
    "core.path_archive" = config_set_path_archive,
    "logging.console" = config_set_logging_console,
    "logging.threshold" = config_set_logging_threshold)

  unknown <- setdiff(names(options), names(setters))
  if (length(unknown)) {
    stop("Can't set configuration option: ",
         paste(squote(unknown), collapse = ", "))
  }

  for (nm in names(options)) {
    root <- setters[[nm]](options[[nm]], root)
  }

  if (any(grepl("^logging\\.", names(options)))) {
    root$logger <- root$config$logging
  }

  invisible()
}


config_set_require_complete_tree <- function(value, root) {
  config <- root$config

  if (config$core$require_complete_tree == value) {
    message("'core.require_complete_tree' was unchanged")
    return()
  }

  if (value) {
    id <- root$index()$unpacked
    outpack_location_pull_packet(id, recursive = TRUE, root = root)
  }

  config$core$require_complete_tree <- value
  config_update(config, root)
}


config_set_use_file_store <- function(value, root) {
  assert_scalar_logical(value)
  config <- root$config

  if (config$core$use_file_store == value) {
    message("'core.use_file_store' was unchanged")
    return()
  }

  if (!value) {
    if (is.null(config$core$path_archive)) {
      stop("If 'path_archive' is NULL, then 'use_file_store' must be TRUE")
    }
    root$remove_file_store()
  }

  if (value) {
    tryCatch(
      root$add_file_store(),
      error = function(e) {
        root$remove_file_store()
        stop("Error adding file store: ", e$message, call. = FALSE)
      })
  }

  config$core$use_file_store <- value
  config_update(config, root)
}


config_set_path_archive <- function(value, root) {
  config <- root$config

  if (identical(value, config$core$path_archive)) {
    message("'core.path_archive' was unchanged")
    return()
  }

  if (is.null(value)) {
    if (!config$core$use_file_store) {
      stop("If 'path_archive' is NULL, then 'use_file_store' must be TRUE")
    }
    path_archive <- file.path(root$path, config$core$path_archive)
    if (fs::dir_exists(path_archive)) {
      fs::dir_delete(path_archive)
    }
    config$core["path_archive"] <- list(NULL)
  } else if (!is.null(config$core$path_archive)) {
    path_archive_old <- file.path(root$path, config$core$path_archive)
    if (fs::dir_exists(path_archive_old)) {
      path_archive_new <- file.path(root$path, value)
      assert_relative_path(value, name = "path_archive")
      assert_directory_does_not_exist(path_archive_new)
      fs::dir_copy(path_archive_old, path_archive_new)
      fs::dir_delete(path_archive_old)
    }
    config$core$path_archive <- value
  } else {
    path_archive <- file.path(root$path, value)
    assert_relative_path(value, name = "path_archive")
    assert_directory_does_not_exist(path_archive)
    tryCatch({
      fs::dir_create(path_archive)
      invisible(lapply(root$index()$unpacked, function(id) {
        meta <- root$metadata(id)
        dst <- file.path(path_archive, meta$name, id, meta$files$path)
        root$files$get(meta$files$hash, dst)
      }))
      config$core$path_archive <- value
    }, error = function(e) {
      path_archive <- file.path(root$path, value)
      if (fs::dir_exists(path_archive)) {
        fs::dir_delete(path_archive)
      }
      stop("Error adding 'path_archive': ", e$message, call. = FALSE)
    })
  }

  config_update(config, root)
}


config_set_logging_threshold <- function(value, root) {
  config <- root$config
  config$logging$threshold <- log_level_check(value, "logging.threshold")
  config_update(config, root)
}


config_set_logging_console <- function(value, root) {
  assert_scalar_logical(value)
  config <- root$config
  config$logging$console <- value
  config_update(config, root)
}


config_new <- function(path_archive, use_file_store, require_complete_tree,
                       logging_console, logging_threshold) {
  if (!is.null(path_archive)) {
    assert_scalar_character(path_archive)
  }
  assert_scalar_logical(use_file_store)
  if (is.null(path_archive) && !use_file_store) {
    stop("If 'path_archive' is NULL, then 'use_file_store' must be TRUE")
  }

  assert_scalar_logical(require_complete_tree)

  assert_scalar_logical(logging_console)
  logging_threshold <- log_level_check(logging_threshold)

  ## TODO: There's a good reason here to wonder if this _should_ be
  ## configurable.  I'll keep it here within the configuration even
  ## though it can't be changed really.
  hash_algorithm <- "sha256"

  list(
    schema_version = outpack_schema_version(),
    core = list(
      path_archive = path_archive,
      use_file_store = use_file_store,
      require_complete_tree = require_complete_tree,
      hash_algorithm = hash_algorithm),
    logging = list(
      console = logging_console,
      threshold = logging_threshold),
    location = new_location_entry(local, 0, "local", NULL))
}


config_serialise <- function(config, path) {
  config$schema_version <- scalar(config$schema_version)
  config$core <- lapply(config$core, scalar)
  config$logging <- lapply(config$logging, scalar)

  prepare_location <- function(loc) {
    c(lapply(loc[setdiff(names(loc), "args")], scalar),
      list(args = lapply(loc$args[[1]], scalar)))
  }
  config$location <- lapply(seq_len(nrow(config$location)), function(i) {
    prepare_location(config$location[i, ])
  })

  to_json(config, "config")
}


config_update <- function(config, root) {
  root$config <- config
  config_write(config, root$path)
  root
}


config_write <- function(config, root_path) {
  writeLines(config_serialise(config),
             file.path(root_path, ".outpack", "config.json"))
}


config_read <- function(root_path) {
  config <- jsonlite::read_json(file.path(root_path, ".outpack/config.json"))
  ## NOTE: make sure that this matches the order in new_location_entry
  config$location <- data_frame(
    name = vcapply(config$location, "[[", "name"),
    id = vcapply(config$location, "[[", "id"),
    priority = vnapply(config$location, "[[", "priority"),
    type = vcapply(config$location, "[[", "type"),
    args = I(lapply(config$location, "[[", "args")))
  if (is.null(config$logging)) {
    ## Logging is unspecified in config, so use implementation defined
    ## defaults:
    config$logging <- list(console = TRUE, threshold = "info")
  }
  config
}
