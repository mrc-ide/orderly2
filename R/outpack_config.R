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
##' See [orderly2::orderly_init] for description of these options.
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
##' @inheritParams orderly_metadata
##'
##' @return Nothing
##' @seealso orderly_config
##' @export
##' @examples
##'
##' # The default configuration does not include a file store, and
##' # saves output within the "archive" directory:
##' path <- withr::local_tempdir()
##' orderly2::orderly_init(path)
##' fs::dir_tree(path, all = TRUE)
##'
##' # Change this after the fact:
##' orderly2::orderly_config_set(core.use_file_store = TRUE,
##'                              core.path_archive = NULL,
##'                              root = path)
##' fs::dir_tree(path, all = TRUE)
orderly_config_set <- function(..., options = list(...), root = NULL,
                               locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())
  if (!missing(options) && ...length() > 0) {
    stop("If 'options' is given, no dot arguments are allowed")
  }
  if (length(options) == 0) {
    return(invisible())
  }

  assert_is(options, "list", call = environment())
  assert_named(options, call = environment())

  setters <- list(
    "core.require_complete_tree" = config_set_require_complete_tree,
    "core.use_file_store" = config_set_use_file_store,
    "core.path_archive" = config_set_path_archive)

  unknown <- setdiff(names(options), names(setters))
  if (length(unknown)) {
    stop("Can't set configuration option: ",
         paste(squote(unknown), collapse = ", "))
  }

  for (nm in names(options)) {
    root <- setters[[nm]](options[[nm]], root, call = environment())
  }

  invisible()
}


##' Read the current orderly configuration, stored within the outpack
##' root, along with any orderly-specific extensions.
##'
##' @title Read configuration
##'
##' @inheritParams orderly_metadata
##'
##' @return A list of configuration options:
##'
##' * `core`: The most important options about the outpack store, containing:
##'   - `path_archive`: The path to the human-readable packet archive,
##'     or `NULL` if disabled (set in [orderly2::orderly_config_set] as
##'     `core.path_archive`)
##'   - `use_file_store`: Indicates if a content-addressable file store
##'     is enabled (`core.use_file_store`)
##'   - `require_complete_tree`: Indicates if this outpack store requires
##'     all dependencies to be fully available (`core.require_complete_tree`)
##'   - `hash_algorithm`: The hash algorithm used (currently not modifiable)
##'
##' * `location`: Information about locations; see
##'   [orderly2::orderly_location_add],
##'   [orderly2::orderly_location_rename] and
##'   [orderly2::orderly_location_remove] to interact with this
##'   configuration, or [orderly2::orderly_location_list] to more
##'   simply list available locations. Returns as a [data.frame] with
##'   columns `name`, `id`, `priority`, `type` and `args`, with `args`
##'   being a list column.
##'
##' * `orderly`: A list of orderly-specific configuration; this is
##'   just the minimum required version (as
##'   `minimum_orderly_version`).
##'
##' @export
##' @examples
##'
##' # A default configuration in a new temporary directory
##' path <- withr::local_tempdir()
##' orderly2::orderly_init(path)
##' orderly2::orderly_config(path)
orderly_config <- function(root = NULL, locate = TRUE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())
  root$config
}


config_set_require_complete_tree <- function(value, root, call) {
  config <- root$config

  if (config$core$require_complete_tree == value) {
    message("'core.require_complete_tree' was unchanged")
    return()
  }

  if (value) {
    id <- root$index$unpacked()
    if (length(id) > 0) {
      orderly_location_pull_packet(id, recursive = TRUE, root = root)
    }
  }

  config$core$require_complete_tree <- value
  config_update(config, root)
}


config_set_use_file_store <- function(value, root, call) {
  assert_scalar_logical(value, call = call)
  config <- root$config

  if (config$core$use_file_store == value) {
    message("'core.use_file_store' was unchanged")
    return()
  }

  if (!value) {
    if (is.null(config$core$path_archive)) {
      stop("If 'path_archive' is NULL, then 'use_file_store' must be TRUE")
    }
    remove_file_store(root)
  }

  if (value) {
    tryCatch(
      add_file_store(root),
      error = function(e) {
        remove_file_store(root)
        stop("Error adding file store: ", e$message, call. = FALSE)
      })
  }

  config$core$use_file_store <- value
  config_update(config, root)
}


config_set_path_archive <- function(value, root, call) {
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
      assert_relative_path(value, name = "'path_archive'", workdir = root$path,
                           call = call)
      assert_directory_does_not_exist(path_archive_new, call = call)
      fs::dir_copy(path_archive_old, path_archive_new)
      fs::dir_delete(path_archive_old)
    }
    config$core$path_archive <- value
  } else {
    path_archive <- file.path(root$path, value)
    assert_relative_path(value, name = "'path_archive'", workdir = root$path,
                         call = call)
    assert_directory_does_not_exist(path_archive, call = call)
    tryCatch({
      fs::dir_create(path_archive)
      invisible(lapply(root$index$unpacked(), function(id) {
        meta <- outpack_metadata_core(id, root)
        dst <- file.path(path_archive, meta$name, id, meta$files$path)
        root$files$get(meta$files$hash, dst, TRUE)
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


config_new <- function(path_archive, use_file_store, require_complete_tree,
                       call = NULL) {
  assert_scalar_character(path_archive, allow_null = TRUE, call = call)
  assert_scalar_logical(use_file_store, call = call)
  if (is.null(path_archive) && !use_file_store) {
    stop("If 'path_archive' is NULL, then 'use_file_store' must be TRUE")
  }

  assert_scalar_logical(require_complete_tree, call = call)

  ## TODO: There's a good reason here to wonder if this _should_ be
  ## configurable.  I'll keep it here within the configuration even
  ## though it can't be changed really.
  hash_algorithm <- "sha256"

  list(
    core = list(
      path_archive = path_archive,
      use_file_store = use_file_store,
      require_complete_tree = require_complete_tree,
      hash_algorithm = hash_algorithm),
    location = new_location_entry(local, "local", NULL))
}


config_serialise <- function(config, path) {
  config$schema_version <- scalar(config$schema_version)
  config$core <- lapply(config$core, scalar)

  prepare_location <- function(loc) {
    args <- loc$args[[1]]
    if (length(args) == 0) {
      args <- set_names(list(), character())
    } else {
      args <- lapply(args, scalar)
    }
    c(lapply(loc[setdiff(names(loc), "args")], scalar),
      list(args = args))
  }
  config$location <- lapply(seq_len(nrow(config$location)), function(i) {
    prepare_location(config$location[i, ])
  })

  config$orderly <- NULL

  to_json(config, "outpack/config.json")
}


config_update <- function(config, root) {
  config_write(config, root$path)
  root$config <- config
  root
}


config_write <- function(config, root_path) {
  writeLines(config_serialise(config),
             file.path(root_path, ".outpack", "config.json"))
}


config_read <- function(root_path) {
  config <- read_json(file.path(root_path, ".outpack/config.json"))
  ## NOTE: make sure that this matches the order in new_location_entry
  config$location <- data_frame(
    name = vcapply(config$location, "[[", "name"),
    type = vcapply(config$location, "[[", "type"),
    args = I(lapply(config$location, "[[", "args")))
  config
}
