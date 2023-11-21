outpack_index <- R6::R6Class(
  "outpack_index",
  cloneable = FALSE,

  private = list(
    path_ = NULL,
    data_ = list()
  ),

  public = list(
    initialize = function(path) {
      private$path_ <- path
    },

    rebuild = function() {
      private$data_ <- index_update(private$path_, NULL, TRUE)
      invisible(self)
    },

    refresh = function() {
      private$data_ <- index_update(private$path_, private$data_, FALSE)
      invisible(self)
    },

    metadata = function(id, call = NULL) {
      ret <- private$data_$metadata[[id]]
      if (is.null(ret)) {
        self$refresh()
        ret <- private$data_$metadata[[id]]
      }
      if (is.null(ret)) {
        cli::cli_abort("Packet '{id}' not found in outpack index", call = call)
      }
      ret
    },

    location = function(name) {
      self$refresh()
      if (is.null(name)) {
        private$data_$location
      } else {
        private$data_$location[private$data_$location$location %in% name, ]
      }
    },

    unpacked = function() {
      self$refresh()
      private$data_$unpacked
    },

    data = function() {
      self$refresh()
      private$data_
    }
  ))


index_update <- function(root_path, prev, skip_cache) {
  path_index <- file.path(root_path, ".outpack", "index", "outpack.rds")

  if (length(prev) == 0 && file.exists(path_index) && !skip_cache) {
    prev <- readRDS(path_index)
  }

  data <- prev
  data$location <- read_locations(root_path, data$location)
  data$metadata <- read_metadata(root_path, data$metadata)
  data$unpacked <- data$location$packet[data$location$location == local]

  if (!identical(data, prev)) {
    fs::dir_create(dirname(path_index))
    saverds_atomic(data, path_index)
  }

  data
}


read_metadata <- function(root_path, prev) {
  path <- file.path(root_path, ".outpack", "metadata")
  id_new <- setdiff(dir(path), names(prev))

  if (length(id_new) == 0) {
    return(prev)
  }

  files <- file.path(path, id_new)
  new <- lapply(files, outpack_metadata_core_read)
  names(new) <- id_new
  ret <- c(prev, new)
  ret[order(names(ret))]
  ret
}


read_locations <- function(root_path, prev) {
  if (is.null(prev)) {
    prev <- data_frame(packet = character(),
                       time = empty_time(),
                       hash = character(),
                       location = character())
  }

  location_path <- fs::dir_ls(file.path(root_path, ".outpack", "location"),
                              type = "directory")
  location_name <- basename(location_path)
  new <- do.call(rbind, lapply(location_name, read_location, root_path, prev))
  ret <- rbind(prev, new)
  ## Always sort by location, then id
  ret <- ret[order(match(ret$location, location_name), ret$packet), ]
  ## Avoids weird computed rownames - always uses 1:n
  rownames(ret) <- NULL
  ret
}


read_location <- function(location_name, root_path, prev) {
  path <- file.path(root_path, ".outpack", "location", location_name)
  packets <- dir(path, re_id)
  is_new <- !(packets %in% prev$packet[prev$location == location_name])
  if (!any(is_new)) {
    return(NULL)
  }

  dat <- lapply(file.path(path, packets[is_new]), jsonlite::read_json)
  data_frame(packet = vcapply(dat, "[[", "packet"),
             time = num_to_time(vnapply(dat, "[[", "time")),
             hash = vcapply(dat, "[[", "hash"),
             location = location_name)
}
