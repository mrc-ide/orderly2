#' @title Query index
#'
#' @description
#' Class for managing the active index whilst evaluating a query
#' @keywords internal
query_index <- R6::R6Class(
  "query_index",
  cloneable = FALSE,

  public = list(
    #' @field index The packet index
    index = NULL,
    #' @field depends Named list of data frames. Names are packet ids, values
    #'   are packets depended on by this packet id (i.e. its parents).
    depends = NULL,
    #' @field uses Named list of data frames. Names are packet ids, values
    #'   are packets which are used by this packet id (i.e. its children).
    uses = NULL,
    #' @field root The outpack root object
    root = NULL,

    #' @description
    #' Create a new query_index object
    #'
    #' @param root The outpack root object
    #' @param index The packet index as a data frame
    #' @param depends Named list of data frames. Names are packet ids, values
    #'   are packets depended on by this packet id (i.e. its parents).
    #' @param uses Named list of data frames. Names are packet ids, values
    #'   are packets used by on by this packet id (i.e. its children). This is
    #'   the same data as `depends` but relationships flow in the other
    #'   direction.
    initialize = function(root, index, depends, uses) {
      self$root <- root
      self$index <- index
      self$depends <- depends
      self$uses <- uses
      lockBinding("root", self)
      lockBinding("depends", self)
      lockBinding("uses", self)
    },

    #' @description
    #' Get the ids of packets which this packet depends to a specified level
    #'
    #' @param id The id of the packet to get parents of
    #' @param depth Depth of parents to get, `depth` 1 gets immediate parents
    #' `depth` 2 gets parents and parents of parents, `depth` Inf will
    #' recurse the whole tree to get all parents
    #' @return The ids of the parents of this packet
    get_packet_depends = function(id, depth) {
      deps <- private$get_dependencies(id, depth, self$depends)
      intersect(deps, self$index$id) %||% character(0)
    },

    #' @description
    #' Get the ids of packets which are used by this packet
    #'
    #' @param id The id of the packet to get children of
    #' @param depth Depth of children to get, `depth` 1 gets immediate children
    #' `depth` 2 gets children and children of children, `depth` Inf will
    #' recurse the whole tree to get all children
    #' @return The ids of the children of this packet
    get_packet_uses = function(id, depth) {
      deps <- private$get_dependencies(id, depth, self$uses)
      intersect(deps, self$index$id) %||% character(0)
    }
  ),

  private = list(
    get_dependencies = function(id, depth, dependency_data) {
      if (depth <= 0) {
        return(character(0))
      }
      deps <- dependency_data[[id]]$packet
      unique(c(deps, unlist(lapply(deps, private$get_dependencies,
                                   depth - 1, dependency_data))))
    }
  )
)


## It would be interesting to know if we can cache this; that might
## help with the pulling metadata issue (as we could then control only
## pulling once in a session).
new_query_index <- function(root, options) {
  root <- root_open(root, locate = FALSE, require_orderly = FALSE)

  if (options$pull_metadata) {
    outpack_location_pull_metadata(options$location, root)
  }
  idx <- root$index()
  metadata <- idx$metadata

  if (!is.null(options$location)) {
    location_id <- location_resolve_valid(options$location, root,
                                          include_local = TRUE,
                                          allow_no_locations = FALSE)
    include <- idx$location$packet[idx$location$location %in% location_id]
    metadata <- metadata[names(metadata) %in% include]
  }
  if (!options$allow_remote) {
    metadata <- metadata[names(metadata) %in% idx$unpacked]
  }

  index <- data_frame(
    id = names(metadata) %||% character(0),
    name = vcapply(metadata, "[[", "name"),
    ## Wrap this in I() because it is a list column
    parameters = I(lapply(metadata, "[[", "parameters")))

  depends <- lapply(metadata, "[[", "depends")
  uses <- build_packet_uses(depends)

  query_index$new(root, index, depends, uses)
}


build_packet_uses <- function(dependencies) {
  ids <- names(dependencies)
  uses <- list()
  for (id in ids) {
    for (packet in dependencies[[id]]$packet) {
      if (is.null(uses[[packet]])) {
        uses[[packet]] <- list(packet = id)
      } else {
        uses[[packet]]$packet <- unique(c(uses[[packet]]$packet, id))
      }
    }
  }
  uses
}
