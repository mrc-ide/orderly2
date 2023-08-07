orderly_graph_packets <- function(from = NULL, to = NULL,
                                  root = NULL, locate = FALSE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())
  if (is.null(from) == is.null(to)) {
    cli::cli_abort("Exactly one of 'from' and 'to' must be given")
  }

  metadata <- root$index$data()$metadata
  if (!is.null(from)) {
    dat <- graph_packets_from(from, metadata)
  } else {
    dat <- graph_packets_to(to, metadata)
  }

  dat
}


graph_packets_to <- function(to, metadata) {
  validate_outpack_id(to)
  packets <- to
  edges <- list()
  i <- 0L
  while (i < length(packets)) {
    i <- i + 1L
    p <- packets[[i]]
    depends <- metadata[[p]]$depends
    if (nrow(depends) > 0) {
      depends$from <- depends$packet
      depends$to <- p
      edges[[p]] <- depends
      packets <- union(packets, depends$packet)
    }
  }
  edges <- graph_edges_to_df(edges)
  list(from = NULL, to = to, packets = sort(packets), edges = edges)
}


graph_packets_from <- function(from, metadata) {
  validate_outpack_id(from)
  uses <- build_packet_uses(lapply(metadata, "[[", "depends"))
  packets <- from
  edges <- list()
  i <- 0L
  while (i < length(packets)) {
    i <- i + 1L
    p <- packets[[i]]
    p_used_by <- uses[[p]]$packet
    if (length(p_used_by) > 0) {
      for (j in p_used_by) {
        depends <- metadata[[j]]$depends
        depends <- depends[depends$packet == p, ]
        depends$from <- p
        depends$to <- j
        edges[[length(edges) + 1]] <- depends
      }
      packets <- union(packets, p_used_by)
    }
  }
  edges <- graph_edges_to_df(edges)
  list(from = from, to = NULL, packets = sort(packets), edges = edges)
}


graph_edges_to_df <- function(edges) {
  data_frame(
    from = unlist_character(lapply(edges, "[[", "from")),
    to = unlist_character(lapply(edges, "[[", "to")),
    query = unlist_character(lapply(edges, "[[", "query")),
    files = I(unname(lapply(edges, "[[", "files"))))
}
