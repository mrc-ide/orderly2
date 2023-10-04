orderly_graph_packets <- function(from = NULL, to = NULL,
                                  root = NULL, locate = FALSE) {
  root <- root_open(root, locate = locate, require_orderly = FALSE,
                    call = environment())
  if (is.null(from) == is.null(to)) {
    cli::cli_abort("Exactly one of 'from' and 'to' must be given")
  }

  metadata <- root$index$data()$metadata
  if (!is.null(from)) {
    if (is.null(metadata[[from]])) {
      cli::cli_abort("Packet '{from}' does not exist for 'from'")
    }
    dat <- graph_packets_from(from, metadata, call = environment())
  } else {
    if (is.null(metadata[[to]])) {
      cli::cli_abort("Packet '{to}' does not exist for 'to'")
    }
    dat <- graph_packets_to(to, metadata, call = environment())
  }
  dat
}


graph_packets_to <- function(to, metadata, call) {
  validate_outpack_id(to, call = call)
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


graph_packets_from <- function(from, metadata, call) {
  validate_outpack_id(from, call = call)
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
  if (length(edges) == 0) {
    data_frame(from = character(),
               to = character(),
               query = character(),
               files = I(list()))
  } else {
    data_frame(
      from = unlist(lapply(edges, "[[", "from"), FALSE, FALSE),
      to = unlist(lapply(edges, "[[", "to"), FALSE, FALSE),
      query = unlist(lapply(edges, "[[", "query"), FALSE, FALSE),
      files = I(unname(unlist(lapply(edges, "[[", "files"), FALSE))))
  }
}
