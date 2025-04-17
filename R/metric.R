# calculate number of pairs with same neighbor
# diagonal pairs not included
calculate_nb <- function(design_matrix) {
  n_rows <- dim(design_matrix)[1]
  n_cols <- dim(design_matrix)[2]
  # TODO: check whether to use env or list
  nb <- new.env()

  # vectorize?
  for (row_ in 1:n_rows) {
    for (col_ in 1:n_cols) {
      node <- design_matrix[row_, col_]
      if (row_ < n_rows) {
        bottom <- design_matrix[row_ + 1, col_]
        if (node < bottom) {
          pair_str <- paste0(node, ",", bottom)
        } else {
          pair_str <- paste0(bottom, ",", node)
        }

        env_add_one(nb, pair_str)
      }

      if (col_ < n_cols) {
        right <- design_matrix[row_, col_ + 1]
        if (node < right) {
          pair_str <- paste0(node, ",", right)
        } else {
          pair_str <- paste0(right, ",", node)
        }

        env_add_one(nb, pair_str)
      }
    }
  }

  nb <- as.list(nb)
  max_nb <- max(unlist(nb))
  max_pairs <- names(nb[nb == max_nb])
  return(list(
    nb = nb,
    max_nb = max_nb,
    max_pairs = max_pairs
  ))
}

# calculate even distribution for 3 reps
calculate_ed_3_reps <- function(edges) {
  # pick 2 shortest connections for 3 reps
  msts <- lapply(edges, function(x) {
    weights <- unlist(lapply(x, tail, 1))
    return(sum(weights) - max(weights))
  })

  min_mst <- min(unlist(msts))
  min_treatments <- names(msts[msts == min_mst])
  return(list(
    msts = msts,
    min_mst = min_mst,
    min_treatments = min_treatments
  ))
}

# calculate even distribution for >3 reps
calculate_ed <- function(design_matrix) {
  vertices <- get_vertices(design_matrix)
  msts <- list()
  sub_graph <- list()

  for (treatment in names(vertices)) {
    reps <- as.character(length(vertices[[treatment]]))
    if (is.null(sub_graph[[reps]])) {
      # initialize a fully-connected graph without weights
      # 1--2, 1--3, ..., 1--n-1, 1--n, 2--3, 2--4, ..., n-1--n
      sub_graph[[reps]] <- igraph::graph_from_edgelist(
        t(combn(seq_along(vertices[[treatment]]), 2)),
        directed = FALSE
      )
    }

    weights <- c()
    # order matters here
    for (i in 1:(length(vertices[[treatment]]) - 1)) {
      for (j in (i + 1):length(vertices[[treatment]])) {
        weights <- c(weights, sum((vertices[[treatment]][[i]] - vertices[[treatment]][[j]])^2))
      }
    }

    igraph::E(sub_graph[[reps]])$weight <- weights
    msts[[treatment]] <- sum(sqrt(igraph::E(igraph::mst(sub_graph[[reps]]))$weight))
  }

  min_mst <- min(unlist(msts))
  min_treatments <- names(msts[msts == min_mst])
  return(list(
    msts = msts,
    min_mst = min_mst,
    min_treatments = min_treatments
  ))
}

get_vertices <- function(design_matrix) {
  vertices <- list()
  for (i in seq_len(nrow(design_matrix))) {
    for (j in seq_len(ncol(design_matrix))) {
      treatment <- design_matrix[i, j]
      design_matrix[i, j] <- treatment

      if (is.null(vertices[[treatment]])) {
        vertices[[treatment]] <- list()
      }

      vertices[[treatment]][[length(vertices[[treatment]]) + 1]] <- c(i, j)
    }
  }

  return(vertices)
}
