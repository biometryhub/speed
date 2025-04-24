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

#' Calculate Adjacency Score for Experimental Design
#'
#' @description
#' Calculates the number of adjacent treatments that are the same in an experimental
#' design layout. Lower scores indicate better separation of treatments.
#'
#' @param design A matrix containing the experimental design layout with treatments
#'
#' @return Numeric value representing the total number of adjacent same treatments.
#'        The score is the sum of same treatments that are adjacent horizontally
#'        and vertically.
#'
#' @examples
#' design <- matrix(c("A", "B", "A",
#'                    "B", "A", "B",
#'                    "A", "B", "A"),
#'                  nrow = 3, byrow = TRUE)
#' calculate_adjacency_score(design)  # Returns the number of adjacent matches
#'
#' @export
calculate_adjacency_score <- function(design) {
        row_adjacencies <- rowSums(design[, -ncol(design)] == design[, -1], na.rm = TRUE)
        col_adjacencies <- colSums(design[-nrow(design), ] == design[-1, ], na.rm = TRUE)
        sum(row_adjacencies) + sum(col_adjacencies)
}

#' Calculate Balance Score for Experimental Design
#'
#' @description
#' Calculates a balance score that measures how evenly treatments are distributed
#' across spatial factors in an experimental design. Lower scores indicate better balance.
#'
#' @param layout_df A data frame containing the experimental design layout
#' @param permute_var Character string specifying the name of the treatment variable
#' @param spatial_fac Character vector of spatial factors to consider (e.g., c("Row", "Col"))
#'
#' @return Numeric value representing the total balance score. Lower values indicate
#'         better balance of treatments across spatial factors.
#'
#' @examples
#' layout_df <- data.frame(
#'   Row = rep(1:3, each = 3),
#'   Col = rep(1:3, times = 3),
#'   Treatment = rep(LETTERS[1:3], 3)
#' )
#' calculate_balance_score(layout_df, "Treatment", c("Row", "Col"))
#'
#' @export
calculate_balance_score <- function(layout_df, permute_var, spatial_fac) {
    bscore <- sapply(spatial_fac, function(el)
        sum(apply(table(layout_df[[el]], layout_df[[permute_var]]), 1, var)))
    sum(bscore)
}

#' Calculate Combined Objective Score for Design Optimization
#'
#' @description
#' Internal function that calculates a combined score based on treatment adjacency
#' and spatial balance. Used by the optimization algorithms.
#'
#' @param design Matrix containing the experimental design layout
#' @param permute_var Character string naming the treatment variable to be permuted
#' @param layout_df Data frame containing the full design layout
#' @param spatial_fac Character vector of spatial factors to consider
#' @param adj_weight Weight for adjacency score (default: 1)
#' @param bal_weight Weight for balance score (default: 1)
#'
#' @return Numeric value representing the weighted sum of adjacency and balance scores
#'
#' @keywords internal
calculate_objective <- function(design, permute_var, layout_df, spatial_fac, adj_weight = 1, bal_weight = 1) {
    layout_df[[permute_var]] <- as.vector(design)
    adj <- calculate_adjacency_score(design)
    bal <- calculate_balance_score(layout_df, permute_var, spatial_fac)
    adj_weight * adj + bal_weight * bal
}
