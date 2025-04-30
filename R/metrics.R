#' Default Objective Function
#'
#' @description
#' A default objective function that combines adjacency and balance scores.
#'
#' @param adj_weight Weight for adjacency score (default: 1)
#' @param bal_weight Weight for balance score (default: 1)
#'
#' @examples
#' design_matrix <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
#' layout_df <- data.frame(
#'   row = rep(1:3, each = 3),
#'   col = rep(1:3, times = 3)
#' )
#' objective_function()(design_matrix, layout_df, "treatment", c("row", "col"))
#'
#' # create an objective function including even distribution and neighbor
#' # balance introduced by Piepho 2018
#' objective_function_piepho <- function(design_matrix, layout_df, swap, spatial_cols) {
#'   ed <- calculate_ed(design_matrix)
#'   ed_score <- -sum(unlist(lapply(ed, function(ed_rep) ed_rep$min_mst)))
#'   nb_score <- calculate_nb(design_matrix)$max_nb
#'
#'   adj_bal_score <- objective_function()(design_matrix, layout_df, swap, spatial_cols)
#'
#'   return(nb_score + ed_score + adj_bal_score)
#' }
#' objective_function_piepho(design_matrix, layout_df, "treatment", c("row", "col"))
#' # usage in speed, speed(..., obj_function = objective_function_piepho)
#'
#' @return A function which returns numeric value representing the score of the design (lower is better) with a
#'   signature \code{function(design_matrix, layout_df, swap, spatial_cols)}. See signature details in
#'   \link{objective_function_signature}.
#'
#' @export
objective_function <- function(
    adj_weight = getOption("speed.adj_weight", 1),
    bal_weight = getOption("speed.bal_weight", 1)) {
  return(
    function(design_matrix, layout_df, swap, spatial_cols) {
      if (adj_weight != 0) {
        adj <- calculate_adjacency_score(design_matrix)

        if (bal_weight == 0) {
          return(adj)
        }
      }

      if (bal_weight != 0) {
        layout_df[[swap]] <- as.vector(design_matrix)
        bal <- calculate_balance_score(layout_df, swap, spatial_cols)

        if (adj_weight == 0) {
          return(bal)
        }
      }

      return(adj_weight * adj + bal_weight * bal)
    }
  )
}

#' Neighbor Balance Calculation
#'
#' @description
#' A metric that counts the occurrence of the same adjacent pairs. Only horizontal and vertical pairs are
#'   counted.
#'
#' @inheritParams objective_function_signature
#'
#' @examples
#' design_matrix <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
#' calculate_nb(design_matrix)
#'
#' @return Named list containing:
#' \itemize{
#'   \item nb - Named list of pairs of items and their number of occurrence
#'   \item max_nb - The highest number of occurrence
#'   \item max_pairs - Vector of pairs of items with the highest number of occurrence
#' }
#'
#' @export
calculate_nb <- function(design_matrix) {
  n_rows <- dim(design_matrix)[1]
  n_cols <- dim(design_matrix)[2]
  # TODO: check whether to use env or list
  nb <- new.env()

  # TODO: check for vectorization
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

#' Even Distribution Calculation
#'
#' @description
#' A metric that represents the even distribution of each item with their minimum spanning tree (mst).
#'
#' @inheritParams objective_function_signature
#'
#' @examples
#' design_matrix <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
#' calculate_ed(design_matrix)
#'
#' @return Named list containing:
#' \itemize{
#'   \item <number of replications> - Named list containing:
#'     \itemize{
#'       \item msts - Named list of pairs of items and their mst
#'       \item min_mst - The lowest mst
#'       \item min_pairs - Pairs of items with the lowest mst
#'     }
#' }
#'
#' @export
calculate_ed <- function(design_matrix) {
  vertices <- get_vertices(design_matrix)
  vertices_3_reps <- list()
  msts <- list()
  sub_graph <- list()

  for (item in names(vertices)) {
    reps <- length(vertices[[item]])
    reps_char <- as.character(reps)
    if (reps == 2) {
      # distance between 2 nodes for 2 reps
      msts[[reps_char]][[item]] <- sqrt(sum((vertices[[item]][[1]] - vertices[[item]][[2]])^2))
    } else if (reps == 3) {
      # 3 reps will be calculated with .calculate_ed_3_reps
      vertices_3_reps[[item]] <- vertices[[item]]
    } else if (reps > 3) {
      # blanket igraph for 4+ reps
      if (is.null(sub_graph[[reps_char]])) {
        # initialize a fully-connected graph without weights
        # 1--2, 1--3, ..., 1--n-1, 1--n, 2--3, 2--4, ..., n-1--n
        edge_table <- t(combn(1:reps, 2))
        sub_graph[[reps_char]] <- igraph::graph_from_edgelist(edge_table, directed = FALSE)
        msts[[reps_char]] <- list()
      }

      weights <- c()
      # order matters here
      for (i in 1:(length(vertices[[item]]) - 1)) {
        for (j in (i + 1):length(vertices[[item]])) {
          weights <- c(weights, sum((vertices[[item]][[i]] - vertices[[item]][[j]])^2))
        }
      }

      igraph::E(sub_graph[[reps_char]])$weight <- weights
      msts[[reps_char]][[item]] <- sum(sqrt(igraph::E(igraph::mst(sub_graph[[reps_char]]))$weight))
    }
  }

  # summarize mst for each reps
  msts <- lapply(msts, function(msts_by_reps) {
    min_mst <- min(unlist(msts_by_reps))
    min_pairs <- names(msts_by_reps[msts_by_reps == min_mst])

    return(list(
      msts = msts_by_reps,
      min_mst = min_mst,
      min_pairs = min_pairs
    ))
  })

  if (length(vertices_3_reps) > 0) {
    msts_3_reps <- .calculate_ed_3_reps(get_edges(vertices_3_reps))
    msts <- c(msts, list("3" = msts_3_reps))
  }

  return(msts)
}

#' Get Weighted Edges
#'
#' @description
#' Calculate the weight of edges from vertices.
#'
#' @inheritParams objective_function_signature
#'
#' @examples
#' design_matrix <- matrix(c(1, 2, 2, 1, 3, 2, 1, 3, 3), nrow = 3, ncol = 3)
#' vertices <- get_vertices(design_matrix)
#'
#' @return Named list containing:
#'   \itemize{
#'     \item <item> - A list of (vertex 1, vertex 2, ...)
#'   }
#'
#' @seealso [get_edges()]
#'
#' @export
get_vertices <- function(design_matrix) {
  # Create vectors of row, col indices and corresponding values
  rows <- row(design_matrix)
  cols <- col(design_matrix)
  items <- as.character(design_matrix)

  # Combine row and col into coordinates
  coords <- Map(c, as.vector(rows), as.vector(cols))

  # Use split to group coordinates by item
  return(split(coords, items))
}

#' Get Weighted Edges
#'
#' @description
#' Calculate the weight of edges from vertices.
#'
#' @param vertices Named list of vertices containing:
#'   \itemize{
#'     \item <item> - A list of (vertex 1, vertex 2, ...)
#'   }
#'
#' @examples
#' design_matrix <- matrix(c(1, 2, 2, 1, 3, 2, 1, 3, 3), nrow = 3, ncol = 3)
#' vertices <- get_vertices(design_matrix)
#' edges <- get_edges(vertices)
#'
#' @return Named list containing:
#'   \itemize{
#'     \item <item> - A list of (vertex 1, vertex 2, weight)
#'   }
#'
#' @seealso [get_vertices()]
#'
#' @export
get_edges <- function(vertices) {
  edges <- vector("list", length(vertices))
  names(edges) <- names(vertices)

  for (item in names(vertices)) {
    coords <- vertices[[item]]
    n_vertices <- length(coords)
    if (n_vertices < 2) {
      edges[[item]] <- list()
      next
    }

    # Preallocate list to hold all edges
    edge_list <- vector("list", n_vertices * (n_vertices - 1) / 2)
    idx <- 1

    for (i in 1:(n_vertices - 1)) {
      for (j in (i + 1):n_vertices) {
        edge_list[[idx]] <- c(i, j, sqrt(sum((coords[[i]] - coords[[j]])^2)))
        idx <- idx + 1
      }
    }

    edges[[item]] <- edge_list
  }

  return(edges)
}

#' Even Distribution Calculation for 3 Replications
#'
#' @description
#' A metric that represents the even distribution of items with 3 replications with their minimum spanning tree
#'   (mst).
#'
#' @param edges A list of lists of edges
#'
#' @return Named list containing:
#' \itemize{
#'   \item msts - Named list of pairs of items and their mst
#'   \item min_mst - The lowest mst
#'   \item min_pairs - Pairs of items with the lowest mst
#' }
#'
#' @seealso [get_edges()]
#'
#' @keywords internal
.calculate_ed_3_reps <- function(edges) {
  # pick 2 shortest connections for 3 reps
  msts <- lapply(edges, function(item_edges) {
    weights <- unlist(lapply(item_edges, function(edge) edge[[3]]))
    return(sum(weights) - max(weights))
  })

  min_mst <- min(unlist(msts))
  min_pairs <- names(msts[msts == min_mst])
  return(list(
    msts = msts,
    min_mst = min_mst,
    min_pairs = min_pairs
  ))
}

#' Calculate Adjacency Score for Experimental Design
#'
#' @description
#' Calculates the number of adjacent treatments that are the same in an experimental
#'   design layout. Lower scores indicate better separation of treatments.
#'
#' @param design A matrix containing the experimental design layout with treatments
#'
#' @return Numeric value representing the total number of adjacent same treatments.
#'   The score is the sum of same treatments that are adjacent horizontally
#'   and vertically.
#'
#' @examples
#' design <- matrix(
#'   c(
#'     "A", "B", "A",
#'     "B", "A", "B",
#'     "A", "B", "A"
#'   ),
#'   nrow = 3, byrow = TRUE
#' )
#' calculate_adjacency_score(design) # Returns the number of adjacent matches
#'
#' @export
calculate_adjacency_score <- function(design) {
  # row_adjacencies <- rowSums(design[, -ncol(design)] == design[, -1], na.rm = TRUE)
  # col_adjacencies <- colSums(design[-nrow(design), ] == design[-1, ], na.rm = TRUE)
  row_adjacencies <- sum(design[, -ncol(design)] == design[, -1], na.rm = TRUE)
  col_adjacencies <- sum(design[-nrow(design), ] == design[-1, ], na.rm = TRUE)
  return(row_adjacencies + col_adjacencies)
}

#' Calculate Balance Score for Experimental Design
#'
#' @description
#' Calculates a balance score that measures how evenly treatments are distributed
#'   across spatial factors in an experimental design. Lower scores indicate better balance.
#'
#' @inheritParams objective_function_signature
#'
#' @return Numeric value representing the total balance score. Lower values indicate
#'   better balance of treatments across spatial factors.
#'
#' @examples
#' layout_df <- data.frame(
#'   row = rep(1:3, each = 3),
#'   col = rep(1:3, times = 3),
#'   treatment = rep(letters[1:3], 3)
#' )
#' calculate_balance_score(layout_df, "treatment", c("row", "col"))
#'
#' @export
calculate_balance_score <- function(layout_df, swap, spatial_cols) {
  score <- sapply(spatial_cols, function(el) {
    sum(apply(table(layout_df[[el]], layout_df[[swap]]), 1, var))
  })
  return(sum(score))
}

#' Objective Function Signature
#'
#' @description
#' A signature for an objective function
#'
#' @param design_matrix A design matrix
#' @param layout_df A data frame representing the spatial information of the design
#' @param swap A column name of the treatment
#' @param spatial_cols Column names of the spatial factors
#'
#' @examples
#' design_matrix <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
#' layout_df <- data.frame(
#'   row = rep(1:3, each = 3),
#'   col = rep(1:3, times = 3)
#' )
#' objective_function()(design_matrix, layout_df, "treatment", c("row", "col"))
#'
#' @return A function which returns numeric value representing the score of the design (lower is better)
objective_function_signature <- function(design_matrix, layout_df, swap, spatial_cols) {
  stop("This is a dummy fucntion for documentation purposes only")
}
