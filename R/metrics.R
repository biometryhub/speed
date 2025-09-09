#' Default objective functions
#'
#' @description
#' Default Objective Function for Design Optimization
#'
#' @rdname objective_functions
#'
#' @param layout_df A data frame representing the current design
#' @param swap A column name of the items to be swapped
#' @param spatial_cols Column name(s) of the spatial factors
#' @param ... Extra parameters passed from [speed]
#'
#' @examples
#' layout_df <- data.frame(
#'   row = rep(1:3, each = 3),
#'   col = rep(1:3, times = 3),
#'   treatment = rep(letters[1:3], 3)
#' )
#' objective_function(layout_df, "treatment", c("row", "col"))
#'
#' @return A numeric value representing the score of the design (lower is better)
#' @export
# fmt: skip
objective_function_signature <- function(layout_df,
                                         swap,
                                         spatial_cols,
                                         ...) {
  stop("This is a dummy function for documentation purposes only")
}

#' Default Objective Function for Design Optimization
#'
#' @param adj_weight Weight for adjacency score (default: 1)
#' @param bal_weight Weight for balance score (default: 1)
#' @param row_column Name of column representing the row of the design (default: "row")
#' @param col_column Name of column representing the column of the design (default: "col")
#'
#' @rdname objective_functions
#' @export
# fmt: skip
objective_function <- function(layout_df,
                               swap,
                               spatial_cols,
                               adj_weight = getOption("speed.adj_weight", 1),
                               bal_weight = getOption("speed.bal_weight", 1),
                               row_column = "row",
                               col_column = "col",
                               ...) {
  # Check if there are only two treatments - adjacency becomes deterministic
  n_treatments <- length(unique(layout_df[[swap]]))
  if (n_treatments == 2 && adj_weight != 0) {
    warning("Only 2 treatments detected in '", swap, "'. Adjacency optimization becomes deterministic (checkerboard pattern). Setting adjacency weight to 0.",
      call. = FALSE
    )
    adj_weight <- 0
  }

  adj_score <- ifelse(adj_weight != 0,
    calculate_adjacency_score(layout_df, swap, row_column, col_column),
    0
  )

  bal_score <- ifelse(bal_weight != 0,
    calculate_balance_score(layout_df, swap, spatial_cols),
    0
  )

  return(list(
    score = round(adj_weight * adj_score + bal_weight * bal_score, 10)
  ))
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
    sum(
      matrixStats::rowVars(
        table(layout_df[[el]], layout_df[[swap]]),
        na.rm = TRUE
      ),
      na.rm = TRUE
    )
  })
  return(sum(score))
}

#' Calculate Adjacency Score for Design
#'
#' @description
#' Calculates the adjacency score for a given experimental design. The adjacency score
#' represents the number of adjacent plots with the same treatment. Lower scores indicate
#' better separation of treatments.
#'
#' @inheritParams objective_function_signature
#'
#' @return Numeric score for treatment adjacencies (lower is better)
#'
#' @examples
#' # Example 1: Design with no adjacencies
#' design_no_adj <- data.frame(
#'   row = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   col = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   treatment = c("A", "B", "A", "B", "A", "B", "A", "B", "A")
#' )
#'
#' # Gives 0
#' calculate_adjacency_score(design_no_adj, "treatment")
#'
#' # Example 2: Design with adjacencies
#' design_with_adj <- data.frame(
#'   row = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   col = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   treatment = c("A", "A", "A", "B", "B", "B", "A", "A", "A")
#' )
#'
#' # Gives value 6
#' calculate_adjacency_score(design_with_adj, "treatment")
#'
#' @export
calculate_adjacency_score <- function(layout_df, swap, row_column = "row", col_column = "col") {
  layout_df <- matrix(
    layout_df[[swap]],
    nrow = max(as.numeric(as.character(layout_df[[row_column]])), na.rm = TRUE),
    ncol = max(as.numeric(as.character(layout_df[[col_column]])), na.rm = TRUE),
    byrow = TRUE
  )

  row_adjacencies <- sum(
    layout_df[, -ncol(layout_df)] == layout_df[, -1],
    na.rm = TRUE
  )
  col_adjacencies <- sum(
    layout_df[-nrow(layout_df), ] == layout_df[-1, ],
    na.rm = TRUE
  )
  return(row_adjacencies + col_adjacencies)
}

#' Objective Function with Metric from Piepho
#'
#' @description
#' Create an objective function including even distribution and neighbor balance introduced by Piepho 2018.
#'
#' @inheritParams objective_function_signature
#' @inheritParams objective_function
#' @inheritParams calculate_nb
#' @inheritParams calculate_ed
#' @param design A data frame representing the spatial information of the design
#' @param current_score_obj A named list containing the current score
#'
#' @examples
#' design_df <- initialise_design_df(
#'   items = c(1, 2, 2, 1, 3, 3, 1, 3, 3),
#'   nrows = 3,
#'   ncols = 3
#' )
#'
#' pair_mapping <- create_pair_mapping(design_df$treatment)
#' objective_function_piepho(design_df, "treatment", c("row", "col"), pair_mapping = pair_mapping)
#' # usage in speed, speed(..., obj_function = objective_function_piepho, pair_mapping = pair_mapping)
#'
#' @return A function which returns a named list of numeric values with one required name `score` representing
#'   the score of the design (lower is better) with a signature `function(design_df, swap, spatial_cols, ...)`.
#'   See signature
#'   details in [objective_function_signature].
#'
#' @references Piepho, H. P., Michel, V., & Williams, E. (2018). Neighbor balance and evenness of distribution
#'   of treatment replications in row-column designs. Biometrical journal. Biometrische Zeitschrift, 60(6),
#'   1172–1189. <https://doi.org/10.1002/bimj.201800013>
#'
#' @seealso [objective_function()], [create_pair_mapping()]
#'
#' @export
# fmt: skip
objective_function_piepho <- function(design,
                                      swap,
                                      spatial_cols,
                                      current_score_obj = NULL,
                                      swapped_items = NULL,
                                      pair_mapping = NULL,
                                      row_column = "row",
                                      col_column = "col",
                                      ...) {
  design_matrix <- matrix(
    design[[swap]],
    nrow = max(as.numeric(levels(design$row))),
    ncol = max(as.numeric(levels(design$col)))
  )

  ed <- calculate_ed(design_matrix, current_score_obj$ed, swapped_items)
  # sum(1/) or 1/sum
  ed_score <- 1 / sum(vapply(ed, function(ed_rep) ed_rep$min_mst, numeric(1)))
  nb <- calculate_nb(design_matrix, pair_mapping)
  nb_score <- nb$var

  design[[swap]] <- as.factor(design_matrix)
  bal_score <- calculate_balance_score(design, swap, spatial_cols)
  adj_score <- calculate_adjacency_score(design, swap, row_column, col_column)

  return(list(
    score = round(nb_score + ed_score + bal_score + adj_score, 10),
    ed = ed,
    bal = bal_score,
    adj = adj_score,
    nb = nb
  ))
}

#' Neighbour Balance Calculation
#'
#' @description
#' A metric that counts the occurrence of the same adjacent pairs. Only horizontal and vertical pairs are
#'   counted.
#'
#' @param design_matrix A matrix representing the design
#' @param pair_mapping A named vector of pairs generated from [create_pair_mapping]
#'
#' @examples
#' design_matrix <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
#' calculate_nb(design_matrix)
#'
#' @return Named list containing:
#' \itemize{
#'   \item nb - Table of pairs of items and their number of occurrence
#'   \item max_nb - The highest number of occurrence
#'   \item max_pairs - Vector of pairs of items with the highest number of occurrence
#' }
#'
#' @seealso [objective_function_piepho()]
#'
#' @export
calculate_nb <- function(design_matrix, pair_mapping = NULL) {
  if (is.null(pair_mapping)) {
    return(.calculate_nb(design_matrix))
  }

  lefts <- design_matrix[, -ncol(design_matrix)]
  rights <- design_matrix[, -1]
  tops <- design_matrix[-nrow(design_matrix), ]
  bottoms <- design_matrix[-1, ]
  lr_pairs <- paste(lefts, rights, sep = ",")
  tb_pairs <- paste(tops, bottoms, sep = ",")

  pairs <- c(lr_pairs, tb_pairs)
  is_sorted <- pairs %in% pair_mapping
  sorted_pairs <- c(pairs[is_sorted], pair_mapping[pairs[!is_sorted]])

  nb <- table(sorted_pairs)
  max_nb <- max(nb)
  max_pairs <- names(nb[nb == max_nb])
  return(list(
    nb = nb,
    max_nb = max_nb,
    max_pairs = max_pairs,
    var = var(nb)
  ))
}

#' Neighbor Balance Calculation without Pair Mapping
#'
#' @description
#' A metric that counts the occurrence of the same adjacent pairs. Only horizontal and vertical pairs are
#'   counted.
#'
#' @inheritParams calculate_nb
#'
#' @return Named list containing:
#' \itemize{
#'   \item nb - Named list of pairs of items and their number of occurrence
#'   \item max_nb - The highest number of occurrence
#'   \item max_pairs - Vector of pairs of items with the highest number of occurrence
#' }
#'
#' @keywords internal
.calculate_nb <- function(design_matrix) {
  n_rows <- dim(design_matrix)[1]
  n_cols <- dim(design_matrix)[2]
  # env is faster than list
  nb <- new.env()

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
  nb_values <- unlist(nb)
  max_nb <- max(nb_values)
  max_pairs <- names(nb[nb == max_nb])
  return(list(
    nb = nb,
    max_nb = max_nb,
    max_pairs = max_pairs,
    var = var(nb_values)
  ))
}

#' Even Distribution Calculation
#'
#' @description
#' A metric that represents the even distribution of each item with their minimum spanning tree (mst).
#'
#' @inheritParams calculate_nb
#' @param current_ed Named list of the current ed calculation
#' @param swapped_items The items that had just been swapped
#'
#' @examples
#' design_matrix <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
#' calculate_ed(design_matrix)
#'
#' @return Named list containing:
#' \itemize{
#'   \item <number of replications> - Named list containing:
#'     \itemize{
#'       \item msts - Named list of items and their mst
#'       \item min_mst - The lowest mst
#'       \item min_items - Pairs of items with the lowest mst
#'     }
#' }
#'
#' @seealso [objective_function_piepho()]
#'
#' @export
calculate_ed <- function(
    design_matrix,
    current_ed = NULL,
    swapped_items = NULL) {
  if (!is.null(swapped_items)) {
    design_matrix[!(design_matrix %in% swapped_items)] <- NA
    msts <- lapply(current_ed, function(ed_by_rep) ed_by_rep$msts)
  } else {
    msts <- list()
  }

  vertices <- get_vertices(design_matrix)
  edges <- get_edges(vertices)

  edges_3_reps <- list()
  sub_graph <- list()

  for (item in names(vertices)) {
    reps <- length(vertices[[item]])
    reps_char <- as.character(reps)
    if (reps == 2) {
      # distance between 2 nodes for 2 reps
      msts[[reps_char]][[item]] <- edges[[item]]
    } else if (reps == 3) {
      # 3 reps will be calculated with .calculate_ed_3_reps
      edges_3_reps[[item]] <- edges[[item]]
    } else if (reps > 3) {
      # blanket igraph for 4+ reps
      if (is.null(sub_graph[[reps_char]])) {
        # initialize a fully-connected graph without weights
        # 1--2, 1--3, ..., 1--n-1, 1--n, 2--3, 2--4, ..., n-1--n
        edge_table <- t(combn(1:reps, 2))
        sub_graph[[reps_char]] <- igraph::graph_from_edgelist(
          edge_table,
          directed = FALSE
        )
      }

      igraph::E(sub_graph[[reps_char]])$weight <- edges[[item]]
      msts[[reps_char]][[item]] <- sum(
        igraph::E(igraph::mst(sub_graph[[reps_char]]))$weight
      )
    }
  }

  # summarize mst for each reps
  ed <- lapply(msts, function(msts_by_reps) {
    min_mst <- min(unlist(msts_by_reps))
    min_items <- names(msts_by_reps[msts_by_reps == min_mst])

    return(list(
      msts = msts_by_reps,
      min_mst = min_mst,
      min_items = min_items
    ))
  })

  if (length(edges_3_reps) > 0) {
    ed$`3` <- .calculate_ed_3_reps(edges_3_reps, current_ed)
  }

  return(ed)
}

#' Get Vertices of Each Item
#'
#' @description
#' Get the vertices of each item in a design matrix.
#'
#' @inheritParams calculate_nb
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
#'     \item <item> - A vector of edge weights
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
      edges[[item]] <- c()
      next
    }

    # Preallocate vector to hold all edges
    item_edges <- numeric(n_vertices * (n_vertices - 1) / 2)
    idx <- 1

    for (i in 1:(n_vertices - 1)) {
      for (j in (i + 1):n_vertices) {
        item_edges[[idx]] <- sqrt(sum((coords[[i]] - coords[[j]])^2))
        idx <- idx + 1
      }
    }

    edges[[item]] <- item_edges
  }

  return(edges)
}

#' Even Distribution Calculation for 3 Replications
#'
#' @description
#' A metric that represents the even distribution of items with 3 replications with their minimum spanning tree
#'   (mst).
#'
#' @param edges A list of vectors of edge weights
#'
#' @importFrom utils modifyList
#'
#' @return Named list containing:
#' \itemize{
#'   \item msts - Named list of pairs of items and their mst
#'   \item min_mst - The lowest mst
#'   \item min_items - Pairs of items with the lowest mst
#' }
#'
#' @seealso [get_edges()]
#'
#' @keywords internal
.calculate_ed_3_reps <- function(edges, current_ed = NULL) {
  # pick 2 shortest connections for 3 reps
  ed <- lapply(
    edges,
    function(weights) {
      sum(weights) - max(weights)
    }
  )

  if (!is.null(current_ed)) {
    ed <- modifyList(current_ed$`3`$msts, ed)
  }

  min_mst <- min(unlist(ed))
  min_items <- names(ed[ed == min_mst])
  return(list(
    msts = ed,
    min_mst = min_mst,
    min_items = min_items
  ))
}

#' Create Pair Mapping
#'
#' @description
#' Create an item pair mapping for [calculate_nb].
#'
#' @param items Vector of items for the design
#'
#' @importFrom stats setNames
#'
#' @examples
#' treatments <- c(rep(1:10, 4), rep(11:16, 3), rep(17:27, 2))
#' create_pair_mapping(treatments)
#'
#' @return Named vector of item pairs as a character separated by `","`:
#' \itemize{
#'   \item "<item 2>,<item 1>" - "<item 1>,<item 2>"
#'   \item "<item 3>,<item 1>" - "<item 1>,<item 3>"
#'   \item ...
#'   \item "<item n-1>,<item 1>" - "<item 1>,<item n-1>"
#'   \item "<item n>,<item 1>" - "<item 1>,<item n>"
#'   \item "<item 3>,<item 2>" - "<item 2>,<item 3>"
#'   \item "<item 4>,<item 2>" - "<item 2>,<item 4>"
#'   \item ...
#'   \item "<item n-1>,<item 2>" - "<item 2>,<item n-1>"
#'   \item "<item n>,<item 2>" - "<item 2>,<item n>"
#'   \item ...
#'   \item "<item n>,<item n-1>" - "<item n-1>,<item n>"
#'   \item "<item 1>,<item 1>" - "<item 1>,<item 1>"
#'   \item "<item 2>,<item 2>" - "<item 2>,<item 2>"
#'   \item ...
#'   \item "<item n-1>,<item n-1>" - "<item n-1>,<item n-1>"
#'   \item "<item n>,<item n>" - "<item n>,<item n>"
#' }
#'
#' @export
create_pair_mapping <- function(items) {
  items <- unique(items)
  combinations <- combn(sort(items), 2)

  identical_pairs <- paste(items, items, sep = ",")
  pairs <- paste(combinations[1, ], combinations[2, ], sep = ",")
  pairs_r <- sapply(
    pairs,
    function(k) paste(rev(strsplit(k, ",")[[1]]), collapse = ",")
  )

  pair_mapping <- setNames(
    c(pairs, identical_pairs),
    c(pairs_r, identical_pairs)
  )
  return(pair_mapping)
}

#' Calculate Efficiency Factor according Piepho
#'
#' @description
#' Calculates an efficiency factor of a design according to Piepho 2015.
#'
#' @param design_df A data frame containing the experimental design with spatial coordinates
#' @param item A column name of the items in the design (e.g., `treatment`, `variety`, `genotype`, etc)
#'
#' @examples
#' df_design <- initialise_design_df(c(
#'   "a", "b", "d", "c",
#'   "e", "a", "f", "b",
#'   "c", "f", "e", "d"
#' ), 3, 4)
#'
#' calculate_efficiency_factor(df_design, "treatment")
#'
#' @return A numeric value representing the efficiency factor of the design. Higher values indicate more efficient designs.
#'
#' @references Piepho, H. P., Williams, E., & Michel, V. (2015). Nonresolvable Row-Column Designs with an Even
#'   Distribution of Treatment Replications. Journal of Agricultural, Biological, and Environmental Statistics,
#'   21, 227-242 (2016). <https://doi.org/10.1007/s13253-015-0241-2>
#'
#' @export
calculate_efficiency_factor <- function(design_df, item) {
  item <- as.character(substitute(item))

  # Design parameters
  encoded_items <- as.integer(as.factor(design_df[[item]]))
  n_treatments <- length(unique(encoded_items))
  n_rows <- max(as.numeric(as.character(design_df$row)))
  n_cols <- max(as.numeric(as.character(design_df$col)))
  n_plots <- nrow(design_df)

  # Create design matrix X for treatments
  X <- matrix(0, nrow = n_plots, ncol = n_treatments)
  for (i in 1:n_plots) {
    X[i, encoded_items[i]] <- 1
  }

  # Create design matrix Z for rows and columns
  # Row and col effects (excluding last row and col to avoid singularity)
  Z_row <- matrix(0, nrow = n_plots, ncol = n_rows - 1)
  Z_col <- matrix(0, nrow = n_plots, ncol = n_cols - 1)
  plot_index <- 1
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      if (i < n_rows) {
        Z_row[plot_index, i] <- 1
      }
      if (j < n_cols) {
        Z_col[plot_index, j] <- 1
      }
      plot_index <- plot_index + 1
    }
  }

  # Combine row and column design matrices
  Z <- cbind(Z_row, Z_col)

  # Check if Z^TZ is invertible
  ZtZ <- t(Z) %*% Z
  condition_number <- kappa(ZtZ)

  # Use Moore-Penrose inverse if matrix is near singular
  if (condition_number > 1e12) {
    ZtZ_inv <- pseudo_inverse(ZtZ)
  } else {
    ZtZ_inv <- solve(ZtZ)
  }

  # Calculate treatment information matrix A_RC
  # A_RC = X^T (I - P_Z) X
  P_Z <- Z %*% ZtZ_inv %*% t(Z)
  I_n <- diag(n_plots)
  A_RC <- t(X) %*% (I_n - P_Z) %*% X

  # Calculate Moore-Penrose inverse of A_RC, variance matrix
  V <- pseudo_inverse(A_RC)

  # Calculate average pairwise variance (apv)
  apv <- 0
  count <- 0
  for (i in 1:(n_treatments - 1)) {
    for (j in (i + 1):n_treatments) {
      pairwise_var <- V[i, i] + V[j, j] - 2 * V[i, j]
      apv <- apv + pairwise_var
      count <- count + 1
    }
  }
  apv <- apv / count

  # Calculate harmonic means of replications
  r_i <- colSums(X)
  r_h <- length(r_i) / sum(1 / r_i)

  # Calculate average efficiency factor
  f_A <- (2 / r_h) / apv
  return(f_A)
}
