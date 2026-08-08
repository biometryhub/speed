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
                               adj_weight = 1,
                               bal_weight = 1,
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

  ring_args <- list(...)
  ring_args <- ring_args[intersect(
    names(ring_args),
    c(
      "ring_dists",
      "ring_weights",
      "ring_type",
      "relationship",
      "by",
      "grid_index"
    )
  )]
  adj_score <- ifelse(adj_weight != 0,
    do.call(
      calculate_adjacency_score,
      c(list(layout_df, swap, row_column, col_column), ring_args)
    ),
    0
  )

  bal_score <- ifelse(bal_weight != 0,
    calculate_balance_score(layout_df, swap, spatial_cols),
    0
  )

  return(list(
    score = round(adj_weight * adj_score + bal_weight * bal_score, 10),
    components = c(
      adjacency = adj_weight * adj_score,
      balance   = bal_weight * bal_score
    )
  ))
}

#' Objective Function for Factorial Design Optimization
#'
#' @inheritParams objective_function
#' @inheritDotParams objective_function
#' @param factorial_separator A character used to separate treatments in the factorial design (default: "-")
#' @param interaction_weight Weight for the balance of interactions (default: 1)
#' @param main_weight Weight for the score of main treatments (default: 1)
#'
#' @examples
#' treatment_a <- paste0("A", 1:8)
#' treatment_b <- paste0("B", 1:3)
#' treatments <- with(expand.grid(treatment_a, treatment_b), paste(Var1, Var2, sep = "-"))
#' df <- initialise_design_df(treatments, 24, 3, 8, 3)
#' objective_function_factorial(df, "treatment", c("row", "col", "block"))
#'
#' @export
# fmt: skip
objective_function_factorial <- function(layout_df,
                                         swap,
                                         spatial_cols,
                                         interaction_weight = 1,
                                         main_weight = 1,
                                         factorial_separator = "-",
                                         ...) {
  if (is.null(factorial_separator) || factorial_separator == "") {
    return(objective_function(layout_df, swap, spatial_cols, ...))
  }

  # count number of treatments
  n_treatments <- stringi::stri_count_fixed(
    as.character(layout_df[[swap]][1]),
    factorial_separator
  ) + 1

  # split treatments
  subtreatments <- stringi::stri_split_fixed(
    as.character(layout_df[[swap]]),
    factorial_separator,
    n = n_treatments,
    simplify = TRUE
  )

  # create temp columns
  treatment_n <- paste0("treatment_", 1:n_treatments)
  layout_df[treatment_n] <- subtreatments

  if (interaction_weight > 0) {
    treatment_score <- calculate_balance_score(layout_df, swap, spatial_cols)
  } else {
    treatment_score <- 0
  }

  if (main_weight > 0) {
    subtreatment_scores <- vapply(treatment_n, function(treatment) {
      objective_function(layout_df, treatment, spatial_cols, ...)$score
    }, numeric(1))
  } else {
    subtreatment_scores <- 0
  }

  return(list(
    score = main_weight * sum(subtreatment_scores) + interaction_weight * treatment_score,
    components = c(
      main        = main_weight * sum(subtreatment_scores),
      interaction = interaction_weight * treatment_score
    )
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

#' Smallest Achievable Balance Score
#'
#' @description
#' Lower bound on [calculate_balance_score()] for any arrangement of `swap`.
#' Each level of a spatial factor holds a fixed number of plots, and the
#' variance of the treatment counts within it is smallest when those plots are
#' split as evenly as possible across the `t` treatments. For a level of `n`
#' plots, the remainder, `rem` is `n %% t`, that minimum has the closed
#' form `rem * (t - rem) / (t * (t - 1))`.
#'
#' @inheritParams objective_function_signature
#'
#' @return A single non-negative numeric value.
#'
#' @seealso [calculate_balance_score()]
#'
#' @keywords internal
.balance_score_min <- function(layout_df, swap, spatial_cols) {
  mins <- vapply(
    spatial_cols,
    function(el) {
      counts <- table(layout_df[[el]], layout_df[[swap]])
      n_treatments <- ncol(counts)
      if (n_treatments < 2) {
        return(0)
      }

      remainders <- rowSums(counts) %% n_treatments
      return(sum(
        remainders *
          (n_treatments - remainders) /
          (n_treatments * (n_treatments - 1))
      ))
    },
    numeric(1)
  )
  return(sum(mins))
}

#' Smallest Achievable Score for the Default Objective
#'
#' @description
#' Lower bound of [objective_function()] for any arrangement of `swap` in this
#' layout: the adjacency component is zero (for simplicity and non zero are
#' mostly impractical) and the balance component is [.balance_score_min()].
#' Because it is a bound rather than an attained value, an unattainable bound
#' is never reached, leaving the run unchanged.
#'
#' Returns `NA_real_` when no bound can be derived: a non-default objective, a
#' `relationship` matrix or any negative weights, `adj_weight`, `bal_weight`,
#' `ring_weights`.
#'
#' @inheritParams objective_function_signature
#' @inheritParams objective_function
#' @param obj_function The objective function used for this level.
#' @param ... Extra arguments for the objective function, as passed to [speed()].
#'   `relationship` and `ring_weights` are read from here.
#'
#' @return A single numeric lower bound, or `NA_real_` when cannot be derived.
#'
#' @seealso [objective_function()], [.balance_score_min()]
#'
#' @keywords internal
.optimal_score <- function(
  layout_df,
  swap,
  spatial_cols,
  obj_function,
  adj_weight = 1,
  bal_weight = 1,
  ...
) {
  dots <- list(...)
  is_boundable <- isTRUE(
    identical(obj_function, objective_function) &&
      is.null(dots$relationship) &&
      all(dots$ring_weights >= 0) &&
      adj_weight >= 0 &&
      bal_weight >= 0
  )
  if (!is_boundable) {
    return(NA_real_)
  }

  bal_min <- .balance_score_min(layout_df, swap, spatial_cols)
  # round as `objective_function()` does
  return(round(bal_weight * bal_min, 10))
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
#' @param by,grid_index Optional grouping of plots into separate grids; see
#'   [calculate_adjacency_score()]. Neighbour balance sums across grids, while
#'   evenness of distribution is scored **per grid** and the scores summed,
#'   reported both in total and per grid. A grid with no treatment replicated
#'   inside it contributes `0`.
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
#'   An objective may optionally return a `components` element: a named numeric vector of the additive pieces
#'   that sum to `score` (e.g. `c(adjacency = ..., balance = ...)`). When present, [summary.design()] reports
#'   this as a faithful score decomposition. See signature
#'   details in [objective_function_signature].
#'
#' @references Piepho, H. P., Michel, V., & Williams, E. (2018). Neighbor balance and evenness of distribution
#'   of treatment replications in row-column designs. Biometrical journal. Biometrische Zeitschrift, 60(6),
#'   1172-1189. <https://doi.org/10.1002/bimj.201800013>
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
                                      by = NULL,
                                      grid_index = NULL,
                                      ...) {
  # `by`/`grid_index` are documented on calculate_adjacency_score()
  if (is.null(grid_index)) {
    grid_index <- grid_indices(design, row_column, col_column, by = by)
  }

  # Each grid is scored on its own: there is no distance between plots at
  # different sites, so a pooled spanning tree would be meaningless.
  ed <- list()
  ed_scores <- setNames(numeric(length(grid_index)), names(grid_index))
  nb_counts <- list()

  for (nm in names(grid_index)) {
    g <- grid_index[[nm]]
    design_matrix <- build_design_matrix(
      design[g$rows, , drop = FALSE],
      swap,
      row_column = row_column,
      col_column = col_column,
      index = g$index
    )
    ed[[nm]] <- calculate_ed(
      design_matrix,
      current_score_obj$ed[[nm]],
      swapped_items
    )
    # A grid with nothing replicated has no spanning tree, so it contributes 0
    # rather than 1/0, which would make the whole score `Inf`.
    ed_scores[[nm]] <- if (length(ed[[nm]]) == 0L) {
      0
    } else {
      1 /
        sum(vapply(ed[[nm]], function(ed_rep) return(ed_rep$min_mst), numeric(1)))
    }
    nb_counts[[nm]] <- unlist(calculate_nb(design_matrix, pair_mapping)$nb)
  }

  # Summed rather than pooled into one reciprocal, so ED scales with adjacency -
  # which also sums per grid - instead of shrinking as sites are added.
  ed_score <- sum(ed_scores)

  # Neighbour balance counts edges and no edge crosses a grid boundary, so the
  # counts sum across grids before the variance is taken.
  all_pairs <- unique(unlist(lapply(nb_counts, names)))
  totals <- setNames(numeric(length(all_pairs)), all_pairs)
  for (counts in nb_counts) {
    totals[names(counts)] <- totals[names(counts)] + counts
  }
  nb <- list(
    nb = totals,
    max_nb = max(totals),
    max_pairs = names(totals)[totals == max(totals)],
    var = stats::var(totals)
  )
  nb_score <- nb$var

  # Balance and adjacency read `design` directly: the treatment column and the
  # coordinates must stay aligned, so neither takes a flattened `design_matrix`.
  bal_score <- calculate_balance_score(design, swap, spatial_cols)
  adj_score <- calculate_adjacency_score(
    design,
    swap,
    row_column,
    col_column,
    grid_index = grid_index
  )

  # Reported alongside the total, not instead of it: per-grid values are not
  # comparable between grids, so they are never ranked or averaged.
  components <- c(
    neighbour_balance = nb_score,
    even_distribution = ed_score,
    balance           = bal_score,
    adjacency         = adj_score
  )
  if (length(grid_index) > 1) {
    components <- c(
      components,
      setNames(ed_scores, paste0("even_distribution_", names(ed_scores)))
    )
  }

  return(list(
    score = round(nb_score + ed_score + bal_score + adj_score, 10),
    ed = ed,
    ed_per_grid = ed_scores,
    bal = bal_score,
    adj = adj_score,
    nb = nb,
    components = components
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
      # Empty cells (a missing plot, or a removed buffer) have no pairs to
      # contribute, matching the pair_mapping path.
      if (is.na(node)) {
        next
      }
      if (row_ < n_rows) {
        bottom <- design_matrix[row_ + 1, col_]
        if (!is.na(bottom)) {
          if (node < bottom) {
            pair_str <- paste0(node, ",", bottom)
          } else {
            pair_str <- paste0(bottom, ",", node)
          }

          env_add_one(nb, pair_str)
        }
      }

      if (col_ < n_cols) {
        right <- design_matrix[row_, col_ + 1]
        if (!is.na(right)) {
          if (node < right) {
            pair_str <- paste0(node, ",", right)
          } else {
            pair_str <- paste0(right, ",", node)
          }

          env_add_one(nb, pair_str)
        }
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
  swapped_items = NULL
) {
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
#' @param row_column Name of the column giving the row of the design (default: "row")
#' @param col_column Name of the column giving the column of the design (default: "col")
#'
#' @examples
#' # `initialise_design_df()` fills `items` down columns, so the literal below is
#' # column-major; the grid it produces is
#' #   a b d c
#' #   e a f b
#' #   c f e d
#' df_design <- initialise_design_df(c(
#'   "a", "e", "c",
#'   "b", "a", "f",
#'   "d", "f", "e",
#'   "c", "b", "d"
#' ), 3, 4)
#'
#' calculate_efficiency_factor(df_design, "treatment")
#'
#' # Not every design can support the estimate. Here each treatment fills one
#' # grid row, so the treatment differences cannot be separated from the row
#' # effects and there is no efficiency factor to report:
#' #   a a a a
#' #   b b b b
#' #   c c c c
#' confounded <- initialise_design_df(rep(c("a", "b", "c"), 4), 3, 4)
#' try(calculate_efficiency_factor(confounded, "treatment"))
#'
#' @return A numeric value representing the efficiency factor of the design,
#'   between 0 and 1. Higher values indicate more efficient designs.
#'
#'   Errors with a `speed_efficiency_rank` condition if the design cannot support
#'   the estimate - that is, if some treatment contrast is not estimable once row
#'   and column effects are eliminated, whether because too few residual degrees
#'   of freedom remain or because a treatment is confounded with a row or column.
#'   Such a design has no efficiency factor; before this check the formula
#'   returned a plausible-looking value, usually above 1.
#'
#' @references Piepho, H. P., Williams, E., & Michel, V. (2015). Nonresolvable Row-Column Designs with an Even
#'   Distribution of Treatment Replications. Journal of Agricultural, Biological, and Environmental Statistics,
#'   21, 227-242 (2016). <https://doi.org/10.1007/s13253-015-0241-2>
#'
#' @export
calculate_efficiency_factor <- function(
  design_df,
  item,
  row_column = "row",
  col_column = "col"
) {
  item <- as.character(substitute(item))

  # An efficiency factor is a property of one experiment, and several cannot be
  # combined. Validated explicitly because pooled sites otherwise return a value
  # above 1 rather than erroring; `summary()` reports one value per site.
  grid_index(design_df, row_column, col_column)

  # Design parameters
  encoded_items <- as.integer(as.factor(design_df[[item]]))
  n_treatments <- length(unique(encoded_items))
  rows <- as_numeric_factor(design_df[[row_column]])
  cols <- as_numeric_factor(design_df[[col_column]])
  n_rows <- max(rows, na.rm = TRUE)
  n_cols <- max(cols, na.rm = TRUE)
  n_plots <- nrow(design_df)

  # Create design matrix X for treatments
  X <- matrix(0, nrow = n_plots, ncol = n_treatments)
  X[cbind(seq_len(n_plots), encoded_items)] <- 1

  # Create design matrix Z for rows and columns, indexed by each plot's own
  # coordinates so the row ordering of `design_df` does not matter. Row and col
  # effects exclude the last row and col to avoid singularity.
  Z_row <- matrix(0, nrow = n_plots, ncol = n_rows - 1)
  Z_col <- matrix(0, nrow = n_plots, ncol = n_cols - 1)
  in_row <- which(rows < n_rows)
  in_col <- which(cols < n_cols)
  Z_row[cbind(in_row, rows[in_row])] <- 1
  Z_col[cbind(in_col, cols[in_col])] <- 1

  # Intercept, then row and column design matrices. The row-column model has a
  # mean, and including it is what makes the estimability test below exact:
  # without it the mean stays inside the treatment term (X's rows sum to 1) and
  # no rank test on `A_RC` can separate estimable contrasts from inestimable
  # ones. Reported values for estimable designs are unchanged.
  Z <- cbind(1, Z_row, Z_col)

  # Moore-Penrose inverse, as for A_RC below: it agrees with solve() when Z has
  # full column rank, and stays defined if it ever does not.
  ZtZ <- t(Z) %*% Z
  ZtZ_inv <- pseudo_inverse(ZtZ)

  # Calculate treatment information matrix A_RC
  # A_RC = X^T (I - P_Z) X
  P_Z <- Z %*% ZtZ_inv %*% t(Z)
  I_n <- diag(n_plots)
  A_RC <- t(X) %*% (I_n - P_Z) %*% X

  # Rank n_treatments - 1 means every treatment contrast is estimable. Without
  # this gate pseudo_inverse() drops the null directions and returns a
  # plausible-looking value above 1 instead of failing. Rank catches both
  # aliasing and too few residual degrees of freedom; counting degrees of
  # freedom catches only the latter. The tolerance matches pseudo_inverse()'s so
  # the two cannot disagree, and `qr()` is unusable here - its relative default
  # reports full rank for eigenvalues 2, 9e-16, 6e-16.
  if (sum(svd(A_RC)$d > 1e-10) != n_treatments - 1) {
    stop(structure(
      class = c(
        "speed_efficiency_rank",
        "speed_efficiency_error",
        "error",
        "condition"
      ),
      list(
        message = paste0(
          "Not all treatment contrasts are estimable after eliminating ",
          "`", row_column, "` and `", col_column, "` effects, so this design ",
          "cannot support an efficiency factor."
        ),
        reason = "treatment contrasts not estimable given row + col",
        call = NULL
      )
    ))
  }

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
