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
#' An objective function combining the even distribution (ED) and neighbour balance (NB) criteria of
#'   Piepho et al. (2018, 2021).
#'
#' @details
#' The score is `nb$var + ed$inv_total_mst + self_adj_weight * nb$self_adjacencies`, and is minimised.
#'   See [calculate_ed] for the ED component and [calculate_nb] for the NB component.
#'
#'   The self-adjacency term stands in for the binarity requirement of the source papers, which reject
#'   outright any design placing an item next to itself. For a binary design the term is exactly 0, so
#'   it changes nothing; it only penalises designs the papers would consider inadmissible.
#'
#'   This corresponds to the second of the two design strategies proposed by Piepho et al. (2018), which
#'   *"directly optimizes ED and NB, while simultaneously seeking to minimize the loss in row-column
#'   efficiency"* (Section 3.2). The first half of that is what this function does. It does **not**
#'   address the efficiency half: the average efficiency factor is not considered anywhere in the score,
#'   so a design that improves ED and NB may do so at a cost in efficiency that goes unmeasured. Use
#'   [calculate_efficiency_factor] to check it for a returned design.
#'
#' @inheritParams objective_function_signature
#' @inheritParams objective_function
#' @inheritParams calculate_nb
#' @inheritParams calculate_ed
#' @param design A data frame representing the spatial information of the design
#' @param current_score_obj A named list containing the current score
#' @param nb_directions Adjacency directions used for the neighbour balance component, passed to
#'   [calculate_nb] as `directions`. Defaults to `"auto"`, which picks the direction from the shape of
#'   the layout following Piepho et al. (2021); see [calculate_nb] for the rule.
#' @param self_adj_weight Weight applied to the number of same-item adjacencies (default: 1). Use 0 to
#'   score ED and NB alone.
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
#' @return A named list with the required element `score`, the score of the design (lower is better),
#'   alongside the `ed` and `nb` components it was built from and a `components` element giving the
#'   additive pieces that sum to `score`, which [summary.design()] reports as a score decomposition. The
#'   `ed` element is fed back as `current_score_obj` on the next call so that MST lengths need only be
#'   recomputed for the items that moved; see [objective_function_signature] for the full contract.
#'
#' @references Piepho, H. P., Michel, V., & Williams, E. (2018). Neighbor balance and evenness of
#'   distribution of treatment replications in row-column designs. Biometrical Journal, 60(6),
#'   1172-1189. <https://doi.org/10.1002/bimj.201800013>
#'
#'   Piepho, H. P., Williams, E. R., & Michel, V. (2021). Generating row-column field experimental
#'   designs with good neighbour balance and even distribution of treatment replications. Journal of
#'   Agronomy and Crop Science, 207, 745-753. <https://doi.org/10.1111/jac.12463>
#'
#' @seealso [objective_function()], [calculate_ed()], [calculate_nb()], [create_pair_mapping()],
#'   [calculate_efficiency_factor()]
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
                                      nb_directions = "auto",
                                      self_adj_weight = 1,
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
  self_adj <- setNames(numeric(length(grid_index)), names(grid_index))

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
    ed_scores[[nm]] <- if (is.finite(ed[[nm]]$inv_total_mst)) {
      ed[[nm]]$inv_total_mst
    } else {
      0
    }
    nb_grid <- calculate_nb(
      design_matrix,
      pair_mapping,
      directions = nb_directions
    )
    nb_counts[[nm]] <- nb_grid$nb
    self_adj[[nm]] <- nb_grid$self_adjacencies
  }

  # Summed rather than pooled into one reciprocal, so ED scales with the number
  # of grids instead of shrinking as sites are added.
  ed_score <- sum(ed_scores)

  # Neighbour balance counts edges and no edge crosses a grid boundary, so the
  # counts sum across grids before the variance is taken.
  all_pairs <- unique(unlist(lapply(nb_counts, names)))
  totals <- setNames(numeric(length(all_pairs)), all_pairs)
  for (counts in nb_counts) {
    totals[names(counts)] <- totals[names(counts)] + counts
  }
  max_nb <- max(totals)
  self_adjacencies <- sum(self_adj)
  nb <- list(
    nb = totals,
    max_nb = max_nb,
    max_pairs = names(totals)[totals == max_nb],
    # var() of a single pair is NA; such a design is trivially balanced
    var = if (length(totals) > 1L) var(totals) else 0,
    s2 = sum(totals * (totals - 1) / 2),
    self_adjacencies = self_adjacencies
  )
  nb_score <- nb$var
  self_adj_score <- self_adj_weight * self_adjacencies

  # Reported alongside the total, not instead of it: per-grid values are not
  # comparable between grids, so they are never ranked or averaged.
  components <- c(
    neighbour_balance = nb_score,
    even_distribution = ed_score,
    self_adjacency    = self_adj_score
  )
  if (length(grid_index) > 1) {
    components <- c(
      components,
      setNames(ed_scores, paste0("even_distribution_", names(ed_scores)))
    )
  }

  return(list(
    score = round(nb_score + ed_score + self_adj_score, 10),
    ed = ed,
    ed_per_grid = ed_scores,
    nb = nb,
    components = components
  ))
}

#' Neighbour Balance Calculation
#'
#' @description
#' A metric describing the neighbour balance (NB) of a design: how evenly the direct adjacencies of the
#'   design are spread over the distinct pairs of items.
#'
#' @details
#' Following Piepho et al. (2018), Section 3.2(c), neighbour balance is assessed from `n_h`, the number
#'   of item *pairs* having `h` adjacencies. Two properties of that definition matter in practice:
#'
#'   * The tabulation covers **every** distinct pair of items, so a pair that is never adjacent
#'     contributes a count of 0. Dropping such pairs would let a design improve its score by making
#'     pairs disappear rather than by balancing them.
#'   * Self-pairs (an item adjacent to itself) are **not** part of `n_h`; the source papers exclude
#'     such designs by requiring the design to be binary. Self-adjacencies are still counted here and
#'     reported as `self_adjacencies` so that they can be penalised separately.
#'
#'   Complete NB means every pair is adjacent equally often, and partial NB means the counts take only
#'   two values differing by one. Both `var` and `s2` measure departure from that ideal, and both are
#'   quantities to *minimise*. For binary designs they are equivalent up to a positive affine
#'   transformation, because the total number of adjacencies is then fixed by the layout.
#'
#' @param design_matrix A matrix representing the design
#' @param pair_mapping A named vector of pairs generated from [create_pair_mapping]. Generated from
#'   `design_matrix` when not supplied.
#' @param directions Adjacency directions to count: `"row"` for horizontal neighbours within a row,
#'   `"col"` for vertical neighbours within a column, or both. Defaults to `"auto"`, which picks the
#'   direction from the shape of the layout following Piepho et al. (2021), Section 2: rows when there
#'   are more columns than rows - the usual field case, where plots are long and thin down the column,
#'   so that only the long boundaries between plots in a row are shared - columns when there are more
#'   rows than columns, and both when the layout is square and neither orientation is privileged.
#'   Resolvability and latinization also enter that rule in the source paper, but `speed` does not model
#'   those, so pass the directions explicitly for a resolvable design.
#'
#' @importFrom stats var
#'
#' @examples
#' design_matrix <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
#' calculate_nb(design_matrix)
#'
#' # count adjacencies along rows only, as in Piepho et al. (2018)
#' calculate_nb(design_matrix, directions = "row")
#'
#' @return Named list containing:
#' \itemize{
#'   \item nb - Named integer vector of counts for every distinct pair of items, including pairs that
#'     are never adjacent (count 0)
#'   \item max_nb - The highest number of occurrences
#'   \item max_pairs - Vector of pairs of items with the highest number of occurrences
#'   \item var - Variance of `nb`; 0 when the design is completely neighbour balanced
#'   \item s2 - The NB score of Piepho et al. (2018), Table 2: `sum(n * (n - 1) / 2)` over pairs
#'   \item self_adjacencies - Number of adjacencies between plots holding the same item, excluded
#'     from `nb`
#' }
#'
#' @references Piepho, H. P., Michel, V., & Williams, E. (2018). Neighbor balance and evenness of
#'   distribution of treatment replications in row-column designs. Biometrical Journal, 60(6),
#'   1172-1189. <https://doi.org/10.1002/bimj.201800013>
#'
#'   Piepho, H. P., Williams, E. R., & Michel, V. (2021). Generating row-column field experimental
#'   designs with good neighbour balance and even distribution of treatment replications. Journal of
#'   Agronomy and Crop Science, 207, 745-753. <https://doi.org/10.1111/jac.12463>
#'
#' @seealso [objective_function_piepho()], [create_pair_mapping()]
#'
#' @export
calculate_nb <- function(
  design_matrix,
  pair_mapping = NULL,
  directions = "auto"
) {
  if (identical(directions, "auto")) {
    n_rows <- nrow(design_matrix)
    n_cols <- ncol(design_matrix)
    directions <- if (n_cols > n_rows) {
      "row"
    } else if (n_rows > n_cols) {
      "col"
    } else {
      c("row", "col")
    }
  } else {
    directions <- match.arg(directions, c("row", "col"), several.ok = TRUE)
  }

  if (is.null(pair_mapping)) {
    pair_mapping <- create_pair_mapping(as.vector(design_matrix))
  }

  # Plots holding no item (e.g. buffers, empty plots) form no pair at all
  paste_adjacent <- function(a, b) {
    keep <- !is.na(a) & !is.na(b)
    paste(a[keep], b[keep], sep = ",")
  }

  pairs <- character(0)
  if ("row" %in% directions && ncol(design_matrix) > 1) {
    pairs <- c(
      pairs,
      paste_adjacent(
        design_matrix[, -ncol(design_matrix)],
        design_matrix[, -1]
      )
    )
  }
  if ("col" %in% directions && nrow(design_matrix) > 1) {
    pairs <- c(
      pairs,
      paste_adjacent(
        design_matrix[-nrow(design_matrix), ],
        design_matrix[-1, ]
      )
    )
  }

  # Canonicalise every adjacency to the sorted "<lower>,<higher>" form
  is_sorted <- pairs %in% pair_mapping
  pairs[!is_sorted] <- pair_mapping[pairs[!is_sorted]]
  if (anyNA(pairs)) {
    stop(
      "`pair_mapping` does not cover every item present in the design. ",
      "Regenerate it with create_pair_mapping().",
      call. = FALSE
    )
  }

  universe <- attr(pair_mapping, "pairs")
  is_self <- pairs %in% attr(pair_mapping, "self_pairs")

  # Every distinct pair starts at zero, so pairs that never adjoin still count
  nb <- setNames(integer(length(universe)), universe)
  observed <- table(pairs[!is_self])
  nb[names(observed)] <- as.integer(observed)

  max_nb <- max(nb)
  return(list(
    nb = nb,
    max_nb = max_nb,
    max_pairs = names(nb)[nb == max_nb],
    # var() of a single pair is NA; such a design is trivially balanced
    var = if (length(nb) > 1L) var(nb) else 0,
    s2 = sum(nb * (nb - 1) / 2),
    self_adjacencies = sum(is_self)
  ))
}

#' Mean MST Edge Length by Prim's Algorithm
#'
#' @description
#' Compute the mean edge length of the minimum spanning tree of a distance matrix directly.
#'
#' @details
#' Preferred over `igraph` for the small point sets a single item's replications form. `igraph`'s graph
#'   construction dominates its runtime at that size, making it roughly 10-30x slower than this loop for
#'   2 to 10 points, with the crossover around 20-25 points. This implementation is also correct for
#'   coincident points, which `igraph::graph_from_adjacency_matrix()` is not - it reads a weight of 0 as
#'   an absent edge, so two plots at the same position would silently drop out of the tree.
#'
#' @param d A symmetric matrix of pairwise distances.
#'
#' @return The total tree length divided by its `n - 1` edges.
#'
#' @keywords internal
.mst_mean_prim <- function(d) {
  n <- nrow(d)
  visited <- rep(FALSE, n)
  visited[1] <- TRUE
  mst_len <- 0

  for (i in 2:n) {
    min_edge <- Inf
    for (u in which(visited)) {
      for (v in which(!visited)) {
        if (d[u, v] < min_edge) {
          min_edge <- d[u, v]
          v_min <- v
        }
      }
    }
    mst_len <- mst_len + min_edge
    visited[v_min] <- TRUE
  }

  return(mst_len / (n - 1))
}

#' Mean MST Edge Length via igraph
#'
#' @description
#' Compute the mean edge length of the minimum spanning tree of a distance matrix using `igraph`.
#'
#' @details
#' Faster than `.mst_mean_prim` only for larger point sets. Note that a distance of exactly 0 is read as
#'   an absent edge by `igraph::graph_from_adjacency_matrix()`, so this must not be used where points may
#'   coincide.
#'
#' @inheritParams .mst_mean_prim
#'
#' @return The total tree length divided by its `n - 1` edges.
#'
#' @keywords internal
.mst_mean_igraph <- function(d) {
  g <- igraph::graph_from_adjacency_matrix(
    d,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )
  return(mean(igraph::E(igraph::mst(g))$weight))
}

#' Even Distribution Calculation
#'
#' @description
#' A metric that represents the even distribution (ED) of each item, measured by the minimum spanning
#'   tree (MST) connecting all replications of that item.
#'
#' @details
#' Following Piepho et al. (2018), Section 3.2(b), `MST_i` is the **arithmetic mean** length of the
#'   edges of the minimum spanning tree connecting the replications of item `i`, using Euclidean
#'   distance in row and column numbers. The mean (rather than the total) is what makes the measure
#'   comparable across items with different numbers of replications, since a tree over `r` replications
#'   has `r - 1` edges. Items with fewer than two replications are given an `MST_i` of 0 and are
#'   excluded from `inv_total_mst`.
#'
#'   Larger `MST_i` means the replications of item `i` are more evenly spread, so `inv_total_mst` is
#'   the quantity to *minimise*.
#'
#' @inheritParams calculate_nb
#' @param current_ed Named list of the current ed calculation
#' @param swapped_items The items that had just been swapped
#'
#' @importFrom stats dist
#'
#' @examples
#' design_matrix <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
#' calculate_ed(design_matrix)
#'
#' @return Named list containing:
#' \itemize{
#'   \item msts - Named numeric vector of each item's `MST_i` (mean MST edge length)
#'   \item total_mst - Sum of `MST_i` across items
#'   \item inv_total_mst - Sum of `1 / MST_i` across items (excluding items with `MST_i` of 0)
#' }
#'
#' @references Piepho, H. P., Michel, V., & Williams, E. (2018). Neighbor balance and evenness of
#'   distribution of treatment replications in row-column designs. Biometrical Journal, 60(6),
#'   1172-1189. <https://doi.org/10.1002/bimj.201800013>
#'
#' @seealso [objective_function_piepho()]
#'
#' @export
calculate_ed <- function(
  design_matrix,
  current_ed = NULL,
  swapped_items = NULL
) {
  rows <- row(design_matrix)
  cols <- col(design_matrix)

  treatments <- as.character(design_matrix)
  coords <- data.frame(
    trt = treatments,
    row = as.vector(rows),
    col = as.vector(cols)
  )

  trt_groups <- split(coords[, c("row", "col")], coords$trt)

  # Initialise from previous ED if supplied
  if (!is.null(current_ed)) {
    msts <- current_ed$msts
  } else {
    msts <- numeric(length(trt_groups))
    names(msts) <- names(trt_groups)
  }

  # Determine which treatments need recomputation
  if (is.null(swapped_items)) {
    recompute <- names(trt_groups)
  } else {
    recompute <- intersect(names(trt_groups), as.character(swapped_items))
  }

  has_igraph <- requireNamespace("igraph", quietly = TRUE)

  for (trt in recompute) {
    xy <- as.matrix(trt_groups[[trt]])
    n <- nrow(xy)

    if (n < 2) {
      msts[trt] <- 0
      next
    }

    d <- as.matrix(dist(xy, method = "euclidean"))
    # Below ~20 points igraph's graph construction costs more than the whole tree
    msts[trt] <- if (n > 20 && has_igraph) {
      .mst_mean_igraph(d)
    } else {
      .mst_mean_prim(d)
    }
  }

  return(list(
    msts = msts,
    total_mst = sum(msts),
    inv_total_mst = sum(1 / msts[msts > 0])
  ))
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

#' Create Pair Mapping
#'
#' @description
#' Create an item pair mapping for [calculate_nb].
#'
#' @param items Vector of items for the design
#'
#' @details
#' The returned mapping carries two attributes used by [calculate_nb]: `"pairs"`, the distinct item
#'   pairs in sorted form, and `"self_pairs"`, the self-pairs. Together these define the full set of
#'   pairs over which neighbour balance is tabulated, which is what allows pairs that never occur as
#'   neighbours to be counted as zero rather than silently omitted.
#'
#' @importFrom stats setNames
#' @importFrom utils combn
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
  # Built from the combinations, not by splitting on ",", so labels may contain commas
  pairs_r <- paste(combinations[2, ], combinations[1, ], sep = ",")

  pair_mapping <- setNames(
    c(pairs, identical_pairs),
    c(pairs_r, identical_pairs)
  )
  attr(pair_mapping, "pairs") <- pairs
  attr(pair_mapping, "self_pairs") <- identical_pairs
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
          "`",
          row_column,
          "` and `",
          col_column,
          "` effects, so this design ",
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
