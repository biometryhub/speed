#' Objective Function with a Concurrence Term
#'
#' @description
#' Extends [objective_function()] with a weighted concurrence term that pushes
#' all pairs of treatments toward the same amount of direct comparison. [speed()]
#' uses it in place of [objective_function()] when `concurrence_factors` is given.
#'
#' @details
#' Each term of expanded `concurrence_factors` splits the plots into groups. All
#' pairs of treatments are counted per group with larger weights for smaller
#' groups, then the variance of the weighted count across pairs is penalised.
#'
#' Only the finest terms are used. For example, if complete rows are nested in
#' a block, `row` is considered finer, then `~ block + row` scores only rows.
#' Terms holding one plot like `row:col` are dropped.
#'
#' @inheritParams objective_function
#' @param conc_weight Weight for the concurrence term (default: 1).
#' @param concurrence_factors A one-sided formula of spatial factors whose terms
#'   define the concurrence groups.
#'
#' @return A named list with `score` (numeric, lower is better), `components`,
#'   `state`, and the cached `connectivity_terms` and `n_pairs`.
#'
#' @seealso [objective_function()], [calculate_efficiency_factor()]
#'
#' @examples
#' df <- initialise_design_df(1:8, 6, 4, 2, 4)
#' objective_function_connectivity(
#'   df, "treatment", c("row", "col"), concurrence_factors = ~ block + row
#' )
#'
#' @export
objective_function_connectivity <- function(
  layout_df,
  swap,
  spatial_cols,
  adj_weight = 1,
  bal_weight = 1,
  conc_weight = 1,
  concurrence_factors = NULL,
  current_score_obj = NULL,
  swapped_items = NULL,
  row_column = "row",
  col_column = "col",
  ...
) {
  items <- layout_df[[swap]]
  if (!is.factor(items)) {
    items <- as.factor(items)
  }

  # Verify and resolve groupings once
  groupings <- current_score_obj$connectivity_terms
  n_pairs <- current_score_obj$n_pairs
  if (is.null(groupings)) {
    .verify_concurrence_factors(layout_df, concurrence_factors)
    groupings <- resolve_connectivity_terms(layout_df, concurrence_factors)
    n_items <- nlevels(items)
    n_pairs <- n_items * (n_items - 1) / 2
  }

  # Adjacency and balance
  base <- objective_function(
    layout_df,
    swap,
    spatial_cols,
    adj_weight = adj_weight,
    bal_weight = bal_weight,
    row_column = row_column,
    col_column = col_column,
    current_score_obj = current_score_obj,
    swapped_items = swapped_items,
    ...
  )
  state <- base$state

  conc_component <- 0
  if (conc_weight != 0 && length(groupings) > 0) {
    prev <- current_score_obj$state
    codes <- as.integer(items)
    is_partial <- !is.null(prev$codes) && !is.null(swapped_items)
    rescored <- which(!is_partial | levels(items) %in% swapped_items)
    item_row <- integer(nlevels(items))
    item_row[rescored] <- seq_along(rescored)
    rescored_plots <- which(item_row[codes] > 0L)

    state$codes <- codes
    state$concurrence <- weighted_concurrence_sums(
      groupings,
      n_pairs,
      codes,
      prev$codes,
      rescored_plots,
      item_row,
      rescored,
      if (is_partial) prev$concurrence
    )
    conc_component <- conc_weight * state$concurrence$score
  }

  return(list(
    score = round(base$score + conc_component, 10),
    components = c(base$components, concurrence = conc_component),
    state = state,
    connectivity_terms = groupings,
    n_pairs = n_pairs
  ))
}

#' Verify concurrence factors
#'
#' @description
#' Checks `concurrence_factors` is a one-sided formula of columns in `layout_df`.
#'
#' @inheritParams objective_function_connectivity
#'
#' @keywords internal
.verify_concurrence_factors <- function(layout_df, concurrence_factors) {
  if (is.null(concurrence_factors)) {
    stop(
      "`concurrence_factors` is required for `objective_function_connectivity`",
      call. = FALSE
    )
  }

  verify_one_sided_formula(concurrence_factors)
  for (col in all.vars(concurrence_factors)) {
    verify_column_exists(col, layout_df, "concurrence factor")
  }

  return(invisible(NULL))
}

#' Resolve the Concurrence Groupings
#'
#' @description
#' Caches the codes, sizes, weights, and member plots of the finest groupings.
#'
#' @inheritParams objective_function_connectivity
#'
#' @return A list of concurrence groupings.
#'
#' @keywords internal
resolve_connectivity_terms <- function(layout_df, concurrence_factors) {
  terms <- attr(stats::terms(concurrence_factors), "term.labels")
  groups <- list()

  for (term in terms) {
    factors <- strsplit(term, ":", fixed = TRUE)[[1]]
    group <- as.factor(interaction(layout_df[factors], drop = TRUE))
    # drop groups of single plots
    if (max(tabulate(group)) < 2) {
      next
    }
    groups[[term]] <- group
  }

  concurrence_groups <- lapply(finest_groupings(groups), function(group) {
    codes <- as.integer(group)
    sizes <- tabulate(codes, nlevels(group))
    return(list(
      plot_groups = codes,
      n_groups = nlevels(group),
      weights = 1 / sizes,
      sizes = sizes,
      # plots by group, group i is members[group_starts[i] + 0:(sizes[i] - 1)]
      members = order(codes, na.last = NA),
      group_starts = cumsum(c(1L, sizes[-length(sizes)]))
    ))
  })

  return(concurrence_groups)
}

#' Drop Coarser Groupings
#'
#' @description
#' A grouping is coarser than another when two or more of the other's groups fit
#' within one of its groups. The first is kept for identical groupings.
#'
#' @param groups A named list of factors over plots.
#'
#' @return `groups` without the coarser groupings.
#'
#' @keywords internal
finest_groupings <- function(groups) {
  n_levels <- vapply(groups, nlevels, integer(1))
  keep <- rep(TRUE, length(groups))
  for (i in seq_along(groups)) {
    for (j in seq_along(groups)[-i]) {
      if (j > i && n_levels[[j]] <= n_levels[[i]]) {
        next
      }

      # check if groups[[j]] fits in groups[[i]]
      joint <- interaction(groups[[j]], groups[[i]], drop = TRUE)
      if (nlevels(joint) == n_levels[[j]]) {
        keep[i] <- FALSE
        break
      }
    }
  }

  return(groups[keep])
}

#' Rows of the Weighted Concurrence Matrix
#'
#' @description
#' Weighted concurrence rows of the rescored items for each element of
#' `codes_list`, built from the groups their plots fall in.
#'
#' @details
#' Items A-D in two blocks of three plots, rescoring A and D:
#'
#' ```r
#' g <- resolve_connectivity_terms(
#'   data.frame(block = rep(1:2, each = 3)), ~block
#' )
#' codes <- c(1L, 2L, 3L, 1L, 2L, 4L) # A B C | A B D
#' W <- concurrence_rows(
#'   g, list(codes), rescored_plots = c(1, 4, 6), item_row = c(1, 0, 0, 2)
#' )
#' W
#' #> [[1]]
#' #>           [,1]      [,2]      [,3]      [,4]
#' #> [1,] 0.6666667 0.6666667 0.3333333 0.3333333
#' #> [2,] 0.3333333 0.3333333 0.0000000 0.3333333
#' ```
#'
#' Row 1 is A and row 2 is D; e.g. A and B share both blocks, so W\[A, B\] =
#' 1/3 + 1/3. W[1, 1] (A, A) and W[2, 4] (D, D) are omitted later.
#'
#' @param rescored_plots Plots holding a rescored item.
#' @param item_row Row of output corresponding to rescored items, 0 if not rescored.
#' @param groupings Concurrence groupings.
#' @param codes_list A list of treatment codes per plot.
#'
#' @return A list of row matrices, one per element of `codes_list`.
#'
#' @keywords internal
concurrence_rows <- function(groupings, codes_list, rescored_plots, item_row) {
  n_items <- length(item_row)
  n_rescored <- max(0, item_row)
  # rescored row a of the item at each of `rescored_plots`
  rows <- lapply(
    codes_list,
    function(codes) return(item_row[codes[rescored_plots]])
  )
  w_rows <- rep(list(matrix(0, n_rescored, n_items)), length(codes_list))
  # e.g. following @details example: b1 = A B C, b2 = A B D, rescoring A and D
  # codes_list = list(c(1, 2, 3, 1, 2, 4)), item_row = c(1, 0, 0, 2)
  # rescored_plots = c(1, 4, 6), rows = c(1, 1, 2), groupings = list(block = g)
  # g = list(
  #   plot_groups = c(1, 1, 1, 2, 2, 2), n_groups = 2, weights = c(1/3, 1/3),
  #   sizes = c(3, 3), members = 1:6, group_starts = c(1, 4)
  # )
  # TODO: outer loop could be vectorized with ~5-10% gain - need more testing
  for (g in groupings) {
    # e.g. c(1, 2, 2)
    plot_groups <- g$plot_groups[rescored_plots]
    # groups holding a rescored plot, e.g. c(1, 2)
    groups <- unique(plot_groups)
    groups <- groups[!is.na(groups)]
    n_groups <- length(groups)
    # for correct indexing, e.g. c(0, 2, 2)
    offset <- n_rescored * (match(plot_groups, groups) - 1)
    sizes <- g$sizes[groups]
    # e.g. members = 1:6, member_groups = c(1, 1, 1, 2, 2, 2)
    members <- g$members[sequence(sizes, from = g$group_starts[groups])]
    member_groups <- rep.int(seq_len(n_groups), sizes)
    weights <- g$weights[groups]
    for (k in seq_along(codes_list)) {
      # e.g. tabulate(c(1, 3, 4), 4) -> rbind(A = c(1, 1), D = c(0, 1))
      # 1 A in group 1, 1 A and D in group 2
      rescored_counts <- tabulate(rows[[k]] + offset, n_rescored * n_groups)
      dim(rescored_counts) <- c(n_rescored, n_groups)
      # e.g. rbind(b1 = c(1, 1, 1, 0), b2 = c(1, 1, 0, 1)), columns are A-D
      member_counts <- tabulate(
        member_groups + n_groups * (codes_list[[k]][members] - 1),
        n_groups * n_items
      )
      dim(member_counts) <- c(n_groups, n_items)
      # e.g. b1 b2           A B C D             A B C D
      #   A [ 1  1 ] %*% b1 [1 1 1 0] / 3  =  A [2 2 1 1] / 3
      #   D [ 0  1 ]     b2 [1 1 0 1]         D [1 1 0 1]
      w_rows[[k]] <- w_rows[[k]] + rescored_counts %*% (member_counts * weights)
    }
  }
  return(w_rows)
}

#' Incremental Weighted Concurrence Score
#'
#' @description
#' Variance of the weighted concurrence across treatment pairs.
#'
#' @inheritParams concurrence_rows
#' @param codes,prev_codes Treatment codes per plot, now and before the swaps.
#' @param n_pairs Number of unordered treatment pairs.
#' @param rescored Codes of the rescored items.
#' @param prev The previous concurrence state.
#'
#' @return A list with `score`, `sum_pairs` and `sum_pairs_sq`.
#'
#' @keywords internal
weighted_concurrence_sums <- function(
  groupings,
  n_pairs,
  codes,
  prev_codes,
  rescored_plots,
  item_row,
  rescored,
  prev
) {
  is_partial <- !is.null(prev)
  codes_list <- if (is_partial) list(codes, prev_codes) else list(codes)
  w_rows <- concurrence_rows(groupings, codes_list, rescored_plots, item_row)
  new_rows <- w_rows[[1]]

  mask <- rescored_mask(rescored, ncol(new_rows))
  if (is_partial) {
    old_rows <- w_rows[[2]]
    weighted_diff <- mask * (new_rows - old_rows)
    sum_pairs <- prev$sum_pairs + sum(weighted_diff)
    sum_pairs_sq <- prev$sum_pairs_sq +
      sum(weighted_diff * (new_rows + old_rows))
  } else {
    weighted_new <- mask * new_rows
    sum_pairs <- sum(weighted_new)
    sum_pairs_sq <- sum(weighted_new * new_rows)
  }

  # var = (sum(x^2) - sum(x)^2 / n) / (n - 1)
  score <- 0
  if (n_pairs >= 2) {
    score <- (sum_pairs_sq - sum_pairs^2 / n_pairs) / (n_pairs - 1)
  }

  return(list(
    score = score,
    sum_pairs = sum_pairs,
    sum_pairs_sq = sum_pairs_sq
  ))
}

#' A Mask for Pairs With a Rescored Item
#'
#' @description
#' A mask over the `rescored` rows so that each off-diagonal pair with at least
#' one item in `rescored` is counted once.
#'
#' @param rescored Codes of the rescored items.
#' @param n_items Number of items.
#'
#' @return A `length(rescored)` by `n_items` matrix: 0 for diagonal, 1/2 for
#'   rescored pairs (appear twice), 1 otherwise.
#'
#' @keywords internal
rescored_mask <- function(rescored, n_items) {
  n_rescored <- length(rescored)
  mask <- matrix(1, n_rescored, n_items)
  mask[, rescored] <- 0.5
  mask[cbind(seq_len(n_rescored), rescored)] <- 0
  return(mask)
}

#' Connectivity Objective for a Level
#'
#' @description
#' Binds `concurrence_factors` and `conc_weight` into
#' [objective_function_connectivity()], so it can stand in as a level's
#' `obj_function`.
#'
#' @inheritParams objective_function_connectivity
#'
#' @return An objective function.
#'
#' @keywords internal
.connectivity_objective <- function(concurrence_factors, conc_weight = 1) {
  fn <- function(...) {
    return(objective_function_connectivity(
      ...,
      conc_weight = conc_weight,
      concurrence_factors = concurrence_factors
    ))
  }
  attr(fn, "objective_name") <- "objective_function_connectivity"
  return(fn)
}
