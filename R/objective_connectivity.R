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
    plots <- which(item_row[codes] > 0L)

    state$codes <- codes
    state$concurrence <- weighted_concurrence_sums(
      groupings,
      n_pairs,
      codes,
      prev$codes,
      plots,
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
#' Caches the codes, sizes, weights and members of the finest groupings of
#' `concurrence_factors`.
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
    # drop singleton groups
    if (max(tabulate(group)) < 2) {
      next
    }
    groups[[term]] <- group
  }

  concurrence_groups <- lapply(finest_groupings(groups), function(group) {
    codes <- as.integer(group)
    size <- tabulate(codes, nlevels(group))
    return(list(
      group = codes,
      n_groups = nlevels(group),
      weight = 1 / size,
      size = size,
      # plots ordered by group, group g at start[g] + 0:(size[g] - 1)
      members = order(codes, na.last = NA),
      start = cumsum(c(1L, size[-length(size)]))
    ))
  })

  return(concurrence_groups)
}

#' Drop Groupings Coarser Than Another
#'
#' @description
#' A grouping is coarser than another when each of the other's groups lies
#' within one of its groups. If groupings are identical, the first is kept.
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
      if (!(n_levels[[j]] > n_levels[[i]] || j < i)) {
        next
      }
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
#' @param plots Plots holding a rescored item.
#' @param item_row Row of each item among the rescored ones, 0 if not rescored.
#' @param groupings Cached concurrence groupings.
#' @param codes_list A list of treatment codes per plot.
#'
#' @return A list of row matrices, one per element of `codes_list`.
#'
#' @keywords internal
concurrence_rows <- function(groupings, codes_list, plots, item_row) {
  n_items <- length(item_row)
  n_rescored <- max(0L, item_row)
  rows <- lapply(codes_list, function(codes) return(item_row[codes[plots]]))
  w_rows <- lapply(codes_list, function(codes) return(0))
  for (g in groupings) {
    plot_group <- g$group[plots]
    # touched groups in increasing order; NA groups drop out of tabulate()
    touched <- which(tabulate(plot_group, g$n_groups) > 0L)
    n_touched <- length(touched)
    pos <- integer(g$n_groups)
    pos[touched] <- seq_len(n_touched)
    inc_offset <- n_rescored * (pos[plot_group] - 1L)
    size <- g$size[touched]
    member <- g$members[sequence(size, from = g$start[touched])]
    member_group <- rep.int(seq_len(n_touched), size)
    weight <- g$weight[touched]
    for (k in seq_along(codes_list)) {
      inc <- tabulate(rows[[k]] + inc_offset, n_rescored * n_touched)
      dim(inc) <- c(n_rescored, n_touched)
      counts <- tabulate(
        member_group + n_touched * (codes_list[[k]][member] - 1L),
        n_touched * n_items
      )
      dim(counts) <- c(n_touched, n_items)
      w_rows[[k]] <- w_rows[[k]] + inc %*% (counts * weight)
    }
  }
  return(w_rows)
}

#' Incremental Weighted Concurrence Score
#'
#' @description
#' Variance of the weighted concurrence across treatment pairs, from running
#' sums updated with the rescored items' old and new rows.
#'
#' @inheritParams concurrence_rows
#' @param codes,prev_codes Treatment codes per plot, now and before the swaps.
#' @param n_pairs Number of unordered treatment pairs.
#' @param rescored Codes of the rescored items.
#' @param prev The previous concurrence state.
#'
#' @return A list with `score`, `sum_off` and `sum_off_sq`.
#'
#' @keywords internal
weighted_concurrence_sums <- function(
  groupings,
  n_pairs,
  codes,
  prev_codes,
  plots,
  item_row,
  rescored,
  prev
) {
  is_partial <- !is.null(prev)
  codes_list <- if (is_partial) list(codes, prev_codes) else list(codes)
  w_rows <- concurrence_rows(groupings, codes_list, plots, item_row)
  new_rows <- w_rows[[1]]

  # swap out the pairs touching rescored items, old W for new
  old_sum <- old_sum_sq <- 0
  if (is_partial) {
    old_rows <- w_rows[[2]]
    old_sum <- touched_sum(old_rows, rescored)
    old_sum_sq <- touched_sum(old_rows * old_rows, rescored)
  }
  sum_off <- (prev$sum_off %||% 0) - old_sum + touched_sum(new_rows, rescored)
  sum_off_sq <- (prev$sum_off_sq %||% 0) -
    old_sum_sq +
    touched_sum(new_rows * new_rows, rescored)

  # var = (sum(x^2) - sum(x)^2 / n) / (n - 1)
  score <- 0
  if (n_pairs >= 2) {
    score <- (sum_off_sq - sum_off^2 / n_pairs) / (n_pairs - 1)
  }

  return(list(score = score, sum_off = sum_off, sum_off_sq = sum_off_sq))
}

#' Sum Over Off-Diagonal Pairs Touching Some Rows
#'
#' @description
#' Sums the off-diagonal pairs with at least one end in `rescored`, each counted
#' once, from the `rescored` rows of a symmetric matrix.
#'
#' @param rows The `rescored` rows of a symmetric matrix.
#' @param rescored Row (and column) indices of `rows` in that matrix.
#'
#' @return A single number.
#'
#' @keywords internal
touched_sum <- function(rows, rescored) {
  inner <- rows[, rescored, drop = FALSE]
  diag_sum <- sum(diag(inner))
  # pairs within rescored appear twice in `rows`
  return(sum(rows) - diag_sum - (sum(inner) - diag_sum) / 2)
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
