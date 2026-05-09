#' Offsets for a Chebyshev or Manhattan Ring
#'
#' @description
#' Returns the `(dx, dy)` offsets of cells at exact distance `d` from the
#' origin under the chosen ring shape. The origin itself is excluded.
#'
#' @inheritParams calculate_adjacency_score
#' @param d Positive integer ring radius.
#'
#' @return Integer matrix with two columns - column 1 holds `dx`, column 2
#'   holds `dy` - one row per cell on the ring.
#'
#' @keywords internal
ring_offsets <- function(d, ring_type = c("manhattan", "chebyshev")) {
  ring_type <- match.arg(ring_type)
  side <- 2 * d + 1
  dx <- rep(-d:d, times = side)
  dy <- rep(-d:d, each = side)
  on_ring <- if (ring_type == "chebyshev") {
    pmax(abs(dx), abs(dy)) == d
  } else {
    abs(dx) + abs(dy) == d
  }
  return(cbind(dx[on_ring], dy[on_ring]))
}

#' Shift a Matrix With NA Padding
#'
#' @description
#' Translates `m` by (`dx` columns, `dy` rows), padding cells that fall off
#' the source with `fill`. Positive `dx` shifts columns right; positive
#' `dy` shifts rows down.
#'
#' @param m A matrix.
#' @param dx,dy Integer column (x-axis) and row (y-axis) offsets.
#' @param fill Value used to pad cells with no source (default `NA`).
#'
#' @return A matrix of the same dimensions as `m`.
#'
#' @keywords internal
shift_pad <- function(m, dx, dy, fill = NA) {
  nr <- nrow(m)
  nc <- ncol(m)
  out <- matrix(fill, nr, nc)
  # entire source falls off the grid when the shift meets or exceeds the
  # corresponding dimension; `seq.int(1, nr - dy)` would otherwise produce
  # a descending sequence and out-of-bounds indices
  if (abs(dx) >= nc || abs(dy) >= nr) {
    return(out)
  }

  r_src <- if (dy >= 0) seq.int(1, nr - dy) else seq.int(1 - dy, nr)
  c_src <- if (dx >= 0) seq.int(1, nc - dx) else seq.int(1 - dx, nc)
  out[r_src + dy, c_src + dx] <- m[r_src, c_src]
  return(out)
}

#' Validate and Flatten a Relationship Matrix for Lookup
#'
#' @description
#' Validates `relationship` and packs it into a flat numeric vector plus
#' row/column level vectors that [adjacency_score_vec()] can index into in
#' constant time per cell. [speed()] calls this once before the SA loop so
#' the per-iteration cost stays in the lookup itself. Direct callers of
#' [calculate_adjacency_score()] must wrap their matrix with this function
#' before passing - the score functions consume the prepped form only.
#'
#' @param relationship A numeric matrix with rownames and colnames covering
#'   every treatment value to be scored.
#' @param treatments Optional character vector of treatment values that
#'   `relationship` must cover. Skipped when `NULL`.
#'
#' @return A list with `flat` (column-major flattened matrix), `row_levels`,
#'   `col_levels`, and `n_row`.
#'
#' @export
prep_relationship <- function(relationship, treatments = NULL) {
  if (!is.matrix(relationship) || !is.numeric(relationship)) {
    stop("`relationship` must be a numeric matrix.", call. = FALSE)
  }

  if (is.null(rownames(relationship)) || is.null(colnames(relationship))) {
    stop(
      "`relationship` must have rownames and colnames covering all ",
      "treatments to be scored.",
      call. = FALSE
    )
  }

  if (!is.null(treatments)) {
    treatments <- as.character(treatments)
    treatments <- unique(treatments[!is.na(treatments)])
    missing <- unique(c(
      setdiff(treatments, rownames(relationship)),
      setdiff(treatments, colnames(relationship))
    ))

    if (length(missing)) {
      stop(
        "`relationship` is missing entries for treatments: ",
        paste(missing, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  return(list(
    flat = as.vector(relationship),
    row_levels = rownames(relationship),
    col_levels = colnames(relationship),
    n_row = nrow(relationship)
  ))
}

#' Per-Cell Weighted Adjacency Score
#'
#' @description
#' For each cell of `design_matrix`, counts neighbours on rings of radius
#' `dists` whose value equals the cell's own, weighted by `weights`.
#' Implemented by stacking shifted copies of the matrix once per offset and
#' reducing across the stack - avoids a per-offset loop when scoring many
#' rings.
#'
#' @inheritParams calculate_adjacency_score
#' @param design_matrix Design matrix
#' @param dists A vector of positive integers, ring radii to score over.
#' @param weights Per-ring weights; must align with `dists`.
#'
#' @return Matrix of the same dimensions as `design_matrix`, where
#'   each cell holds its weighted matching-neighbour count.
#'
#' @keywords internal
adjacency_score_vec <- function(design_matrix,
                                dists = c(1, 2),
                                weights = c(1, 2),
                                ring_type = c("manhattan", "chebyshev"),
                                relationship = NULL) {
  ring_type <- match.arg(ring_type)
  stopifnot(length(dists) == length(weights))
  nr <- nrow(design_matrix)
  nc <- ncol(design_matrix)

  ring_list <- lapply(dists, ring_offsets, ring_type = ring_type)
  ring_sizes <- vapply(ring_list, nrow, integer(1))
  offsets <- do.call(rbind, ring_list)
  offset_weights <- inverse.rle(list(values = weights, lengths = ring_sizes))
  n_offsets <- nrow(offsets)

  shifted_stack <- simplify2array(
    lapply(seq_len(n_offsets), function(k) {
      shift_pad(design_matrix, offsets[k, 1], offsets[k, 2], fill = NA)
    }),
    higher = TRUE
  )
  # force 3D even when n_offsets == 1, where simplify2array returns a matrix
  dim(shifted_stack) <- c(nr, nc, n_offsets)

  if (is.null(relationship)) {
    pair_values <- sweep(shifted_stack, c(1, 2), design_matrix, FUN = "==")
    pair_values[is.na(pair_values)] <- FALSE
  } else {
    orig_idx <- match(as.character(design_matrix), relationship$row_levels)
    shifted_idx <- match(as.character(shifted_stack), relationship$col_levels)
    flat_idx <- rep(orig_idx, n_offsets) + relationship$n_row * (shifted_idx - 1L)
    pair_values <- relationship$flat[flat_idx]
    pair_values[is.na(pair_values)] <- 0
    dim(pair_values) <- c(nr, nc, n_offsets)
  }

  weighted <- sweep(pair_values, 3, offset_weights, FUN = "*")
  score <- rowSums(weighted, dims = 2)
  return(score)
}

#' Calculate Adjacency Score for Design
#'
#' @description
#' Counts adjacent plots - immediate horizontal and vertical neighbours -
#' that share the same treatment. Lower scores indicate better separation.
#'
#' Internally this is a thin wrapper around [adjacency_score_vec()]. With
#' the defaults (`ring_dists = 1`, `ring_weights = 1`,
#' `ring_type = "manhattan"`) it scores the immediate row/column
#' neighbourhood the simulated-annealing loop in [speed()] minimises.
#' Pass longer `ring_dists` / `ring_weights` to also penalise like-treatment
#' matches at larger ring radii. The per-cell score is summed and halved
#' because each adjacency is counted once from each endpoint.
#'
#' Pass a `relationship` matrix to score neighbour pairs by a graded
#' similarity (e.g. genetic relatedness) instead of a strict identity match.
#'
#' @param layout_df A data frame containing the design.
#' @param swap Column name of the treatments to be scored.
#' @param row_column Name of the column representing rows (default `"row"`).
#' @param col_column Name of the column representing columns
#'   (default `"col"`).
#' @param ring_dists A vector of positive integers, ring radii to score over.
#'   (default `1`, i.e. only the immediate neighbourhood).
#' @param ring_weights Per-ring weights aligned with `ring_dists` (default
#'   `1`).
#' @param ring_type Ring shape: `"manhattan"` (default; diamond ring)
#'   or `"chebyshev"` (square ring). See [ring_offsets()].
#' @param relationship Optional pairwise-relationship lookup produced by
#'   [prep_relationship()]. When supplied, each neighbour pair contributes
#'   `relationship[cell, neighbour]` rather than `1` for matches and `0`
#'   otherwise. NA-padded cells off the design edge contribute `0`. Defaults
#'   to `NULL`, which keeps the strict identity match. Pass the raw matrix
#'   through `prep_relationship()` first; the score functions consume only
#'   the prepped form.
#'
#' @return A non-negative numeric value: the number of like-treatment edges
#'   in the row/column adjacency graph.
#'
#' @examples
#' # Example 1: design with no like-treatment adjacencies
#' design_no_adj <- data.frame(
#'   row = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   col = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   treatment = c("A", "B", "A", "B", "A", "B", "A", "B", "A")
#' )
#' calculate_adjacency_score(design_no_adj, "treatment") # 0
#'
#' # Example 2: design with adjacencies
#' design_with_adj <- data.frame(
#'   row = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   col = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   treatment = c("A", "A", "A", "B", "B", "B", "A", "A", "A")
#' )
#' calculate_adjacency_score(design_with_adj, "treatment") # 6
#'
#' # Example 3: graded relationship between A and B
#' rel <- prep_relationship(matrix(
#'   c(1, 0.3, 0.3, 1),
#'   nrow = 2,
#'   dimnames = list(c("A", "B"), c("A", "B"))
#' ))
#' calculate_adjacency_score(design_no_adj, "treatment", relationship = rel)
#' # 3.6: each of the 12 A-B edges contributes 0.3
#'
#' @seealso [adjacency_score_vec()]
#'
#' @export
calculate_adjacency_score <- function(layout_df,
                                      swap,
                                      row_column = "row",
                                      col_column = "col",
                                      ring_dists = 1,
                                      ring_weights = 1,
                                      ring_type = c("manhattan", "chebyshev"),
                                      relationship = NULL) {
  ring_type <- match.arg(ring_type)

  design_matrix <- matrix(
    layout_df[[swap]],
    nrow = max(as_numeric_factor(layout_df[[row_column]]), na.rm = TRUE),
    ncol = max(as_numeric_factor(layout_df[[col_column]]), na.rm = TRUE),
    byrow = TRUE
  )

  per_cell <- adjacency_score_vec(
    design_matrix,
    dists = ring_dists,
    weights = ring_weights,
    ring_type = ring_type,
    relationship = relationship
  )
  return(sum(per_cell) / 2)
}
