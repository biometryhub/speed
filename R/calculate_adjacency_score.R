#' Offsets for a Chebyshev or Manhattan Ring
#'
#' @description
#' Returns the `(dx, dy)` offsets of cells at exact distance `d` from the
#' origin under the chosen ring shape. The origin itself is excluded.
#'
#' @param d Positive integer ring radius.
#' @param ring_type Ring shape: `"manhattan"` (rook moves, diamond ring) or
#'   `"chebyshev"` (king moves, square ring).
#'
#' @return Integer matrix with two columns — column 1 holds `dx`, column 2
#'   holds `dy` — one row per cell on the ring.
#'
#' @keywords internal
ring_offsets <- function(d, ring_type = c("manhattan", "chebyshev")) {
  ring_type <- match.arg(ring_type)
  side <- 2 * d + 1
  dx <- rep(-d:d, each = side)
  dy <- rep(-d:d, times = side)
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
#' Translates `m` by (`dx` rows, `dy` columns), padding cells that fall off
#' the source with `fill`. Positive `dx` shifts rows down; positive `dy`
#' shifts columns right.
#'
#' @param m A matrix.
#' @param dx,dy Integer row and column offsets.
#' @param fill Value used to pad cells with no source (default `NA`).
#'
#' @return A matrix of the same dimensions as `m`.
#'
#' @keywords internal
shift_pad <- function(m, dx, dy, fill = NA) {
  nr <- nrow(m)
  nc <- ncol(m)
  out <- matrix(fill, nr, nc)

  r_src <- if (dx >= 0) seq.int(1, nr - dx) else seq.int(1 - dx, nr)
  c_src <- if (dy >= 0) seq.int(1, nc - dy) else seq.int(1 - dy, nc)
  out[r_src + dx, c_src + dy] <- m[r_src, c_src]
  return(out)
}

#' Per-Cell Weighted Adjacency Score
#'
#' @description
#' For each cell of `design_matrix`, counts neighbours on rings of radius
#' `dists` whose value equals the cell's own, weighted by `weights`.
#' Implemented by stacking shifted copies of the matrix once per offset and
#' reducing across the stack — avoids a per-offset loop when scoring many
#' rings.
#'
#' @param design_matrix Design matrix
#' @param dists A vector of positive integers, ring radii to score over.
#' @param weights Per-ring weights; must align with `dists`.
#' @param ring_type Ring shape: `"manhattan"` (diamond) or `"chebyshev"`
#'   (square). See [ring_offsets()].
#'
#' @return Integer matrix of the same dimensions as `design_matrix`, where
#'   each cell holds its weighted matching-neighbour count.
#'
#' @keywords internal
adjacency_score_vec <- function(design_matrix,
                                dists = c(1, 2),
                                weights = c(1, 2),
                                ring_type = c("manhattan", "chebyshev")) {
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

  matches <- sweep(shifted_stack, c(1, 2), design_matrix, FUN = "==")
  matches[is.na(matches)] <- FALSE

  weighted <- sweep(matches, 3, offset_weights, FUN = "*")
  score <- rowSums(weighted, dims = 2)
  return(score)
}

#' Calculate Adjacency Score for Design
#'
#' @description
#' Counts adjacent plots — immediate horizontal and vertical neighbours —
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
#' @param layout_df A data frame containing the design.
#' @param swap Column name of the treatments to be scored.
#' @param row_column Name of the column representing rows (default `"row"`).
#' @param col_column Name of the column representing columns
#'   (default `"col"`).
#' @param ring_dists Positive integer ring radii to score over (default
#'   `1`, i.e. only the immediate neighbourhood).
#' @param ring_weights Per-ring weights aligned with `ring_dists` (default
#'   `1`).
#' @param ring_type Ring shape: `"manhattan"` (default; rook neighbours,
#'   diamond ring) or `"chebyshev"` (king neighbours, square ring). See
#'   [ring_offsets()].
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
#' @seealso [adjacency_score_vec()]
#'
#' @export
calculate_adjacency_score <- function(layout_df,
                                      swap,
                                      row_column = "row",
                                      col_column = "col",
                                      ring_dists = 1,
                                      ring_weights = 1,
                                      ring_type = c("manhattan", "chebyshev")) {
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
    ring_type = ring_type
  )
  return(sum(per_cell) / 2)
}
