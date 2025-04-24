#' Initialize Design Matrix
#'
#' @description
#' Initialize a design matrix based on the treatment matrix and swap matrix.
#'
#' @param treatment_matrix A matrix of treatments
#' @param swap_matrix A matrix that constrains swap boundaries
#'
#' @keywords internal
initialize_design_matrix <- function(treatment_matrix, swap_matrix) {
  nrows <- nrow(treatment_matrix)
  ncols <- ncol(treatment_matrix)
  design_matrix <- matrix(NA, nrow = nrows, ncol = ncols)

  if (is.null(swap_matrix)) {
    design_matrix[] <- sample(as.vector(treatment_matrix))
  } else {
    for (level in unique(as.vector(swap_matrix))) {
      mask <- which(swap_matrix == level, arr.ind = TRUE)
      vals <- treatment_matrix[mask]
      design_matrix[mask] <- sample(vals)
    }
  }

  return(design_matrix)
}

#' Generate Neighbor Design
#'
#' @description
#' Generates a neighbor design based on the current design and swap parameters.
#'
#' @param design_matrix A design matrix
#' @param swap_matrix A matrix that constrains swap boundaries
#' @param swap_count Number of treatment swaps per iteration
#' @param swap_all_blocks Logical; if TRUE, performs swaps in all blocks at each iteration
#'
#' @keywords internal
generate_neighbor <- function(design_matrix, swap_matrix, swap_count, swap_all_blocks) {
  new_design <- design_matrix
  swap_levels <- unique(as.vector(swap_matrix))
  if (!swap_all_blocks) swap_levels <- sample(swap_levels, 1)
  for (level in swap_levels) {
    for (i in 1:swap_count) {
      level <- sample(swap_levels, 1)
      positions <- which(swap_matrix == level, arr.ind = TRUE)
      if (nrow(positions) < 2) next

      idx <- sample(seq_len(nrow(positions)), 2)
      pos1 <- positions[idx[1], ]
      pos2 <- positions[idx[2], ]
      tmp <- new_design[pos1[1], pos1[2]]
      new_design[pos1[1], pos1[2]] <- new_design[pos2[1], pos2[2]]
      new_design[pos2[1], pos2[2]] <- tmp
    }
  }
  return(new_design)
}

# TODO: add doc
# currently support only full rep
initialize_design_df <- function(treatments, reps, nrows, ncols, nrows_block = NULL, ncols_block = NULL) {
  # .verify_initialize_design_df(treatments, reps, nrows, ncols, nrows_block, ncols_block)
  rows <- factor(rep(1:nrows, ncols))
  cols <- factor(rep(1:ncols, each = nrows))
  treatments <- factor(rep(treatments, reps))
  df <- data.frame(
    row = rows,
    col = cols,
    treatment = treatments
  )

  if (!is.null(nrows_block)) {
    nblocks_row <- nrows / nrows_block
    nblocks_col <- ncols / ncols_block

    df$row_block <- factor(rep(1:nblocks_row, ncols, each = nrows_block))
    df$col_block <- factor(rep(1:nblocks_col, each = nrows * ncols_block))
    df$block <- factor(as.numeric(df$row_block) + nblocks_row * (as.numeric(df$col_block) - 1))
  }
  return(df)
}
# row <- factor(rep(1:20, each = 20))
# col <- factor(rep(1:20, 20))
# block <- factor(rep(1:10, each = 40))
# treats <- rep(factor(paste("V", 1:40, sep = "")), 10)
# dat <- data.frame(row = row, col = col, treat = treats, row_block = block)
# dat <- dat[order(dat$col, dat$row), ]
# dat$col_block <- factor(rep(1:10, each = 40))

.verify_initialize_design_df <- function(treatments, reps, nrows, ncols, nrows_block, block_ncols) {
  verify_positive_whole_number(reps, nrows, ncols)

  if ((!is.null(nrows_block) && is.null(block_ncols)) || (is.null(nrows_block) && !is.null(block_ncols))) {
    stop("`block_nrows` and `block_ncols` must both be numeric or `NULL`")
  }

  if (!is.null(nrows_block)) {
    verify_positive_whole_number(nrows_block)
    verify_positive_whole_number(block_ncols)

    verify_multiple_of(nrows, nrows_block)
    verify_multiple_of(ncols, block_ncols)
  }
}
