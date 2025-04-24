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

# TODO: a function to generate design df
# row <- factor(rep(1:20, each = 20))
# col <- factor(rep(1:20, 20))
# block <- factor(rep(1:10, each = 40))
# treats <- rep(factor(paste("V", 1:40, sep = "")), 10)
# dat <- data.frame(row = row, col = col, treat = treats, row_block = block)
# dat <- dat[order(dat$col, dat$row), ]
# dat$col_block <- factor(rep(1:10, each = 40))
