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

#' Generate a Neighbour Design by Swapping Treatments
#'
#' @param design Data frame containing the current design
#' @param swap Column name of the treatment to swap
#' @param swap_within Column name defining groups within which to swap treatments
#' @param swap_count Number of swaps to perform
#' @param swap_all_blocks Whether to perform swaps in all blocks or just one
#'
#' @return A data frame with the updated design after swapping
#'
#' @keywords internal
generate_neighbour <- function(design,
                               swap,
                               swap_within,
                               swap_count = getOption("speed.swap_count", 1),
                               swap_all_blocks = getOption("speed.swap_all_blocks", FALSE)) {
    new_design <- design

    # Get unique blocks
    blocks <- unique(design[[swap_within]])

    if (swap_all_blocks) {
        # Swap in all blocks
        blocks_to_swap <- blocks
    } else {
        # Pick a random block
        blocks_to_swap <- sample(blocks, 1)
    }

    # Perform swaps in selected blocks
    for (block in blocks_to_swap) {
        # Get indices of plots in this block
        block_indices <- which(design[[swap_within]] == block & !is.na(design[[swap]]))

        if (length(block_indices) >= 2) {  # Need at least 2 plots to swap
            for (i in 1:swap_count) {
                # Select two random plots in this block
                swap_pair <- sample(block_indices, 2)

                # Swap treatments
                temp <- new_design[[swap]][swap_pair[1]]
                new_design[[swap]][swap_pair[1]] <- new_design[[swap]][swap_pair[2]]
                new_design[[swap]][swap_pair[2]] <- temp
            }
        }
    }

    return(new_design)
}


#' Initialize Design Data Frame
#'
#' @description
#' Initialize a design data frame with or without blocking.
#'
#' @param items Items to be placed in the design
#' @param nrows Number of rows in the design
#' @param ncols Number of columns in the design
#' @param nrows_block Number of rows in each block
#' @param ncols_block Number of columns in each block
#'
#' @return A data frame containing the design
#'
#' @examples
#' initialize_design_df(
#'   items = c(1, 2, 2, 1, 3, 3, 1, 3, 3),
#'   nrows = 3,
#'   ncols = 3
#' )
#'
#' @export
initialize_design_df <- function(items, nrows, ncols, nrows_block = NULL, ncols_block = NULL) {
  .verify_initialize_design_df(items, nrows, ncols, nrows_block, ncols_block)
  rows <- rep(1:nrows, ncols)
  cols <- rep(1:ncols, each = nrows)
  df <- data.frame(
    row = rows,
    col = cols,
    treatment = items
  )

  if (!is.null(nrows_block)) {
    nblocks_row <- nrows / nrows_block
    nblocks_col <- ncols / ncols_block

    df$row_block <- rep(1:nblocks_row, ncols, each = nrows_block)
    df$col_block <- rep(1:nblocks_col, each = nrows * ncols_block)
    df$block <- as.numeric(df$row_block) + nblocks_row * (as.numeric(df$col_block) - 1)
  }
  return(df)
}

.verify_initialize_design_df <- function(items, nrows, ncols, nrows_block, block_ncols) {
  verify_positive_whole_number(nrows, ncols)

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
