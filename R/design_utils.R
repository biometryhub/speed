#' Generate a Neighbour Design by Swapping Treatments
#'
#' @param design Data frame containing the current design
#' @param swap Column name of the treatment to swap, or named list for hierarchical designs
#' @param swap_within Column name defining groups within which to swap treatments, or named list for hierarchical designs
#' @param level The level of the design to be optimised in the current loop. Relevant for sequential designs. Simple designs pass this as `NULL`.
#' @param swap_count Number of swaps to perform
#' @param swap_all_blocks Whether to perform swaps in all blocks or just one
#'
#' @return A list with the updated design after swapping and information about swapped items
#'
#' @keywords internal
# fmt: skip
generate_neighbour <- function(design,
                               swap,
                               swap_within,
                               level = NULL,
                               swap_count = getOption("speed.swap_count", 1),
                               swap_all_blocks = getOption("speed.swap_all_blocks", FALSE)) {
  # Check if this is a hierarchical design
  is_hierarchical <- is.list(swap) && !is.null(names(swap))

  if (is_hierarchical) {
    return(generate_sequential_neighbour(design, swap, swap_within, level, swap_count, swap_all_blocks))
  } else {
    return(generate_simple_neighbour(design, swap, swap_within, level, swap_count, swap_all_blocks))
  }
}

#' Generate neighbour for simple (non-hierarchical) designs
#' @keywords internal
# fmt: skip
generate_simple_neighbour <- function(design,
                                      swap,
                                      swap_within,
                                      level,
                                      swap_count,
                                      swap_all_blocks) {
  new_design <- design

  # Get unique blocks
  all_blocks <- design[[swap_within]]
  blocks <- levels(all_blocks)

  if (swap_all_blocks) {
    # Swap in all blocks
    blocks_to_swap <- blocks
  } else {
    # Pick a random block
    blocks_to_swap <- sample(blocks, 1)
  }

  swapped_idx <- 1
  swapped_items <- character(2 * swap_count * length(blocks_to_swap))

  # Perform swaps in selected blocks
  for (block in blocks_to_swap) {
    # Get indices of plots in this block
    block_indices <- which(all_blocks == block & !is.na(design[[swap]]))

    if (length(block_indices) >= 2) {
      # Need at least 2 plots to swap
      for (i in 1:swap_count) {
        # Select two random plots in this block
        swap_pair <- sample(block_indices, 2)
        to_be_swapped <- new_design[[swap]][swap_pair]
        if (to_be_swapped[1] == to_be_swapped[2]) {
          no_dupe_filter <- new_design[[swap]][block_indices] != to_be_swapped[1]
          swap_pair[[2]] <- sample(block_indices[no_dupe_filter], 1)
          to_be_swapped[2] <- new_design[[swap]][[swap_pair[[2]]]]
        }

        # Swap treatments
        new_design[[swap]][rev(swap_pair)] <- to_be_swapped
        swapped_items[swapped_idx:(swapped_idx + 1)] <- to_be_swapped

        swapped_idx <- swapped_idx + 2
      }
    }
  }

  return(list(design = new_design, swapped_items = swapped_items))
}

#' Generate neighbour for sequential or hierarchical designs
#' @keywords internal
# fmt: skip
generate_sequential_neighbour <- function(design,
                                          swap,
                                          swap_within,
                                          level,
                                          swap_count,
                                          swap_all_blocks) {
  new_design <- design

  # Get the swap columns for the specified level
  level_swap <- swap[[level]]
  level_swap_within <- swap_within[[level]]

  # Get unique groups for this level
  groups <- levels(design[[level_swap_within]])

  if (swap_all_blocks) {
    # Swap in all groups
    groups_to_swap <- groups
  } else {
    # Pick a random group
    groups_to_swap <- sample(groups, 1)
  }

  swapped_idx <- 1
  swapped_items <- character(2 * swap_count * length(groups_to_swap))

  # Perform swaps in selected groups
  for (group in groups_to_swap) {
    # Get unique treatments within this group
    group_data <- new_design[new_design[[level_swap_within]] == group & !is.na(new_design[[level_swap]]), ]
    group_treatments <- unique(
      new_design[new_design[[level_swap_within]] == group & !is.na(new_design[[level_swap]]), level_swap]
    )

    if (nrow(group_data) >= 2) {
      for (i in 1:swap_count) {
        # Select two random treatments
        swap_pair <- sample(group_treatments, 2)

        # Ensure they're different treatments
        if (swap_pair[1] == swap_pair[2]) {
          different_treatments <- group_treatments[group_treatments != swap_pair[1]]
          if (length(different_treatments) > 0) {
            swap_pair[2] <- sample(different_treatments, 1)
          } else {
            next # Skip this swap if no different treatments available
          }
        }

        # Find all plots with these treatments in this group
        plots_1 <- which(new_design[[level_swap_within]] == group &
          new_design[[level_swap]] == swap_pair[1])
        plots_2 <- which(new_design[[level_swap_within]] == group &
          new_design[[level_swap]] == swap_pair[2])

        # Swap all instances of these treatments
        new_design[[level_swap]][plots_1] <- swap_pair[2]
        new_design[[level_swap]][plots_2] <- swap_pair[1]

        swapped_items[swapped_idx] <- swap_pair[1]
        swapped_items[swapped_idx + 1] <- swap_pair[2]
        swapped_idx <- swapped_idx + 2
      }
    }
  }

  return(list(design = new_design, swapped_items = swapped_items[1:(swapped_idx - 1)]))
}

#' Infer 'row' and 'col' with Patterns
#'
#' @description
#' Infer data frame names with patterns to determine if variations of 'row' and 'col' columns exist.
#'
#' @inheritParams objective_function_signature
#' @param grid_factors A named list specifying grid factors to construct a
#'   matrix for calculating adjacency score, `dim1` for row and `dim2` for
#'   column. (default: `list(dim1 = "row", dim2 = "col")`).
#' @param quiet Logical (default: FALSE). If TRUE, output will be suppressed.
#'
#' @returns A list containing:
#' - **inferred** - Logical; if TRUE, row and column columns were inferred from the data frame
#' - **row** - Name of the row column
#' - **col** - Name of the column column
#'
#' @keywords internal
# fmt: skip
infer_row_col <- function(layout_df, grid_factors = list(dim1 = "row", dim2 = "col"), quiet = FALSE) {
  if (grid_factors$dim1 %in% names(layout_df) && grid_factors$dim2 %in% names(layout_df)) {
    row_col <- grid_factors$dim1
    col_col <- grid_factors$dim2
    if (!quiet) {
      message(row_col, " and ", col_col, " are used as row and column, respectively.")
    }
    return(list(inferred = TRUE, row = row_col, col = col_col))
  }

  row_pattern <- "(?i)^row(s|)$"
  col_pattern <- "(?i)^(col(umn|)|range)(s|)$"

  row_col <- grep(row_pattern, names(layout_df), value = TRUE)[1]
  col_col <- grep(col_pattern, names(layout_df), value = TRUE)[1]
  if (is.na(row_col) || is.na(col_col)) {
    if (is.na(row_col)) {
      warning(
        "Cannot infer row in the design data frame. speed.adj_weight is set to 0 for this call. If this is not",
        " intended, provide `grid_factors` argument.",
        call. = FALSE
      )
    } else {
      warning(
        "Cannot infer column in the design data frame. speed.adj_weight is set to 0 for this call. If this is",
        " not intended, provide `grid_factors` argument.",
        call. = FALSE
      )
    }

    return(list(inferred = FALSE))
  }
  if (!quiet) {
    message(row_col, " and ", col_col, " are used as row and column, respectively.")
  }
  return(list(inferred = TRUE, row = row_col, col = col_col))
}

#' Initialise Design Data Frame
#'
#' @description
#' Initialise a design data frame with or without blocking.
#'
#' @param items Items to be placed in the design. Either a single numeric value (the number of
#' equally replicated items), or a vector of items.
#' @param nrows Number of rows in the design
#' @param ncols Number of columns in the design
#' @param block_nrows Number of rows in each block
#' @param block_ncols Number of columns in each block
#'
#' @return A data frame containing the design
#'
#' @examples
#' initialise_design_df(
#'   items = c(1, 2, 2, 1, 3, 3, 1, 3, 3),
#'   nrows = 3,
#'   ncols = 3
#' )
#'
#' # blocking
#' initialise_design_df(rep(1:8, 4), 8, 4, 2, 2)
#'
#' # another blocking example
#' initialise_design_df(
#'   items = paste0("T", 1:6),
#'   nrows = 4,
#'   ncols = 6,
#'   block_nrows = 2,
#'   block_ncols = 3
#' )
#' @export
# fmt: skip
initialise_design_df <- function(items,
                                 nrows,
                                 ncols,
                                 block_nrows = 1,
                                 block_ncols = 1) {
  .verify_initialise_design_df(nrows, ncols, block_nrows, block_ncols)

  # If items is a single numeric value, take it as the number of equally replicated treatments
  if (length(items) == 1 && is.numeric(items)) {
    items <- paste0("T", 1:items)
  }

  # Create grid
  df <- expand.grid(row = 1:nrows, col = 1:ncols)
  df$treatment <- items

  # If blocked design
  if (block_nrows != 1 || block_ncols != 1) {
    nblocks_row <- nrows / block_nrows
    nblocks_col <- ncols / block_ncols

    # Which block do the columns and rows belong to?
    df$row_block <- ceiling(df$row / block_nrows)
    df$col_block <- ceiling(df$col / block_ncols)

    # Which block do the experimental units belong to?
    df$block <- df$col_block + (df$row_block - 1) * nblocks_col

    # For each block, assign treatments
    df$treatment[order(df$block)] <- items
  }

  return(df)
}

#' Shuffle Items in A Group
#'
#' @inheritParams generate_neighbour
#' @inheritParams speed
#'
#' @return A data frame with the items shuffled
#'
#' @keywords internal
# fmt: skip
shuffle_items <- function(design, swap, swap_within, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  for (i in unique(design[[swap_within]])) {
    items <- design[design[[swap_within]] == i, ][[swap]]
    design[design[[swap_within]] == i, ][[swap]] <- sample(items)
  }

  return(design)
}

# fmt: skip
.verify_initialise_design_df <- function(nrows,
                                         ncols,
                                         block_nrows,
                                         block_ncols) {
  verify_positive_whole_number(nrows, ncols)

  if (
    (!is.null(block_nrows) && is.null(block_ncols)) ||
      (is.null(block_nrows) && !is.null(block_ncols))
  ) {
    stop("`block_nrows` and `block_ncols` must both be numeric or `NULL`")
  }

  if (!is.null(block_nrows)) {
    verify_positive_whole_number(block_nrows)
    verify_positive_whole_number(block_ncols)

    verify_multiple_of(nrows, block_nrows)
    verify_multiple_of(ncols, block_ncols)
  }
}

# Alias for the function to maintain backward compatibility
#' @rdname initialise_design_df
initialize_design_df <- initialise_design_df
