#' Generate a Neighbour Design by Swapping Treatments
#'
#' @param design Data frame containing the current design
#' @param swap Column name of the treatment to swap, or named list for hierarchical designs
#' @param swap_within Column name defining groups within which to swap treatments, or named list for hierarchical designs
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
                               swap_count = getOption("speed.swap_count", 1),
                               swap_all_blocks = getOption("speed.swap_all_blocks", FALSE)) {
  
  # Check if this is a hierarchical design
  is_hierarchical <- is.list(swap) && !is.null(names(swap))
  
  if (is_hierarchical) {
    return(generate_hierarchical_neighbour(design, swap, swap_within, swap_count, swap_all_blocks))
  } else {
    return(generate_simple_neighbour(design, swap, swap_within, swap_count, swap_all_blocks))
  }
}

#' Generate neighbour for simple (non-hierarchical) designs
#' @keywords internal
# fmt: skip
generate_simple_neighbour <- function(design,
                                      swap,
                                      swap_within,
                                      swap_count,
                                      swap_all_blocks) {
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
  
  swapped_idx <- 1
  swapped_items <- character(2 * swap_count * length(blocks_to_swap))
  
  # Perform swaps in selected blocks
  for (block in blocks_to_swap) {
    # Get indices of plots in this block
    block_indices <- which(
      design[[swap_within]] == block & !is.na(design[[swap]])
    )
    
    if (length(block_indices) >= 2) {
      # Need at least 2 plots to swap
      for (i in 1:swap_count) {
        # Select two random plots in this block
        swap_pair <- sample(block_indices, 2)
        if (design[[swap]][swap_pair[1]] == design[[swap]][swap_pair[2]]) {
          no_dupe_filter = design[[swap]][block_indices] != design[[swap]][swap_pair[1]]
          swap_pair[[2]] <- sample(block_indices[no_dupe_filter], 1)
        }
        
        # Swap treatments
        temp <- new_design[[swap]][swap_pair[1]]
        new_design[[swap]][swap_pair[1]] <- new_design[[swap]][swap_pair[2]]
        new_design[[swap]][swap_pair[2]] <- temp
        
        swapped_items[swapped_idx] <- new_design[[swap]][swap_pair[1]]
        swapped_items[swapped_idx + 1] <- new_design[[swap]][swap_pair[2]]
        swapped_idx <- swapped_idx + 2
      }
    }
  }
  
  return(list(design = new_design, swapped_items = swapped_items))
}

#' Generate neighbour for hierarchical designs
#' @keywords internal
# fmt: skip
generate_hierarchical_neighbour <- function(design,
                                            swap,
                                            swap_within,
                                            swap_count,
                                            swap_all_blocks) {
  new_design <- design
  
  # Extract hierarchy levels from names (ordered from highest to lowest)
  hierarchy_levels <- names(swap)
  all_swapped_items <- list()
  
  # Process each level of the hierarchy
  for (level in hierarchy_levels) {
    level_swap <- swap[[level]]
    level_swap_within <- swap_within[[level]]
    
    # Get unique groups for this level
    groups <- unique(design[[level_swap_within]])
    
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
      # Get unique treatment units within this group
      group_data <- design[design[[level_swap_within]] == group & !is.na(design[[level_swap]]), ]
      
      if (level == hierarchy_levels[1]) {
        # Highest level (e.g., wholeplot): swap entire treatment units
        treatment_units <- unique(group_data[[level_swap]])
        
        if (length(treatment_units) >= 2) {
          for (i in 1:swap_count) {
            # Select two random treatment units
            swap_pair <- sample(treatment_units, 2)
            
            # Ensure they're different treatments
            if (swap_pair[1] == swap_pair[2]) {
              different_treatments <- treatment_units[treatment_units != swap_pair[1]]
              if (length(different_treatments) > 0) {
                swap_pair[2] <- sample(different_treatments, 1)
              } else {
                next  # Skip this swap if no different treatments available
              }
            }
            
            # Find all plots belonging to these treatment units
            plots_1 <- which(new_design[[level_swap_within]] == group & 
                            new_design[[level_swap]] == swap_pair[1])
            plots_2 <- which(new_design[[level_swap_within]] == group & 
                            new_design[[level_swap]] == swap_pair[2])
            
            # Swap all plots belonging to these treatment units
            temp <- new_design[[level_swap]][plots_1]
            new_design[[level_swap]][plots_1] <- new_design[[level_swap]][plots_2]
            new_design[[level_swap]][plots_2] <- temp
            
            swapped_items[swapped_idx] <- swap_pair[1]
            swapped_items[swapped_idx + 1] <- swap_pair[2]
            swapped_idx <- swapped_idx + 2
          }
        }
      } else {
        # Lower levels (e.g., subplot): swap within higher-level units
        higher_level_units <- unique(group_data[[swap_within[[hierarchy_levels[which(hierarchy_levels == level) - 1]]]]])
        
        for (higher_unit in higher_level_units) {
          # Get indices of plots in this higher-level unit
          unit_indices <- which(
            new_design[[level_swap_within]] == group & 
            new_design[[swap_within[[hierarchy_levels[which(hierarchy_levels == level) - 1]]]]] == higher_unit &
            !is.na(new_design[[level_swap]])
          )
          
          if (length(unit_indices) >= 2) {
            for (i in 1:swap_count) {
              # Select two random plots within this higher-level unit
              swap_pair <- sample(unit_indices, 2)
              
              # Ensure different treatments
              if (new_design[[level_swap]][swap_pair[1]] == new_design[[level_swap]][swap_pair[2]]) {
                no_dupe_filter <- new_design[[level_swap]][unit_indices] != new_design[[level_swap]][swap_pair[1]]
                if (sum(no_dupe_filter) > 0) {
                  swap_pair[2] <- sample(unit_indices[no_dupe_filter], 1)
                } else {
                  next  # Skip this swap
                }
              }
              
              # Swap treatments
              temp <- new_design[[level_swap]][swap_pair[1]]
              new_design[[level_swap]][swap_pair[1]] <- new_design[[level_swap]][swap_pair[2]]
              new_design[[level_swap]][swap_pair[2]] <- temp
              
              swapped_items[swapped_idx] <- new_design[[level_swap]][swap_pair[1]]
              swapped_items[swapped_idx + 1] <- new_design[[level_swap]][swap_pair[2]]
              swapped_idx <- swapped_idx + 2
            }
          }
        }
      }
    }
    
    all_swapped_items[[level]] <- swapped_items[1:(swapped_idx - 1)]
  }
  
  return(list(design = new_design, swapped_items = all_swapped_items))
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
#' @export
# fmt: skip
initialise_design_df <- function(items,
                                 nrows,
                                 ncols,
                                 block_nrows = NULL,
                                 block_ncols = NULL) {
  .verify_initialise_design_df(nrows, ncols, block_nrows, block_ncols)

  # If items is a single numeric value, take it as the number of equally replicated treatments
  if (length(items) == 1 && is.numeric(items)) {
    items <- paste0("T", 1:items)
  }

  rows <- rep(1:nrows, ncols)
  cols <- rep(1:ncols, each = nrows)
  df <- data.frame(
    row = rows,
    col = cols,
    treatment = items
  )
  if (!is.null(block_nrows)) {
    nblocks_row <- nrows / block_nrows
    nblocks_col <- ncols / block_ncols

    df$row_block <- rep(1:nblocks_row, ncols, each = block_nrows)
    df$col_block <- rep(1:nblocks_col, each = nrows * block_ncols)
    df$block <- as.numeric(df$row_block) +
      nblocks_row * (as.numeric(df$col_block) - 1)
  }
  return(df)
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