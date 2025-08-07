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
          no_dupe_filter <- design[[swap]][block_indices] != design[[swap]][swap_pair[1]]
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
    # Get unique treatments within this group
    group_data <- design[design[[level_swap_within]] == group & !is.na(design[[level_swap]]), ]
    group_treatments <- unique(design[design[[level_swap_within]] == group & !is.na(design[[level_swap]]), level_swap])

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
            next  # Skip this swap if no different treatments available
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

#' Initialise Design Data Frame
#'
#' @description
#' Initialise a design data frame with or without blocking, including support for split-plot designs.
#'
#' @param items Items to be placed in the design. Either a single numeric value (the number of
#' equally replicated items), or a vector of items. For split-plot designs, can be a named list
#' with 'whole_plot' and 'sub_plot' elements.
#' @param nrows Number of rows in the design
#' @param ncols Number of columns in the design
#' @param block_nrows Number of rows in each block
#' @param block_ncols Number of columns in each block
#' @param design_type Type of design to create. Options: "crd" (completely randomized design),
#' "rcbd" (randomized complete block design), "split_plot", "strip_plot"
#' @param wp_nrows Number of rows per whole plot (for split-plot designs)
#' @param wp_ncols Number of columns per whole plot (for split-plot designs)
#' @param randomize Whether to randomize treatment assignments (default: TRUE)
#' @param seed Random seed for reproducibility
#'
#' @return A data frame containing the design
#'
#' @examples
#' # Simple CRD
#' initialise_design_df(
#'   items = c(1, 2, 2, 1, 3, 3, 1, 3, 3),
#'   nrows = 3,
#'   ncols = 3
#' )
#'
#' # RCBD with blocking
#' initialise_design_df(rep(1:8, 4), 8, 4, 2, 2, design_type = "rcbd")
#'
#' # Split-plot design
#' initialise_design_df(
#'   items = list(whole_plot = c("A", "B"), sub_plot = c("X", "Y", "Z")),
#'   nrows = 6, ncols = 4,
#'   design_type = "split_plot",
#'   wp_nrows = 2, wp_ncols = 2
#' )
#'
#' @export
# fmt: skip
initialise_design_df <- function(items,
                                 nrows,
                                 ncols,
                                 block_nrows = NULL,
                                 block_ncols = NULL,
                                 design_type = "crd",
                                 wp_nrows = NULL,
                                 wp_ncols = NULL,
                                 randomize = TRUE,
                                 seed = NULL) {
  .verify_initialise_design_df(nrows, ncols, block_nrows, block_ncols, design_type, wp_nrows, wp_ncols)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Handle different design types
  switch(design_type,
    "crd" = .create_crd(items, nrows, ncols, randomize),
    "rcbd" = .create_rcbd(items, nrows, ncols, block_nrows, block_ncols, randomize),
    "split_plot" = .create_split_plot(items, nrows, ncols, wp_nrows, wp_ncols, block_nrows, block_ncols, randomize),
    "strip_plot" = .create_strip_plot(items, nrows, ncols, wp_nrows, wp_ncols, block_nrows, block_ncols, randomize),
    stop("Unsupported design_type. Use 'crd', 'rcbd', 'split_plot', or 'strip_plot'")
  )
}

#' Shuffle Items in A Group
#'
#' @inheritParams generate_neighbour
#' @inheritParams speed
#'
#' @return A data frame with the items shuffled
#'
#' @keywords internal
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
                                         block_ncols,
                                         design_type = "crd",
                                         wp_nrows = NULL,
                                         wp_ncols = NULL) {
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

  # Validate split-plot parameters
  if (design_type %in% c("split_plot", "strip_plot")) {
    if (is.null(wp_nrows) || is.null(wp_ncols)) {
      stop("For split-plot designs, both `wp_nrows` and `wp_ncols` must be specified")
    }
    verify_positive_whole_number(wp_nrows, wp_ncols)
    verify_multiple_of(nrows, wp_nrows)
    verify_multiple_of(ncols, wp_ncols)
  }
}

# Helper function for CRD
.create_crd <- function(items, nrows, ncols, randomize) {
  # If items is a single numeric value, take it as the number of equally replicated treatments
  if (length(items) == 1 && is.numeric(items)) {
    items <- paste0("T", 1:items)
  }

  rows <- rep(1:nrows, ncols)
  cols <- rep(1:ncols, each = nrows)
  
  df <- data.frame(
    row = rows,
    col = cols,
    treatment = if (randomize) sample(items) else items
  )
  
  return(df)
}

# Helper function for RCBD
.create_rcbd <- function(items, nrows, ncols, block_nrows, block_ncols, randomize) {
  df <- .create_crd(items, nrows, ncols, randomize = FALSE)
  
  if (!is.null(block_nrows)) {
    nblocks_row <- nrows / block_nrows
    nblocks_col <- ncols / block_ncols

    df$row_block <- rep(1:nblocks_row, ncols, each = block_nrows)
    df$col_block <- rep(1:nblocks_col, each = nrows * block_ncols)
    df$block <- as.numeric(df$row_block) +
      nblocks_row * (as.numeric(df$col_block) - 1)
    
    # Randomize treatments within each block
    if (randomize) {
      for (b in unique(df$block)) {
        block_indices <- which(df$block == b)
        df$treatment[block_indices] <- sample(df$treatment[block_indices])
      }
    }
  }
  
  return(df)
}

# Helper function for split-plot design
.create_split_plot <- function(items, nrows, ncols, wp_nrows, wp_ncols, block_nrows, block_ncols, randomize) {
  # Validate items for split-plot
  if (!is.list(items) || !all(c("whole_plot", "sub_plot") %in% names(items))) {
    stop("For split-plot designs, 'items' must be a list with 'whole_plot' and 'sub_plot' elements")
  }
  
  whole_plot_treatments <- items$whole_plot
  sub_plot_treatments <- items$sub_plot
  
  # Create base design
  rows <- rep(1:nrows, ncols)
  cols <- rep(1:ncols, each = nrows)
  
  # Calculate whole plot structure
  wp_rows <- nrows / wp_nrows
  wp_cols <- ncols / wp_ncols
  
  # Create whole plot identifiers
  whole_plot_id <- rep(1:(wp_rows * wp_cols), each = wp_nrows * wp_ncols)
  
  # Assign whole plot treatments
  wp_treatment_assignment <- rep(whole_plot_treatments, length.out = wp_rows * wp_cols)
  if (randomize) {
    wp_treatment_assignment <- sample(wp_treatment_assignment)
  }
  
  # Create sub-plot assignments within each whole plot
  sub_plot_assignment <- character(nrows * ncols)
  for (i in 1:(wp_rows * wp_cols)) {
    wp_indices <- which(whole_plot_id == i)
    sp_treatments <- rep(sub_plot_treatments, length.out = length(wp_indices))
    if (randomize) {
      sp_treatments <- sample(sp_treatments)
    }
    sub_plot_assignment[wp_indices] <- sp_treatments
  }
  
  df <- data.frame(
    row = rows,
    col = cols,
    whole_plot = whole_plot_id,
    whole_plot_treatment = wp_treatment_assignment[whole_plot_id],
    sub_plot_treatment = sub_plot_assignment,
    treatment = paste(wp_treatment_assignment[whole_plot_id], sub_plot_assignment, sep = "_")
  )
  
  # Add blocking if specified
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

# Helper function for strip-plot design
.create_strip_plot <- function(items, nrows, ncols, wp_nrows, wp_ncols, block_nrows, block_ncols, randomize) {
  # Validate items for strip-plot
  if (!is.list(items) || !all(c("row_factor", "col_factor") %in% names(items))) {
    stop("For strip-plot designs, 'items' must be a list with 'row_factor' and 'col_factor' elements")
  }
  
  row_treatments <- items$row_factor
  col_treatments <- items$col_factor
  
  # Create base design
  rows <- rep(1:nrows, ncols)
  cols <- rep(1:ncols, each = nrows)
  
  # Calculate strip structure
  row_strips <- nrows / wp_nrows
  col_strips <- ncols / wp_ncols
  
  # Assign row treatments (horizontal strips)
  row_treatment_assignment <- rep(row_treatments, length.out = row_strips)
  if (randomize) {
    row_treatment_assignment <- sample(row_treatment_assignment)
  }
  
  # Assign column treatments (vertical strips)
  col_treatment_assignment <- rep(col_treatments, length.out = col_strips)
  if (randomize) {
    col_treatment_assignment <- sample(col_treatment_assignment)
  }
  
  # Create strip assignments
  row_strip <- rep(rep(1:row_strips, each = wp_nrows), ncols)
  col_strip <- rep(1:col_strips, each = nrows * wp_ncols)
  
  df <- data.frame(
    row = rows,
    col = cols,
    row_strip = row_strip,
    col_strip = col_strip,
    row_treatment = row_treatment_assignment[row_strip],
    col_treatment = col_treatment_assignment[col_strip],
    treatment = paste(row_treatment_assignment[row_strip], 
                     col_treatment_assignment[col_strip], sep = "_")
  )
  
  # Add blocking if specified
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

# Alias for the function to maintain backward compatibility
#' @rdname initialise_design_df
initialize_design_df <- initialise_design_df
