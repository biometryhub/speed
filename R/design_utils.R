#' Generate a Neighbour Design by Swapping Treatments
#'
#' @param design Data frame containing the current design
#' @param swap Column name of the treatment to swap, or named list for
#'   hierarchical designs
#' @param swap_within Column name defining groups within which to swap
#'   treatments, or named list for hierarchical designs
#' @param swap_count Number of swaps to perform
#' @param swap_all_blocks Whether to perform swaps in all blocks or just one
#' @param swap_all Whether to swap all matching items or a single item at a time
#'   (default: FALSE)
#'
#' @return A list with the updated design after swapping and information about
#'   swapped items
#'
#' @keywords internal
# fmt: skip
generate_neighbour <- function(design,
                               swap,
                               swap_within,
                               swap_count = getOption("speed.swap_count", 1),
                               swap_all_blocks = getOption("speed.swap_all_blocks", FALSE),
                               swap_all = FALSE) {
  if (swap_all) {
    return(generate_multi_swap_neighbour(design, swap, swap_within, swap_count, swap_all_blocks))
  } else {
    return(generate_single_swap_neighbour(design, swap, swap_within, swap_count, swap_all_blocks))
  }
}

#' Generate neighbour for simple (non-hierarchical) designs
#' @keywords internal
# fmt: skip
generate_single_swap_neighbour <- function(design, swap, swap_within, swap_count, swap_all_blocks) {
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

        # If both plots have the same treatment, try to find a different one
        if (to_be_swapped[1] == to_be_swapped[2]) {
          different_indices <- block_indices[new_design[[swap]][block_indices] != to_be_swapped[1]]

          # Only proceed with swap if different treatments are available
          if (length(different_indices) > 0) {
            swap_pair[[2]] <- sample(different_indices, 1)
            to_be_swapped[2] <- new_design[[swap]][[swap_pair[[2]]]]
          } else {
            # Skip this swap - no different treatments available
            to_be_swapped <- NULL
          }
        }

        # Perform the swap only if we have valid treatments to swap
        if (!is.null(to_be_swapped)) {
          new_design[[swap]][rev(swap_pair)] <- to_be_swapped
          swapped_items[swapped_idx:(swapped_idx + 1)] <- to_be_swapped
          swapped_idx <- swapped_idx + 2
        }
      }
    }
  }

  return(list(design = new_design, swapped_items = swapped_items))
}

#' Generate neighbour for sequential or hierarchical designs
#' @keywords internal
# fmt: skip
generate_multi_swap_neighbour <- function(design, swap, swap_within, swap_count, swap_all_blocks) {
  new_design <- design

  # Get unique groups for this level
  groups <- levels(design[[swap_within]])

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
    group_filter <- new_design[[swap_within]] == group
    group_data <- new_design[group_filter & !is.na(new_design[[swap]]), ]
    group_treatments <- unique(group_data[[swap]])

    if (nrow(group_data) >= 2) {
      for (i in 1:swap_count) {
        # Only proceed if there are at least 2 different treatments
        if (length(group_treatments) < 2) {
          # Skip this swap - only one treatment in this group
          next
        }

        # Select two different treatments
        # Use sample with replace=FALSE to ensure they're different
        swap_pair <- sample(group_treatments, 2, replace = FALSE)

        # Find all plots with these treatments in this group
        plots_1 <- which(group_filter & new_design[[swap]] == swap_pair[1])
        plots_2 <- which(group_filter & new_design[[swap]] == swap_pair[2])

        # Swap all instances of these treatments
        new_design[[swap]][plots_1] <- swap_pair[2]
        new_design[[swap]][plots_2] <- swap_pair[1]

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
#' @param items Items to be placed in the design. Either a single numeric value
#'   (the number of equally replicated items), or a vector of items. (default:
#'   `NULL`)
#' @param nrows Number of rows in the design (default: `NULL`)
#' @param ncols Number of columns in the design (default: `NULL`)
#' @param block_nrows Number of rows in each block (default: `NULL`)
#' @param block_ncols Number of columns in each block (default: `NULL`)
#' @param designs A list of named arguments describing design specifications,
#'   required if `nrows` and `ncols` are absent. (default: `NULL`)
#' @param design_col A column name to distinguish different designs (default:
#'   `"site"`)
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
#'
#' # MET
#' initialise_design_df(
#'   items = c(rep(1:10, 6), rep(11:20, 8)),
#'   designs = list(
#'     a = list(nrows = 10, ncols = 3),
#'     b = list(nrows = 10, ncols = 5),
#'     c = list(nrows = 10, ncols = 6)
#'   )
#' )
#'
#' # MET with different items for each site
#' initialise_design_df(
#'   designs = list(
#'     a = list(items = 1:30, nrows = 10, ncols = 6),
#'     b = list(items = 1:25, nrows = 10, ncols = 5),
#'     c = list(items = 16:30, nrows = 10, ncols = 3)
#'   )
#' )
#'
#' @export
# fmt: skip
initialise_design_df <- function(items = NULL,
                                 nrows = NULL,
                                 ncols = NULL,
                                 block_nrows = NULL,
                                 block_ncols = NULL,
                                 designs = NULL,
                                 design_col = "site") {
  .verify_initialise_design_df(items, nrows, ncols, block_nrows, block_ncols, designs, design_col)

  # If items is a single numeric value, take it as the number of equally replicated treatments
  if (length(items) == 1 && is.numeric(items)) {
    items <- paste0("T", 1:items)
  }

  # if designs is provided, usually for multi-site
  if (!is.null(designs)) {
    return(initialise_multiple_designs_df(items, designs, design_col))
  }

  # Create grid
  df <- expand.grid(row = 1:nrows, col = 1:ncols)
  df$treatment <- items

  # If blocked design
  if (!is.null(block_nrows)) {
    nblocks_row <- nrows / block_nrows

    df$row_block <- ceiling(df$row / block_nrows)
    df$col_block <- ceiling(df$col / block_ncols)
    df$block <- df$row_block + nblocks_row * (df$col_block - 1)

    # For each block, assign treatments
    df$treatment[order(df$block)] <- items
  }

  return(df)
}

#' Initialise Multiple Design Data Frames
#'
#' @inheritParams initialise_design_df
#'
#' @keywords internal
initialise_multiple_designs_df <- function(items, designs, design_col) {
  designs <- add_names(designs)
  df <- data.frame()
  for (design_name in names(designs)) {
    design_args <- designs[[design_name]]
    items_sub <- design_args$items
    if (is.null(items_sub)) {
      item_idx <- seq_len(design_args$nrows * design_args$ncols)
      items_sub <- items[item_idx]
      items <- items[-item_idx]
    }

    df_sub <- initialise_design_df(
      items_sub, design_args$nrows, design_args$ncols, design_args$block_nrows, design_args$block_ncols
    )
    df_sub[[design_col]] <- design_name
    df <- rbind_fill(df, df_sub)
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

#' Random Initialize
#'
#' @description
#' Randomly shuffle items with [shuffle_items] n times and return the best design.
#'
#' @inheritParams speed
#'
#' @return A data frame with the items shuffled
#'
#' @keywords internal
random_initialize <- function(design, optimise, seed = NULL, ...) {
  random_initialisation <- optimise[[1]]$optimize_params$random_initialisation
  if (random_initialisation == 0) {
    return(design)
  }

  best_score <- Inf
  best_design <- design
  for (i in seq_len(random_initialisation)) {
    shuffled_design <- design
    for (opt in optimise) {
      shuffled_design <- shuffle_items(shuffled_design, opt$swap, opt$swap_within, seed + i - 1)
    }

    # scoring
    current_score <- 0
    for (opt in optimise) {
      spatial_cols <- all.vars(opt$spatial_factors)
      current_score <- current_score + opt$obj_function(shuffled_design, opt$swap, spatial_cols, ...)$score
    }

    if (current_score < best_score) {
      if (current_score == 0) {
        return(shuffled_design)
      }

      best_score <- current_score
      best_design <- shuffled_design
    }
  }

  return(best_design)
}

# fmt: skip
.verify_initialise_design_df <- function(items,
                                         nrows,
                                         ncols,
                                         block_nrows,
                                         block_ncols,
                                         designs,
                                         design_col) {
  if (is.null(designs) && is.null(nrows) && is.null(ncols)) {
    stop("Either `nrows` and `ncols` or `designs` must be provided")
  }

  if (is.null(designs)) {
    verify_positive_whole_number(length(items), nrows, ncols)
  } else {
    verify_list(designs)
    valid_args <- c("items", "nrows", "ncols", "block_nrows", "block_ncols")
    for (design in designs) {
      verify_list(design)
      for (arg in names(design)) {
        if (!(arg %in% valid_args)) {
          stop(paste0("`", arg, "` is an invalid argument"))
        }
      }
      if (length(setdiff(c("nrows", "ncols"), names(design))) > 0) {
        stop("`nrows` and `ncols` must be provided for each design")
      }
    }

    # check items
    items_exist <- unlist(lapply(designs, function(x) "items" %in% names(x)))
    if (any(items_exist) && !all(items_exist)) {
      stop("`items` must be provided for all designs")
    }
    if (all(!items_exist) && is.null(items)) {
      stop("`items` must be provided for all designs or `items` must be provided to `initialise_design_df`")
    }
  }
  verify_character(design_col)

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
    verify_multiple_of(nrows * ncols, length(items))
  }
}

# Alias for the function to maintain backward compatibility
#' @rdname initialise_design_df
#' @export
initialize_design_df <- initialise_design_df
