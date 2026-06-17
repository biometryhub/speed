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
    block_indices <- which(all_blocks == block & !is.na(all_blocks) & !is.na(design[[swap]]))

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
    group_filter <- new_design[[swap_within]] == group & !is.na(new_design[[swap_within]])
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
#' @param splits Deprecated; use [initialise_split_design_df()] instead. A named
#'   list of nested-unit specifications, ordered from the
#'   outermost level to the innermost. Each entry is itself a list with
#'   `nrows` and `ncols` (the dimensions of one unit at that level, in cells)
#'   and an optional `items` (treatments to allocate across the units at that
#'   level, one item per unit, ordered by parent then within-parent ID).
#'   For each level, `<name>` and `<name>_treatment` columns are added (the
#'   latter only if `items` is provided). Used to build hierarchical layouts
#'   such as split-plot, split-split-plot, and strip-plot designs.
#'   (default: `NULL`)
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
                                 splits = NULL,
                                 designs = NULL,
                                 design_col = "site") {
  .verify_initialise_design_df(items, nrows, ncols, block_nrows, block_ncols, splits, designs, design_col)

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
  if (!is.null(items)) {
    df$treatment <- items
  }

  # If blocked design
  if (!is.null(block_nrows)) {
    nblocks_row <- nrows / block_nrows

    df$row_block <- ceiling(df$row / block_nrows)
    df$col_block <- ceiling(df$col / block_ncols)
    df$block <- df$row_block + nblocks_row * (df$col_block - 1)

    # For each block, assign treatments
    if (!is.null(items)) {
      df$treatment[order(df$block)] <- items
    }
  }

  if (!is.null(splits)) {
    warning(
      "The `splits` argument of `initialise_design_df()` is deprecated and will be removed in a future ",
      "version.\nUse `initialise_split_design_df()` instead.",
      call. = FALSE
    )
    df <- apply_splits(df, splits, nrows, ncols, block_nrows, block_ncols)
  }

  return(df)
}

#' Apply Nested Splits to a Design Data Frame
#'
#' @description
#' Adds nested-unit ID columns (and optional treatment columns) to a design data
#' frame produced by [initialise_design_df()]. Each split level subdivides the
#' parent unit (block, or the whole grid if there are no blocks) into smaller
#' rectangular units of size `nrows` by `ncols`. Subsequent levels subdivide
#' the units of the previous level, with the same column-major numbering that
#' [initialise_design_df()] uses for blocks.
#'
#' @inheritParams initialise_design_df
#' @param df A data frame with `row` and `col` columns (and `block` if blocking
#'   is in use), as built by [initialise_design_df()].
#'
#' @return The data frame with one ID column per split (named after the split)
#'   and one `<name>_treatment` column per split that supplies `items`.
#'
#' @keywords internal
apply_splits <- function(df, splits, nrows, ncols, block_nrows, block_ncols) {
  splits <- add_names(splits)

  if (!is.null(block_nrows)) {
    parent_id <- df$block
    parent_nrows <- block_nrows
    parent_ncols <- block_ncols
  } else {
    parent_id <- rep(1L, nrow(df))
    parent_nrows <- nrows
    parent_ncols <- ncols
  }

  for (split_name in names(splits)) {
    split <- splits[[split_name]]

    # Local row/col within the parent unit (parents are contiguous rectangles)
    local_row <- ((df$row - 1) %% parent_nrows) + 1
    local_col <- ((df$col - 1) %% parent_ncols) + 1

    child_row_idx <- ceiling(local_row / split$nrows)
    child_col_idx <- ceiling(local_col / split$ncols)
    n_child_rows <- parent_nrows / split$nrows
    n_children_per_parent <- n_child_rows * (parent_ncols / split$ncols)

    df[[split_name]] <- (parent_id - 1) * n_children_per_parent +
      child_row_idx +
      n_child_rows * (child_col_idx - 1)

    if (!is.null(split$items)) {
      treatment_col <- paste0(split_name, "_treatment")
      n_children <- max(df[[split_name]])
      items_vec <- split$items

      if (length(items_vec) == 1 && is.numeric(items_vec)) {
        items_vec <- paste0("T", seq_len(items_vec))
      }

      if (length(items_vec) != n_children) {
        if (n_children %% length(items_vec) == 0) {
          # Recycle once per parent unit so each parent receives a full set
          items_vec <- rep(items_vec, length.out = n_children)
        } else {
          stop(sprintf(
            "`items` for split `%s` must have length %d (or divide it); got %d",
            split_name, n_children, length(items_vec)
          ), call. = FALSE)
        }
      }

      df[[treatment_col]] <- items_vec[df[[split_name]]]
    }

    parent_id <- df[[split_name]]
    parent_nrows <- split$nrows
    parent_ncols <- split$ncols
  }

  return(df)
}

#' Initialise a Split-Plot Design Data Frame
#'
#' @description
#' Build a split plot design from the ground up, given the nested unit
#' structure and how many times the largest unit is replicated. The field
#' dimensions are derived from the nested structure, and any number of split
#' levels is supported (split-plot, split-split-plot, and so on).
#'
#' @param splits A named list of nested-unit specifications, ordered from the
#'   *innermost* (smallest) level to the *outermost* (the replicated unit, e.g.
#'   the block). Each entry is itself a list with:
#'   - `nrows`, `ncols` - the dimensions of one unit at that level, in cells.
#'     The innermost level is always 1x1, so its `nrows`/`ncols` may be omitted.
#'   - `items` - optional treatments to allocate across the units at that level,
#'     one item per unit. A single number is expanded to `T1`, `T2`, ...;
#'     a shorter vector is reused to fill each parent unit.
#'   The outermost entry defines the replicated unit; `rep_dim` tiles it across
#'   the field. For each level a `<name>` ID column is added, plus a
#'   `<name>_treatment` column wherever `items` is supplied.
#' @param rep_dim Length-2 integer vector `c(row_reps, col_reps)` giving the
#'   replicate dimension of the whole structure (default: `c(1, 1)`).
#'
#' @return A data frame with `row` and `col` columns plus one ID (and optional
#'   treatment) column per split level.
#'
#' @examples
#' # split-plot: 4 blocks (2x2) of 3x4 cells; each block holds 3 wholeplots
#' # (1x4, treatments A-C), each wholeplot holds 4 subplots (1x1, treatments a-d)
#' initialise_split_design_df(
#'   splits = list(
#'     subplot   = list(items = letters[1:4]),
#'     wholeplot = list(items = LETTERS[1:3], nrows = 1, ncols = 4),
#'     block     = list(nrows = 3, ncols = 4)
#'   ),
#'   rep_dim = c(2, 2)
#' )
#'
#' # split-split-plot: an extra level nested inside the subplot
#' initialise_split_design_df(
#'   splits = list(
#'     subsubplot = list(items = 1:2),
#'     subplot    = list(items = letters[1:4], nrows = 1, ncols = 2),
#'     wholeplot  = list(items = LETTERS[1:3], nrows = 1, ncols = 8),
#'     block      = list(nrows = 3, ncols = 8)
#'   ),
#'   rep_dim = c(2, 1)
#' )
#'
#' @export
initialise_split_design_df <- function(splits, rep_dim = c(1, 1)) {
  .verify_initialise_split_design_df(splits, rep_dim)

  splits <- add_names(splits)

  # fill innermost dim
  if (is.null(splits[[1]]$nrows)) splits[[1]]$nrows <- 1
  if (is.null(splits[[1]]$ncols)) splits[[1]]$ncols <- 1

  # construct the design with outermost level and rep dim
  outer_split <- splits[[length(splits)]]
  nrows <- outer_split$nrows * rep_dim[[1]]
  ncols <- outer_split$ncols * rep_dim[[2]]

  df <- expand.grid(row = seq_len(nrows), col = seq_len(ncols))

  # Outermost -> innermost: subdivide each parent into units numbered
  # by column, offset by parent ID for globally unique contiguous IDs.
  # Treatments recycle once per parent.
  parent_id <- rep(1L, nrow(df))
  parent_nrows <- nrows
  parent_ncols <- ncols
  for (split_name in rev(names(splits))) {
    split <- splits[[split_name]]

    # Which child unit (within its parent) each cell falls in, by column
    n_unit_rows <- parent_nrows %/% split$nrows
    local_row <- ((df$row - 1) %% parent_nrows) %/% split$nrows
    local_col <- ((df$col - 1) %% parent_ncols) %/% split$ncols
    unit_in_parent <- local_row + 1 + n_unit_rows * local_col
    n_units <- n_unit_rows * (parent_ncols %/% split$ncols)

    df[[split_name]] <- (parent_id - 1) * n_units + unit_in_parent

    items <- split$items
    if (!is.null(items)) {
      if (length(items) == 1 && is.numeric(items)) {
        items <- paste0("T", seq_len(items))
      }
      df[[paste0(split_name, "_treatment")]] <- items[(unit_in_parent - 1) %% length(items) + 1]
    }

    parent_id <- df[[split_name]]
    parent_nrows <- split$nrows
    parent_ncols <- split$ncols
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

  for (i in levels(design[[swap_within]])) {
    swap_within_filter <- design[[swap_within]] == i & !is.na(design[[swap_within]])
    items <- design[swap_within_filter, ][[swap]]
    design[swap_within_filter, ][[swap]] <- sample(items)
  }

  return(design)
}

#' Random Initialise
#'
#' @description
#' Randomly shuffle items with [shuffle_items] n times and return the best design.
#'
#' @inheritParams speed
#'
#' @return A data frame with the items shuffled
#'
#' @keywords internal
random_initialise <- function(design, optimise, seed = NULL, ...) {
  random_initialisation <- optimise[[1]]$optimise_params$random_initialisation
  if (random_initialisation == 0) {
    return(design)
  }

  if (length(optimise) > 1) {
    groups <- c()
    for (i in seq_along(optimise)) {
      groups <- c(groups, optimise[[i]]$swap_within)
      if (i == 1) next

      now <- as.numeric(Sys.time())
      dummy_col <- paste0(paste(groups, collapse = "_"), "_", now)
      optimise[[i]]$swap_within <- dummy_col
      design[[dummy_col]] <- apply(design[, groups], 1, paste, collapse = "-") |>
        factor()
    }
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
      adj_weight <- opt$optimise_params$adj_weight
      bal_weight <- opt$optimise_params$bal_weight
      current_score <- current_score + opt$obj_function(
        shuffled_design,
        opt$swap,
        spatial_cols,
        adj_weight = adj_weight,
        bal_weight = bal_weight,
        ...
      )$score
    }

    if (current_score < best_score) {
      if (current_score == 0) {
        return(shuffled_design)
      }

      best_score <- current_score
      best_design <- shuffled_design
    }
  }

  for (opt in optimise[-1]) {
    best_design[[opt$swap_within]] <- NULL
  }

  return(best_design)
}

# fmt: skip
.verify_initialise_design_df <- function(items,
                                         nrows,
                                         ncols,
                                         block_nrows,
                                         block_ncols,
                                         splits,
                                         designs,
                                         design_col) {
  if (is.null(designs) && is.null(nrows) && is.null(ncols)) {
    stop("Either `nrows` and `ncols` or `designs` must be provided")
  }

  if (is.null(designs)) {
    verify_positive_whole_number(nrows, ncols)
    if (!is.null(items)) {
      verify_positive_whole_number(length(items))
    } else if (is.null(splits)) {
      stop("`items` must be provided when `splits` is `NULL`", call. = FALSE)
    }
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
    if (!is.null(items)) {
      verify_multiple_of(nrows * ncols, length(items))
    }
  }

  if (!is.null(splits)) {
    verify_list(splits)
    valid_split_args <- c("nrows", "ncols", "items")
    parent_nrows <- if (!is.null(block_nrows)) block_nrows else nrows
    parent_ncols <- if (!is.null(block_ncols)) block_ncols else ncols

    splits_named <- add_names(splits)
    for (split_name in names(splits_named)) {
      split <- splits_named[[split_name]]
      verify_list(split)
      for (arg in names(split)) {
        if (!(arg %in% valid_split_args)) {
          stop(paste0("`", arg, "` is an invalid argument in `splits$", split_name, "`"), call. = FALSE)
        }
      }
      if (length(setdiff(c("nrows", "ncols"), names(split))) > 0) {
        stop(paste0("`nrows` and `ncols` must be provided for split `", split_name, "`"), call. = FALSE)
      }
      verify_positive_whole_number(split$nrows, split$ncols)
      verify_multiple_of(parent_nrows, split$nrows)
      verify_multiple_of(parent_ncols, split$ncols)

      parent_nrows <- split$nrows
      parent_ncols <- split$ncols
    }
  }
}

.verify_initialise_split_design_df <- function(splits, rep_dim) {
  # need at least a nested level and the replicated unit
  verify_list(splits)
  if (length(splits) < 2) {
    stop("`splits` must contain at least two levels", call. = FALSE)
  }

  # rep_dim is c(row_reps, col_reps)
  verify_positive_whole_numbers(rep_dim)
  if (length(rep_dim) != 2) {
    stop("`rep_dim` must be a length-2 vector `c(row_reps, col_reps)`", call. = FALSE)
  }

  valid_split_args <- c("nrows", "ncols", "items")
  splits <- add_names(splits)

  # only the innermost level may omit dimensions (it defaults to 1x1)
  innermost_name <- names(splits)[[1]]

  # outermost -> innermost; the outermost's parent is the rep-tiled field
  outer_split <- splits[[length(splits)]]
  parent_name <- "field"
  parent_nrows <- (if (is.null(outer_split$nrows)) 1 else outer_split$nrows) * rep_dim[[1]]
  parent_ncols <- (if (is.null(outer_split$ncols)) 1 else outer_split$ncols) * rep_dim[[2]]
  for (split_name in rev(names(splits))) {
    split <- splits[[split_name]]

    # check for unknown arguments (catches typos like `nrow`)
    verify_list(split)
    for (arg in names(split)) {
      if (!(arg %in% valid_split_args)) {
        stop(sprintf("`%s` is an invalid argument in `splits$%s`", arg, split_name), call. = FALSE)
      }
    }

    # only the innermost level may omit dimensions (it defaults to 1x1)
    if (split_name != innermost_name && (is.null(split$nrows) || is.null(split$ncols))) {
      stop(sprintf(
        "`nrows` and `ncols` must be provided for split `%s`; only the innermost level may omit them",
        split_name
      ), call. = FALSE)
    }

    # check unit dimensions are positive whole numbers (innermost defaults to 1x1)
    this_nrows <- if (is.null(split$nrows)) 1 else split$nrows
    this_ncols <- if (is.null(split$ncols)) 1 else split$ncols
    verify_positive_whole_number(
      this_nrows, this_ncols,
      var_names = c(sprintf("splits$%s$nrows", split_name), sprintf("splits$%s$ncols", split_name))
    )

    # check if fit in parent dimension
    if (parent_nrows %% this_nrows != 0 || parent_ncols %% this_ncols != 0) {
      stop(sprintf(
        "split `%s` (%dx%d) does not tile evenly into %s (%dx%d)",
        split_name, this_nrows, this_ncols, parent_name, parent_nrows, parent_ncols
      ), call. = FALSE)
    }

    # check items fill the units in a parent a whole number of times
    items <- split$items
    if (!is.null(items)) {
      n_units <- (parent_nrows * parent_ncols) %/% (this_nrows * this_ncols)
      n_items <- if (length(items) == 1 && is.numeric(items)) items else length(items)
      if (n_units %% n_items != 0) {
        stop(sprintf(
          "`items` for split `%s` has length %d, which does not divide the %d units per parent",
          split_name, n_items, n_units
        ), call. = FALSE)
      }
    }

    parent_name <- sprintf("splits$%s", split_name)
    parent_nrows <- this_nrows
    parent_ncols <- this_ncols
  }
}

# Alias for the function to maintain backward compatibility
#' @rdname initialise_design_df
#' @export
initialize_design_df <- initialise_design_df
