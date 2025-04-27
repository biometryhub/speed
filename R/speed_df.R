#' Optimize Experimental Design Layout Using Simulated Annealing
#'
#' @description
#' Optimizes the spatial layout of experimental designs using simulated annealing to minimize
#' treatment adjacency and maintain treatment balance across spatial factors.
#'
#' @param data A data frame containing the initial design layout with spatial coordinates
#' @param swap A column name of the treatment to be swapped (e.g., `treatment`)
#' @param swap_within A string specifying the blocking variable that is a boundary within which to swap
#'   treatments. Specify `"1"` or `"none"` for no boundary (default: `"1"`)
#' @param spatial_factors A one-sided formula specifying spatial factors to consider for balance (default:
#'   `~row + col`)
#' @param iterations Maximum number of iterations for the simulated annealing algorithm (default: 10000)
#' @param early_stop_iterations Number of iterations without improvement before early stopping (default: 2000)
#' @param obj_function Objective function used to calculate score (lower is better) (default:
#'   \link{objective_function})
#' @param quiet Logical; if TRUE, suppresses progress messages (default: FALSE)
#' @param seed A numeric value for random seed. If provided, it ensures reproducibility of results (default:
#'   NULL).
#'
#' @return A list containing:
#' \itemize{
#'   \item design_df - Data frame of optimized design
#'   \item score - Final optimization score
#'   \item adjacency_score - Score for treatment adjacencies
#'   \item balance_score - Score for spatial balance
#'   \item scores - Vector of scores across iterations
#'   \item temperatures - Vector of temperatures across iterations
#'   \item iterations_run - Total number of iterations performed
#'   \item stopped_early - Logical indicating if optimization stopped early
#'   \item treatments - Vector of unique treatments
#'   \item seed - Random seed used for reproducibility of the design. If not set in the function, the seed is
#'      set to the second element of .Random.seed.
#' }
#'
#' @importFrom stringi stri_sort
#'
#' @examples
#' # Create a simple design with 3 replicates of 4 treatments in a 4x3 layout
#' df <- data.frame(
#'   row = rep(1:4, each = 3),
#'   col = rep(1:3, times = 4),
#'   treatment = rep(LETTERS[1:4], 3)
#' )
#'
#' # Optimize the design
#' result <- speed_df(df, swap = "treatment")
#'
#' @export
speed_df <- function(
        data,
        swap,
        swap_within = "1",
        spatial_factors = ~ row + col,
        iterations = 10000,
        early_stop_iterations = 2000,
        obj_function = objective_function_df,
        quiet = FALSE,
        seed = NULL) {
    # Permute is for the levels of the treatment that get shuffled within the levels of the swap_within factor
    # E.g. swap_within = ~block will permute treatments within blocks, rather than the entire layout
    # E.g. permute = ~treatment will permute the levels of treatment within the blocks

    # NOTE: weights moved to cost function
    swap_count <- getOption("speed.swap_count", 1)
    swap_all_blocks <- getOption("speed.swap_all_blocks", FALSE)
    adaptive_swaps <- getOption("speed.adaptive_swaps", FALSE)
    start_temp <- getOption("speed.start_temp", 100)
    cooling_rate <- getOption("speed.cooling_rate", 0.99)

    if (!is.character(swap)) {
        swap <- as.character(substitute(swap))
    }

    .verify_speed_inputs(
        data,
        swap,
        swap_within,
        spatial_factors,
        iterations,
        early_stop_iterations,
        quiet,
        seed,
        swap_count,
        swap_all_blocks,
        adaptive_swaps,
        start_temp,
        cooling_rate
    )

    # Set up the design dataframe
    layout_df <- data

    # Define the swapping groups
    if (swap_within == "1" || swap_within == "none") {
        layout_df$swap_group <- factor(rep(1, nrow(data)))
    } else {
        layout_df$swap_group <- factor(layout_df[[swap_within]])
    }

    spatial_cols <- all.vars(spatial_factors)
    treatments <- layout_df[[swap]]

    # Set seed for reproducibility
    if (is.null(seed)) {
        seed <- .Random.seed[2]
    }
    set.seed(seed)

    # Initialize the current design as a copy of the original
    current_design_df <- layout_df
    best_design_df <- current_design_df

    # Calculate initial score
    current_score <- obj_function(current_design_df, swap, spatial_cols)

    # TODO: somehow move this to `.verify_speed_inputs`
    if (!is.numeric(current_score)) {
        stop("Value from `objective_function` must be numeric.")
    }

    best_score <- current_score
    temp <- start_temp
    scores <- numeric(iterations)
    temperatures <- numeric(iterations)
    last_improvement_iter <- 0

    # Main optimization loop
    for (iter in 1:iterations) {
        scores[iter] <- current_score
        temperatures[iter] <- temp

        if (adaptive_swaps) {
            current_swap_count <- max(1, round(swap_count * temp / start_temp))
            current_swap_all_blocks <- runif(1) < (temp / start_temp) && swap_all_blocks
        } else {
            current_swap_count <- swap_count
            current_swap_all_blocks <- swap_all_blocks
        }

        # Generate a neighboring solution by swapping treatments
        new_design_df <- generate_neighbor_df(current_design_df, swap, "swap_group",
                                              current_swap_count, current_swap_all_blocks)

        # Calculate score for the new design
        new_score <- obj_function(new_design_df, swap, spatial_cols)

        # Accept or reject the new design based on simulated annealing criteria
        if (new_score < current_score || runif(1) < exp((current_score - new_score) / temp)) {
            current_design_df <- new_design_df
            current_score <- new_score
            if (new_score < best_score) {
                best_design_df <- new_design_df
                best_score <- new_score
                last_improvement_iter <- iter
            }
        }

        # Cool down the temperature
        temp <- temp * cooling_rate

        # Progress reporting
        if (!quiet && iter %% 1000 == 0) {
            cat(
                "Iteration:", iter, "Score:", current_score, "Best:", best_score,
                "Since Improvement:", iter - last_improvement_iter, "\n"
            )
        }

        # Early stopping check
        if (iter - last_improvement_iter >= early_stop_iterations) {
            if (!quiet) cat("Early stopping at iteration", iter, "\n")
            scores <- scores[1:iter]
            temperatures <- temperatures[1:iter]
            break
        }
    }

    # Remove the swap_group column from the final design if it was added internally
    if (swap_within == "1" || swap_within == "none") {
        best_design_df$swap_group <- NULL
    } else if (!"swap_within" %in% names(data)) {
        best_design_df$swap_group <- NULL
    }

    output <- list(
        design_df = best_design_df,
        score = best_score,
        adjacency_score = calculate_adjacency_score_df(best_design_df, swap, spatial_cols),
        balance_score = calculate_balance_score_df(best_design_df, swap, spatial_cols),
        scores = scores,
        temperatures = temperatures,
        iterations_run = length(scores),
        stopped_early = length(scores) < iterations,
        treatments = stringi::stri_sort(unique(best_design_df[[swap]]), numeric = TRUE),
        seed = seed
    )

    class(output) <- c("design", class(output))

    return(output)
}

#' Generate a Neighboring Solution by Swapping Treatments in a Data Frame
#'
#' @description
#' Creates a new design by swapping treatments within specified swap groups.
#'
#' @param design_df A data frame containing the current design
#' @param treatment_col The column name containing treatments
#' @param swap_group_col The column name containing swap groups
#' @param swap_count Number of swaps to perform
#' @param swap_all_blocks Whether to perform swaps in all blocks
#'
#' @return A data frame with the new design after swaps
#'
#' @keywords internal
generate_neighbor_df <- function(design_df, treatment_col, swap_group_col, swap_count = 1, swap_all_blocks = FALSE) {
    new_design_df <- design_df

    # Get unique swap groups
    swap_groups <- unique(design_df[[swap_group_col]])

    # Determine which groups to process
    groups_to_process <- if (swap_all_blocks) {
        swap_groups
    } else {
        sample(swap_groups, 1)
    }

    # Perform swaps within each selected group
    for (group in groups_to_process) {
        # Get indices for current group
        group_indices <- which(new_design_df[[swap_group_col]] == group)

        if (length(group_indices) >= 2) {  # Need at least 2 positions to swap
            for (i in 1:swap_count) {
                # Randomly select two positions within the group
                swap_positions <- sample(group_indices, 2)

                # Swap treatments
                temp <- new_design_df[swap_positions[1], treatment_col]
                new_design_df[swap_positions[1], treatment_col] <- new_design_df[swap_positions[2], treatment_col]
                new_design_df[swap_positions[2], treatment_col] <- temp
            }
        }
    }

    return(new_design_df)
}

#' Calculate Adjacency Score for a Design Data Frame
#'
#' @description
#' Calculates a score based on treatment adjacencies, where lower scores indicate
#' fewer adjacent cells with the same treatment.
#'
#' @param design_df A data frame containing the design
#' @param treatment_col The column name containing treatments
#' @param spatial_cols Vector of column names for spatial coordinates
#'
#' @return Numeric score for adjacency
#'
#' @keywords internal
calculate_adjacency_score_df <- function(design_df, treatment_col, spatial_cols) {
    # This is a placeholder implementation
    # A more sophisticated implementation would identify true adjacencies in the spatial layout
    # based on the spatial_cols provided

    total_adjacencies <- 0

    # For traditional row/col layouts
    if (all(c("row", "col") %in% spatial_cols)) {
        # Create a lookup of positions
        positions <- data.frame(
            id = 1:nrow(design_df),
            row = design_df$row,
            col = design_df$col,
            treatment = design_df[[treatment_col]]
        )

        # Check each position for adjacencies
        for (i in 1:nrow(positions)) {
            current_row <- positions$row[i]
            current_col <- positions$col[i]
            current_treatment <- positions$treatment[i]

            # Look for adjacent positions (up, down, left, right)
            adjacent_positions <- positions[
                (positions$row == current_row & abs(positions$col - current_col) == 1) |
                    (positions$col == current_col & abs(positions$row - current_row) == 1),
            ]

            # Count adjacencies with same treatment
            same_treatment_adjacencies <- sum(adjacent_positions$treatment == current_treatment)
            total_adjacencies <- total_adjacencies + same_treatment_adjacencies
        }

        # We count each adjacency twice (once from each cell), so divide by 2
        total_adjacencies <- total_adjacencies / 2
    } else {
        # For arbitrary spatial layouts, we would need a more general adjacency definition
        # This could use distance-based measures or custom adjacency matrices provided by the user
        warning("Custom adjacency calculation needed for non-row/col spatial factors")

        # Default simple method: count duplicates in treatment across consecutive rows in the data frame
        # This is just a placeholder and should be replaced with proper adjacency logic
        total_adjacencies <- sum(design_df[[treatment_col]][-1] == design_df[[treatment_col]][-nrow(design_df)])
    }

    return(total_adjacencies)
}

#' Calculate Balance Score for a Design Data Frame
#'
#' @description
#' Calculates a score based on how well treatments are balanced across spatial factors.
#'
#' @param design_df A data frame containing the design
#' @param treatment_col The column name containing treatments
#' @param spatial_cols Vector of column names for spatial factors
#'
#' @return Numeric score for balance
#'
#' @keywords internal
calculate_balance_score_df <- function(design_df, treatment_col, spatial_cols) {
    treatments <- unique(design_df[[treatment_col]])
    total_imbalance <- 0

    for (spatial_col in spatial_cols) {
        # Skip if column doesn't exist
        if (!spatial_col %in% names(design_df)) next

        levels <- unique(design_df[[spatial_col]])

        # Calculate expected count for perfect balance
        expected_count <- nrow(design_df) / (length(treatments) * length(levels))

        # Calculate actual counts for each treatment in each level
        for (treatment in treatments) {
            for (level in levels) {
                actual_count <- sum(design_df[[treatment_col]] == treatment &
                                        design_df[[spatial_col]] == level)

                # Add deviation from expected count to total imbalance
                total_imbalance <- total_imbalance + abs(actual_count - expected_count)
            }
        }
    }

    return(total_imbalance)
}

#' A Default Objective Function for Design Optimization
#'
#' @description
#' Calculates a composite score based on treatment adjacency and spatial balance.
#'
#' @param design_df A data frame containing the design
#' @param treatment_col The column name containing treatments
#' @param spatial_cols Vector of column names for spatial factors
#' @param adjacency_weight Weight for the adjacency component (default: 1)
#' @param balance_weight Weight for the balance component (default: 1)
#'
#' @return A numeric score where lower values represent better designs
#'
#' @export
objective_function_df <- function(design_df, treatment_col, spatial_cols,
                               adjacency_weight = 1, balance_weight = 1) {
    adjacency_score <- calculate_adjacency_score_df(design_df, treatment_col, spatial_cols)
    balance_score <- calculate_balance_score_df(design_df, treatment_col, spatial_cols)

    return(adjacency_weight * adjacency_score + balance_weight * balance_score)
}

