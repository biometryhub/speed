
#' Optimize Experimental Design Layout Using Simulated Annealing
#'
#' @description
#' Optimizes the spatial layout of experimental designs using simulated annealing to minimize
#' treatment adjacency and maintain treatment balance across spatial factors. Works with
#' regular or irregular spatial designs.
#'
#' @param data A data frame containing the experimental design with spatial coordinates
#' @param swap A column name of the treatment to be swapped (e.g., `treatment`)
#' @param swap_within A string specifying the blocking variable that is a boundary within which to swap
#'   treatments. Specify `"1"` or `"none"` for no boundary (default: `"1"`)
#' @param spatial_factors A one-sided formula specifying spatial factors to consider for balance (default:
#'   `~row + col`)
#' @param adjacency_method Method to determine adjacent plots: "coordinate" uses Euclidean distance,
#'   "cardinal" uses only up/down/left/right neighbors (default: "coordinate")
#' @param adjacency_threshold Distance threshold for considering plots adjacent when using
#'   "coordinate" method (default: 1.5)
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
#'   \item seed - Random seed used for reproducibility of the design
#'   \item adjacency_list - List of adjacent plots used in optimization
#' }
#'
#' @importFrom stringi stri_sort
#' @importFrom dplyr %>% filter mutate select group_by ungroup
#' @importFrom stats dist runif
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
        adjacency_method = "coordinate",
        adjacency_threshold = 1.5,
        iterations = 10000,
        early_stop_iterations = 2000,
        obj_function = objective_function(),
        quiet = FALSE,
        seed = NULL) {

    # Extract options
    swap_count <- getOption("speed.swap_count", 1)
    swap_all_blocks <- getOption("speed.swap_all_blocks", FALSE)
    adaptive_swaps <- getOption("speed.adaptive_swaps", FALSE)
    start_temp <- getOption("speed.start_temp", 100)
    cooling_rate <- getOption("speed.cooling_rate", 0.99)

    swap <- as.character(substitute(swap))

    # Verify inputs
    .verify_speed_inputs_df(
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
        cooling_rate,
        adjacency_method,
        adjacency_threshold
    )

    # Handle swap_within
    layout_df <- data
    if (swap_within == "1" || swap_within == "none") {
        layout_df$swap_group <- factor(rep(1, nrow(data)))
        swap_within <- "swap_group"
    }

    spatial_cols <- all.vars(spatial_factors)
    treatments <- layout_df[[swap]]

    # Set seed for reproducibility
    if (is.null(seed)) {
        dummy_seed <- runif(1)
        seed <- .Random.seed[3]
    }
    set.seed(seed)

    # Find adjacent plots
    adjacency_list <- find_adjacent_plots(layout_df, spatial_cols,
                                          method = adjacency_method,
                                          threshold = adjacency_threshold)

    # Initialize design
    current_design <- layout_df
    best_design <- current_design

    # Calculate initial score
    current_score <- obj_function(current_design, adjacency_list, swap, spatial_cols)
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

        # Generate new design by swapping treatments
        new_design <- generate_neighbor_df(current_design,
                                           swap,
                                           swap_within,
                                           swap_count = current_swap_count,
                                           swap_all_blocks = current_swap_all_blocks)

        # Calculate new score
        new_score <- obj_function(new_design, adjacency_list, swap, spatial_cols)

        # Decide whether to accept the new design
        if (new_score < current_score || runif(1) < exp((current_score - new_score) / temp)) {
            current_design <- new_design
            current_score <- new_score
            if (new_score < best_score) {
                best_design <- new_design
                best_score <- new_score
                last_improvement_iter <- iter
            }
        }

        # Cool temperature
        temp <- temp * cooling_rate

        # Progress reporting
        if (!quiet && iter %% 1000 == 0) {
            cat(
                "Iteration:", iter, "Score:", current_score, "Best:", best_score,
                "Since Improvement:", iter - last_improvement_iter, "\n"
            )
        }

        # Early stopping
        if (iter - last_improvement_iter >= early_stop_iterations) {
            if (!quiet) cat("Early stopping at iteration", iter, "\n")
            scores <- scores[1:iter]
            temperatures <- temperatures[1:iter]
            break
        }
    }

    # Finalize output
    output <- list(
        design_df = best_design,
        score = best_score,
        adjacency_score = calculate_adjacency_score_df(best_design, adjacency_list, swap),
        balance_score = calculate_balance_score_df(best_design, swap, spatial_cols),
        scores = scores,
        temperatures = temperatures,
        iterations_run = length(scores),
        stopped_early = length(scores) < iterations,
        treatments = stringi::stri_sort(unique(as.vector(treatments)), numeric = TRUE),
        seed = seed,
        adjacency_list = adjacency_list
    )

    class(output) <- c("design", class(output))
    return(output)
}

#' Find Adjacent Plots in Experimental Design
#'
#' @param data A data frame containing the experimental design with spatial coordinates
#' @param spatial_cols Character vector of column names containing spatial coordinates
#' @param method Method to determine adjacency: "coordinate" or "cardinal"
#' @param threshold Distance threshold for considering plots adjacent in "coordinate" method
#'
#' @return A list where each element corresponds to a plot and contains indices of adjacent plots
#'
#' @keywords internal
find_adjacent_plots <- function(data, spatial_cols, method = "coordinate", threshold = 1.5) {
    n <- nrow(data)
    adjacency_list <- vector("list", n)

    if (method == "coordinate") {
        # Extract coordinates
        coords <- as.matrix(data[, spatial_cols, drop = FALSE])

        # Calculate pairwise distances
        dist_matrix <- as.matrix(dist(coords))

        # For each plot, find adjacent plots based on distance threshold
        for (i in 1:n) {
            # Plots are adjacent if distance is <= threshold and not the same plot
            adjacency_list[[i]] <- which(dist_matrix[i, ] <= threshold & dist_matrix[i, ] > 0)
        }
    } else if (method == "cardinal") {
        # Assumes spatial_cols contains "row" and "col" for traditional grid layout
        if (!all(c("row", "col") %in% spatial_cols)) {
            stop("Cardinal adjacency method requires 'row' and 'col' in spatial_factors")
        }

        # Convert to numeric if needed
        row_vals <- as.numeric(as.character(data$row))
        col_vals <- as.numeric(as.character(data$col))

        # For each plot, find adjacent plots (up, down, left, right)
        for (i in 1:n) {
            row_i <- row_vals[i]
            col_i <- col_vals[i]

            # Find plots that are adjacent in cardinal directions
            adjacent_indices <- which(
                (row_vals == row_i & abs(col_vals - col_i) == 1) |  # left or right
                    (col_vals == col_i & abs(row_vals - row_i) == 1)    # up or down
            )

            adjacency_list[[i]] <- adjacent_indices
        }
    } else {
        stop("Invalid adjacency method. Use 'coordinate' or 'cardinal'.")
    }

    return(adjacency_list)
}

#' Generate a Neighbor Design by Swapping Treatments
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
generate_neighbor_df <- function(design, swap, swap_within, swap_count = 1, swap_all_blocks = FALSE) {
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
        block_indices <- which(design[[swap_within]] == block)

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

#' Default Objective Function for Design Optimization
#'
#' @param design Data frame containing the current design
#' @param adjacency_list List of adjacent plots
#' @param swap Column name of the treatment
#' @param spatial_cols Character vector of spatial factor column names
#' @param adjacency_weight Weight for adjacency score (default: 1)
#' @param balance_weight Weight for balance score (default: 1)
#'
#' @return Numeric score (lower is better)
#'
#' @export
objective_function <- function(adjacency_weight = 1, balance_weight = 1) {
    function(design, adjacency_list, swap, spatial_cols) {
        adj_score <- calculate_adjacency_score_df(design, adjacency_list, swap)
        bal_score <- calculate_balance_score_df(design, swap, spatial_cols)

        return(adjacency_weight * adj_score + balance_weight * bal_score)
    }
}

#' Calculate Adjacency Score for Design
#'
#' @param design Data frame containing the current design
#' @param adjacency_list List of adjacent plots
#' @param swap Column name of the treatment
#'
#' @return Numeric score for treatment adjacencies (lower is better)
#'
#' @keywords internal
calculate_adjacency_score_df <- function(design, adjacency_list, swap) {
    n <- nrow(design)
    treatment_vals <- design[[swap]]
    same_adjacent_count <- 0

    for (i in 1:n) {
        adjacent_indices <- adjacency_list[[i]]
        if (length(adjacent_indices) > 0) {
            # Count how many adjacent plots have the same treatment
            same_adjacent_count <- same_adjacent_count + sum(treatment_vals[adjacent_indices] == treatment_vals[i])
        }
    }

    # Divide by 2 because each pair is counted twice (once from each direction)
    return(same_adjacent_count / 2)
}

#' Calculate Balance Score for Design
#'
#' @param design Data frame containing the current design
#' @param swap Column name of the treatment
#' @param spatial_cols Character vector of spatial factor column names
#'
#' @return Numeric score for spatial balance (lower is better)
#'
#' @keywords internal
calculate_balance_score_df <- function(design, swap, spatial_cols) {
    balance_score <- 0
    treatments <- unique(design[[swap]])

    # Calculate balance for each spatial factor
    for (factor in spatial_cols) {
        factor_levels <- unique(design[[factor]])
        expected_count <- nrow(design) / (length(factor_levels) * length(treatments))

        # For each combination of treatment and factor level
        for (treatment in treatments) {
            for (level in factor_levels) {
                # Count occurrences of this treatment in this level
                actual_count <- sum(design[[swap]] == treatment & design[[factor]] == level)

                # Add squared deviation from expected count to balance score
                balance_score <- balance_score + (actual_count - expected_count)^2
            }
        }
    }

    return(balance_score)
}

#' Verify inputs for speed function
#'
#' @param data Data frame containing the initial design
#' @param swap Column name of the treatment to swap
#' @param swap_within Column name defining groups within which to swap treatments
#' @param spatial_factors Formula specifying spatial factors
#' @param iterations Maximum number of iterations
#' @param early_stop_iterations Number of iterations without improvement before early stopping
#' @param quiet Whether to suppress progress messages
#' @param seed Random seed for reproducibility
#' @param swap_count Number of swaps to perform in each iteration
#' @param swap_all_blocks Whether to perform swaps in all blocks
#' @param adaptive_swaps Whether to adapt number of swaps based on temperature
#' @param start_temp Starting temperature for simulated annealing
#' @param cooling_rate Cooling rate for temperature
#' @param adjacency_method Method to determine adjacent plots
#' @param adjacency_threshold Distance threshold for considering plots adjacent
#'
#' @return NULL (invisibly)
#'
#' @keywords internal
.verify_speed_inputs_df <- function(
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
        cooling_rate,
        adjacency_method,
        adjacency_threshold) {
    # Check data is a data frame
    if (!is.data.frame(data)) {
        stop("'data' must be a data frame")
    }

    # Check swap is a valid column
    if (!swap %in% names(data)) {
        stop(paste0("'", swap, "' is not a column in 'data'"))
    }

    # Check swap_within is a valid column or "1"/"none"
    if (!(swap_within %in% c("1", "none") || swap_within %in% names(data))) {
        stop(paste0("'", swap_within, "' is not a column in 'data' and not '1' or 'none'"))
    }

    # Check spatial_factors columns exist
    spatial_cols <- all.vars(spatial_factors)
    missing_cols <- spatial_cols[!spatial_cols %in% names(data)]
    if (length(missing_cols) > 0) {
        stop(paste0("The following spatial factor columns are missing from 'data': ",
                    paste(missing_cols, collapse = ", ")))
    }

    # Check adjacency method
    if (!adjacency_method %in% c("coordinate", "cardinal")) {
        stop("'adjacency_method' must be either 'coordinate' or 'cardinal'")
    }

    # Check numeric parameters
    if (!is.numeric(iterations) || iterations <= 0) {
        stop("'iterations' must be a positive number")
    }
    if (!is.numeric(early_stop_iterations) || early_stop_iterations <= 0) {
        stop("'early_stop_iterations' must be a positive number")
    }
    if (!is.numeric(adjacency_threshold) || adjacency_threshold <= 0) {
        stop("'adjacency_threshold' must be a positive number")
    }

    # Check logical parameters
    if (!is.logical(quiet)) {
        stop("'quiet' must be logical")
    }
    if (!is.logical(swap_all_blocks)) {
        stop("'swap_all_blocks' must be logical")
    }
    if (!is.logical(adaptive_swaps)) {
        stop("'adaptive_swaps' must be logical")
    }

    # Check seed is numeric or NULL
    if (!is.null(seed) && !is.numeric(seed)) {
        stop("'seed' must be numeric or NULL")
    }

    # Check temperature parameters
    if (!is.numeric(start_temp) || start_temp <= 0) {
        stop("'start_temp' must be a positive number")
    }
    if (!is.numeric(cooling_rate) || cooling_rate <= 0 || cooling_rate >= 1) {
        stop("'cooling_rate' must be between 0 and 1")
    }

    # Check swap_count
    if (!is.numeric(swap_count) || swap_count <= 0) {
        stop("'swap_count' must be a positive number")
    }

    return(invisible(NULL))
}


#' Print method for design objects
#'
#' @param x Design object returned by speed function
#' @param ... Additional arguments passed to print
#'
#' @return x invisibly
#'
#' @export
print.design <- function(x, ...) {
    cat("Optimized Experimental Design\n")
    cat("----------------------------\n")
    cat("Score:", x$score, "\n")
    cat("Adjacency Score:", x$adjacency_score, "\n")
    cat("Balance Score:", x$balance_score, "\n")
    cat("Iterations Run:", x$iterations_run, "\n")
    cat("Stopped Early:", x$stopped_early, "\n")
    cat("Treatments:", paste(x$treatments, collapse = ", "), "\n")
    cat("Seed:", x$seed, "\n\n")

    # Print first few rows of the optimized design
    cat("Optimized Design (first 6 rows):\n")
    print(head(x$design_df))

    return(invisible(x))
}
