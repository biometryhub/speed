#' Optimize Experimental Design Layout Using Simulated Annealing
#'
#' @description
#' Optimizes the spatial layout of experimental designs using simulated annealing to minimize
#' treatment adjacency and maintain treatment balance across spatial factors.
#'
#' @param data A data frame containing the initial design layout with row and col coordinates
#' @param permute A one-sided formula specifying the treatment variable to be permuted (e.g., ~treatment)
#' @param swap_within A one-sided formula specifying the blocking factor within which to permute treatments (default: ~1)
#' @param spatial_factors A one-sided formula specifying spatial factors to consider for balance (default: ~row + col)
#' @param iterations Maximum number of iterations for the simulated annealing algorithm (default: 10000)
#' @param early_stop_iterations Number of iterations without improvement before early stopping (default: 2000)
#' @param quiet Logical; if TRUE, suppresses progress messages (default: FALSE)
#' @param seed A numeric value for random seed. If provided, it ensures reproducibility of results (default: NULL).
#' @param swap_count Number of treatment swaps per iteration (default: 1)
#' @param swap_all_blocks Logical; if TRUE, performs swaps in all blocks at each iteration (default: FALSE)
#' @param adaptive_swaps Logical; if TRUE, adjusts swap parameters based on temperature (default: FALSE)
#' @param start_temp Starting temperature for simulated annealing (default: 100)
#' @param cooling_rate Rate at which temperature decreases (default: 0.99)
#' @param adj_weight Weight given to adjacency score in objective function (default: 1)
#' @param bal_weight Weight given to balance score in objective function (default: 1)
#'
#' @return A list containing:
#' \itemize{
#'   \item design - Matrix of optimized treatment layout
#'   \item design_df - Data frame of optimized design
#'   \item score - Final optimization score
#'   \item adjacency_score - Score for treatment adjacencies
#'   \item balance_score - Score for spatial balance
#'   \item scores - Vector of scores across iterations
#'   \item temperatures - Vector of temperatures across iterations
#'   \item iterations_run - Total number of iterations performed
#'   \item stopped_early - Logical indicating if optimization stopped early
#'   \item treatments - Vector of unique treatments
#'   \item seed - Random seed used for reproducibility of the design. If not set in the function, the seed is set to the second element of .Random.seed.
#' }
#'
#' @importFrom stringi stri_sort
#'
#' @examples
#' # Create a simple design with 3 replicates of 4 treatments in a 4x3 layout
#' df <- data.frame(
#'   row = rep(1:4, each = 3),
#'   col = rep(1:3, times = 4),
#'   Treatment = rep(LETTERS[1:4], 3)
#' )
#'
#' # Optimize the design
#' result <- speed(df, permute = ~Treatment)
#'
#' @export

speed <- function(data,
                  permute,
                  swap_within = ~ 1,
                  spatial_factors = ~ row + col,
                  iterations = 10000,
                  early_stop_iterations = 2000,
                  quiet = FALSE,
                  seed = NULL,

                  # These could probably be options
                  swap_count = 1,
                  swap_all_blocks = FALSE,
                  adaptive_swaps = FALSE,
                  start_temp = 100,
                  cooling_rate = 0.99,
                  adj_weight = 1,
                  bal_weight = 1) {

# Permute is for the levels of the treatment that get shuffled within the levels of the swap_within factor
# E.g. swap_within = ~block will permute treatments within blocks, rather than the entire layout
# E.g. permute = ~treatment will permute the levels of treatment within the blocks

    if (!is.data.frame(data))
        stop("df must be an initial data frame of the design")
    layout_df <- data
    if(!inherits(permute, "formula"))
        stop("permute must be a one sided formula")
    permute_var <- deparse(permute[[2]])
    if (!(permute_var %in% names(layout_df)))
        stop("permute column not found in data frame")
    permute_vals <- layout_df[[permute_var]]
    if(!inherits(swap_within, "formula"))
        stop("swap_within must be a one sided formula")
    swap_var <- deparse(swap_within[[2]])
    if(swap_var == "1")
        swap_vals <- factor(rep(1, nrow(data)))
    else if (!(swap_var %in% names(layout_df)))
        stop("swap column not found in data frame")
    else swap_vals <- layout_df[[swap_var]]
    if(!inherits(spatial_factors, "formula"))
        stop("spatial_factors must be a one sided formula")
    spatial_fac <- all.vars(spatial_factors)
    if (!all(spatial_fac %in% names(layout_df)))
        stop("One or more spatial_factors not found in data frame")

    # Derive dimensions
    row <- as.integer(as.character(layout_df$row))
    col <- as.integer(as.character(layout_df$col))
    nrows <- max(row)
    ncols <- max(col)
    permute_mat <- matrix(permute_vals, nrow = nrows, ncol = ncols, byrow = FALSE)
    swap_mat <- matrix(swap_vals, nrow = nrows, ncol = ncols, byrow = FALSE)
    treatments <- unique(as.vector(permute_mat))

    # Set seed for reproducibility
    if(is.null(seed)) {
        gen_seed <- runif(1)
        seed <- .Random.seed[2]
    }
    set.seed(seed)

    perm_design <- function(permute_mat, swap_mat) {
        design <- matrix(NA, nrow = nrows, ncol = ncols)
        if (is.null(swap_mat)) {
            design[] <- sample(as.vector(permute_mat))
        } else {
            for (level in unique(as.vector(swap_mat))) {
                mask <- which(swap_mat == level, arr.ind = TRUE)
                vals <- permute_mat[mask]
                design[mask] <- sample(vals)
            }
        }
        return(design)
    }
    current_design <- perm_design(permute_mat, swap_mat)
    best_design <- current_design

    generate_neighbor <- function(design, swap_mat, swap_count, swap_all_blocks) {
        new_design <- design
        swap_levels <- unique(as.vector(swap_mat))
        if(!swap_all_blocks) swap_levels <- sample(swap_levels, 1)
        for (level in swap_levels) {
            for (i in 1:swap_count) {
                level <- sample(swap_levels, 1)
                positions <- which(swap_mat == level, arr.ind = TRUE)
                if (nrow(positions) < 2) next
                idx <- sample(1:nrow(positions), 2)
                pos1 <- positions[idx[1], ]; pos2 <- positions[idx[2], ]
                tmp <- new_design[pos1[1], pos1[2]]
                new_design[pos1[1], pos1[2]] <- new_design[pos2[1], pos2[2]]
                new_design[pos2[1], pos2[2]] <- tmp
            }
        }
        return(new_design)
    }

    current_score <- calculate_objective(current_design, permute_var, layout_df, spatial_fac)
    best_score <- current_score
    temp <- start_temp
    scores <- numeric(iterations)
    temperatures <- numeric(iterations)
    last_improvement_iter <- 0
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
        new_design <- generate_neighbor(current_design, swap_mat, current_swap_count, current_swap_all_blocks)
        new_score <- calculate_objective(new_design, permute_var, layout_df, spatial_fac)
        if (new_score < current_score || runif(1) < exp((current_score - new_score) / temp)) {
            current_design <- new_design
            current_score <- new_score
            if (new_score < best_score) {
                best_design <- new_design
                best_score <- new_score
                last_improvement_iter <- iter
            }
        }
        temp <- temp * cooling_rate
        if (!quiet && iter %% 1000 == 0) {
            cat("Iteration:", iter, "Score:", current_score, "Best:", best_score,
                "Since Improvement:", iter - last_improvement_iter, "\n")
        }
        if (iter - last_improvement_iter >= early_stop_iterations) {
            if (!quiet) cat("Early stopping at iteration", iter, "\n")
            scores <- scores[1:iter]
            temperatures <- temperatures[1:iter]
            break
        }
    }
    design_df <- layout_df
    design_df[[permute_var]] <- as.vector(best_design)
    return(list(
        design = best_design,
        design_df = design_df,
        score = best_score,
        adjacency_score = calculate_adjacency_score(best_design),
        balance_score = calculate_balance_score(design_df, permute_var, spatial_fac),
        scores = scores,
        temperatures = temperatures,
        iterations_run = length(scores),
        stopped_early = length(scores) < iterations,
        treatments = stringi::stri_sort(treatments, numeric = TRUE),
        seed = seed
    ))
}
