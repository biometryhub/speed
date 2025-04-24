#' Optimize Experimental Design Layout Using Simulated Annealing
#'
#' @description
#' Optimizes the spatial layout of experimental designs using simulated annealing to minimize
#' treatment adjacency and maintain treatment balance across spatial factors.
#'
#' @param data A data frame containing the initial design layout with row and col coordinates
#' @param treatment_cols A string specifying the treatment variable to be swapped (e.g., `"treatment"`)
#' @param swap_within A string specifying the blocking variable that is a boundary within which to swap
#'   treatments. Specify `"1"` or `"none"` for no boundary (default: `"1"`)
#' @param spatial_factors A one-sided formula specifying spatial factors to consider for balance (default:
#'   `~row + col`)
#' @param iterations Maximum number of iterations for the simulated annealing algorithm (default: 10000)
#' @param early_stop_iterations Number of iterations without improvement before early stopping (default: 2000)
#' @param objective_function Objective function used to calculate score (lower is better) (default:
#'   \link{objective_function_default})
#' @param quiet Logical; if TRUE, suppresses progress messages (default: FALSE)
#' @param seed A numeric value for random seed. If provided, it ensures reproducibility of results (default:
#'   NULL).
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
#' result <- speed(df, treatment_cols = "treatment")
#'
#' @export
speed <- function(
    data,
    treatment_cols,
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 10000,
    early_stop_iterations = 2000,
    objective_function = objective_function_default(),
    quiet = FALSE,
    seed = NULL
    # These could probably be options
    # swap_count = 1,
    # swap_all_blocks = FALSE,
    # adaptive_swaps = FALSE,
    # start_temp = 100,
    # cooling_rate = 0.99,
    ) {
  # Permute is for the levels of the treatment that get shuffled within the levels of the swap_within factor
  # E.g. swap_within = ~block will permute treatments within blocks, rather than the entire layout
  # E.g. permute = ~treatment will permute the levels of treatment within the blocks
  # TODO: convert to doc for options
  # swap_count Number of treatment swaps per iteration (default: 1)
  # swap_all_blocks Logical; if TRUE, performs swaps in all blocks at each iteration (default: FALSE)
  # adaptive_swaps Logical; if TRUE, adjusts swap parameters based on temperature (default: FALSE)
  # start_temp Starting temperature for simulated annealing (default: 100)
  # cooling_rate Rate at which temperature decreases (default: 0.99)
  # NOTE: weights moved to cost function
  swap_count <- getOption("speed.swap_count", 1)
  swap_all_blocks <- getOption("speed.swap_all_blocks", FALSE)
  adaptive_swaps <- getOption("speed.adaptive_swaps", FALSE)
  start_temp <- getOption("speed.start_temp", 100)
  cooling_rate <- getOption("speed.cooling_rate", 0.99)

  .verify_speed_inputs(
    data,
    treatment_cols,
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

  # currently support only 1 constraint
  layout_df <- data
  if (swap_within == "1") {
    swap_vals <- factor(rep(1, nrow(data)))
  } else {
    swap_vals <- layout_df[[swap_within]]
  }
  spatial_cols <- all.vars(spatial_factors)
  treatments <- layout_df[[treatment_cols]]

  # set up matrices
  # NOTE: force user to provide row and col
  row <- as.integer(as.character(layout_df$row))
  col <- as.integer(as.character(layout_df$col))
  nrows <- max(row)
  ncols <- max(col)
  treatment_matrix <- matrix(treatments, nrow = nrows, ncol = ncols, byrow = FALSE)
  swap_matrix <- matrix(swap_vals, nrow = nrows, ncol = ncols, byrow = FALSE)

  # Set seed for reproducibility
  if (is.null(seed)) {
    seed <- .Random.seed[2]
  }
  set.seed(seed)

  current_design <- initialize_design_matrix(treatment_matrix, swap_matrix)
  best_design <- current_design

  current_score <- objective_function(current_design, layout_df, treatment_cols, spatial_cols)
  # TODO: somehow move this to `.verify_speed_inputs`
  if (!is.numeric(current_score)) {
    stop("Value from `objective_function` must be numeric.")
  }

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

    new_design <- generate_neighbor(current_design, swap_matrix, current_swap_count, current_swap_all_blocks)
    new_score <- objective_function(new_design, layout_df, treatment_cols, spatial_cols)
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
      cat(
        "Iteration:", iter, "Score:", current_score, "Best:", best_score,
        "Since Improvement:", iter - last_improvement_iter, "\n"
      )
    }
    if (iter - last_improvement_iter >= early_stop_iterations) {
      if (!quiet) cat("Early stopping at iteration", iter, "\n")
      scores <- scores[1:iter]
      temperatures <- temperatures[1:iter]
      break
    }
  }

  design_df <- layout_df
  design_df[[treatment_cols]] <- as.vector(best_design)

  return(list(
    design = best_design,
    design_df = design_df,
    score = best_score,
    adjacency_score = calculate_adjacency_score(best_design),
    balance_score = calculate_balance_score(design_df, treatment_cols, spatial_cols),
    scores = scores,
    temperatures = temperatures,
    iterations_run = length(scores),
    stopped_early = length(scores) < iterations,
    treatments = stringi::stri_sort(unique(as.vector(treatments)), numeric = TRUE),
    seed = seed
  ))
}


#' Verify Inputs for `speed`
#'
#' @description
#' Verify inputs for the `speed` function.
#'
#' @param data A data frame containing the initial design layout with row and col coordinates
#' @param treatment_cols A one-sided formula specifying the treatment variable to be permuted (e.g.,
#'   `~treatment`)
#' @param swap_within A one-sided formula specifying the blocking factor within which to permute treatments
#'   (default: `~1`)
#' @param spatial_factors A one-sided formula specifying spatial factors to consider for balance (default:
#'   `~row + col`)
#' @param iterations Maximum number of iterations for the simulated annealing algorithm (default: 10000)
#' @param early_stop_iterations Number of iterations without improvement before early stopping (default: 2000)
#' @param quiet Logical; if TRUE, suppresses progress messages (default: FALSE)
#' @param seed A numeric value for random seed. If provided, it ensures reproducibility of results (default:
#'   NULL).
#' @param swap_count Number of treatment swaps per iteration (default: 1)
#' @param swap_all_blocks Logical; if TRUE, performs swaps in all blocks at each iteration (default: FALSE)
#' @param adaptive_swaps Logical; if TRUE, adjusts swap parameters based on temperature (default: FALSE)
#' @param start_temp Starting temperature for simulated annealing (default: 100)
#' @param cooling_rate Rate at which temperature decreases (default: 0.99)
#'
#' @keywords internal
.verify_speed_inputs <- function(
    data,
    treatment_cols,
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
    cooling_rate) {
  if (!is.data.frame(data)) {
    stop("`data` must be an initial data frame of the design")
  }

  for (col in treatment_cols) {
    if (!(col %in% names(data))) {
      .not_found_in_cols_error(col, data, "treatment")
    }
  }

  # currently support only 1 constraint
  if (swap_within != "1") {
    for (col in swap_within) {
      if (!(col %in% names(data))) {
        .not_found_in_cols_error(col, data, "constraint")
      }
    }
  }

  if (!inherits(spatial_factors, "formula")) {
    stop("spatial_factors must be a one sided formula")
  }

  for (col in all.vars(spatial_factors)) {
    if (!(col %in% names(data))) {
      .not_found_in_cols_error(col, data, "spatial factor")
    }
  }

  verify_positive_whole_number(iterations, early_stop_iterations, swap_count)
  verify_non_negative_whole(start_temp)
  verify_boolean(quiet, adaptive_swaps, swap_all_blocks)
  verify_between(cooling_rate, lower = 0, upper = 1, upper_exclude = TRUE)
  if (!is.null(seed)) {
    verify_between(seed, lower = -.Machine$integer.max, upper = .Machine$integer.max)
  }
}

#' Not Found in Columns Error
#'
#' @description
#' Ipsum lorem
#'
#' @param col
#' @param data
#' @param prefix
#'
#' @keywords internal
.not_found_in_cols_error <- function(col, data, prefix) {
  stop(paste0(prefix, ' "', col, '" not found in data frame columns: ', names(data), collapse = ", "))
}
