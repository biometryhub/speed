#' Optimise Experimental Design Layout Using Simulated Annealing
#'
#' @description
#' Optimises the spatial layout of experimental designs using simulated annealing to minimize
#' treatment adjacency and maintain treatment balance across spatial factors. Works with
#' regular or irregular spatial designs.
#'
#' @param data A data frame containing the experimental design with spatial coordinates
#' @param swap A column name of the items to be swapped (e.g., `treatment`, `variety`, `genotype`, etc)
#' @param swap_within A string specifying the variable that defines a boundary within which to swap
#'   treatments. Specify `"1"` or `"none"` for no boundary (default: `"1"`). Other examples might be `"block"`
#'   or `"replicate"` or even `"site"`.
#' @param spatial_factors A one-sided formula specifying spatial factors to consider for balance (default:
#'   `~row + col`)
#' @param iterations Maximum number of iterations for the simulated annealing algorithm (default: 10000)
#' @param early_stop_iterations Number of iterations without improvement before early stopping (default: 2000)
#' @param obj_function Objective function used to calculate score (lower is better) (default:
#'   \link{objective_function})
#' @param quiet Logical; if TRUE, suppresses progress messages (default: FALSE)
#' @param seed A numeric value for random seed. If provided, it ensures reproducibility of results (default:
#'   NULL).
#' @param ... Other arguments passed through to objective functions.
#'
#' @return A list containing:
#' \itemize{
#'   \item design_df - Data frame of optimised design
#'   \item score - Final optimization score
#'   \item adjacency_score - Score for treatment adjacencies
#'   \item balance_score - Score for spatial balance
#'   \item scores - Vector of scores across iterations
#'   \item temperatures - Vector of temperatures across iterations
#'   \item iterations_run - Total number of iterations performed
#'   \item stopped_early - Logical indicating if optimization stopped early
#'   \item treatments - Vector of unique treatments
#'   \item seed - Random seed used for reproducibility of the design. If not set in the function, the seed is
#'      set to the third element of .Random.seed.
#' }
#'
#' @importFrom stringi stri_sort
#' @importFrom stats runif
#' @importFrom rlang check_dots_used
#'
#' @examples
#' # Create a simple design with 3 replicates of 4 treatments in a 4x3 layout
#' df <- data.frame(
#'   row = rep(1:4, each = 3),
#'   col = rep(1:3, times = 4),
#'   treatment = rep(LETTERS[1:4], 3)
#' )
#'
#' # Optimise the design
#' result <- speed(df, swap = "treatment")
#'
#' @export
# fmt: skip
speed <- function(data,
                  swap,
                  swap_within = "1",
                  spatial_factors = ~ row + col,
                  iterations = 10000,
                  early_stop_iterations = 2000,
                  obj_function = objective_function,
                  quiet = FALSE,
                  seed = NULL,
                  ...) {
  # Extract options
  swap_count <- getOption("speed.swap_count", 1)
  swap_all_blocks <- getOption("speed.swap_all_blocks", FALSE)
  adaptive_swaps <- getOption("speed.adaptive_swaps", FALSE)
  start_temp <- getOption("speed.start_temp", 100)
  cooling_rate <- getOption("speed.cooling_rate", 0.99)

  swap <- as.character(substitute(swap))
  swap_within <- as.character(substitute(swap_within))

  # Verify inputs
  .verify_speed_inputs(data,
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
                       cooling_rate)
  rlang::check_dots_used()

  # Handle swap_within
  layout_df <- data
  if (swap_within == "1" || swap_within == "none") {
    layout_df$swap_group <- factor(rep(1, nrow(data)))
    swap_within <- "swap_group"
  }

  spatial_cols <- all.vars(spatial_factors)
  treatments <- layout_df[[swap]]

  # Initialize design
  current_design <- layout_df
  best_design <- current_design

  # Calculate initial score
  current_score_obj <- obj_function(current_design, swap, spatial_cols, ...)
  current_score <- current_score_obj$score
  if (!is.numeric(current_score)) {
    stop("`$score` from `objective_function` must be numeric.")
  }

  best_score_obj <- current_score_obj
  best_score <- current_score
  temp <- start_temp
  scores <- numeric(iterations)
  temperatures <- numeric(iterations)
  last_improvement_iter <- 0

  # Set seed for reproducibility
  if (is.null(seed)) {
    # dummy_seed <- runif(1)
    seed <- .GlobalEnv$.Random.seed[3]
  }
  set.seed(seed)

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
    new_design <- generate_neighbour(
      current_design,
      swap,
      swap_within,
      swap_count = current_swap_count,
      swap_all_blocks = current_swap_all_blocks
    )

    # Calculate new score
    new_score_obj <- obj_function(new_design$design,
                                  swap,
                                  spatial_cols,
                                  current_score_obj = current_score_obj,
                                  swapped_items = new_design$swapped_items,
                                  ...)
    new_score <- new_score_obj$score

    # Decide whether to accept the new design
    if (new_score < current_score || runif(1) < exp((current_score - new_score) / temp)) {
      current_design <- new_design$design
      current_score <- new_score
      current_score_obj <- new_score_obj
      if (new_score < best_score) {
        best_design <- new_design$design
        best_score_obj <- new_score_obj
        best_score <- new_score
        last_improvement_iter <- iter
      }
    }

    # Cool temperature
    temp <- temp * cooling_rate

    # Progress reporting
    if (!quiet && iter %% 1000 == 0) {
      cat(
        "Iteration:",
        iter,
        "Score:",
        current_score,
        "Best:",
        best_score,
        "Since Improvement:",
        iter - last_improvement_iter,
        "\n"
      )
    }

    # Early stopping
    if (iter - last_improvement_iter >= early_stop_iterations || new_score == 0) {
      if (!quiet) cat("Early stopping at iteration", iter, "\n")
      scores <- scores[1:iter]
      temperatures <- temperatures[1:iter]
      break
    }
  }

  if (!is.null(best_design$swap_group)) {
    best_design$swap_group <- NULL
  }

  # Finalize output
  output <- list(
    design_df = best_design,
    score = best_score,
    scores = scores,
    temperatures = temperatures,
    iterations_run = length(scores),
    stopped_early = length(scores) < iterations,
    treatments = stringi::stri_sort(unique(as.vector(treatments)), numeric = TRUE),
    seed = seed
  )

  class(output) <- c("design", class(output))
  return(output)
}


#' Print method for speed design objects
#'
#' @param x Design object returned by speed function
#' @param ... Additional arguments passed to print
#'
#' @importFrom utils head
#'
#' @return x invisibly
#'
#' @export
print.design <- function(x, ...) {
  cat("Optimised Experimental Design\n")
  cat("----------------------------\n")
  cat("Score:", x$score, "\n")
  # cat("Adjacency Score:", x$adjacency_score, "\n")
  # cat("Balance Score:", x$balance_score, "\n")
  cat("Iterations Run:", x$iterations_run, "\n")
  cat("Stopped Early:", x$stopped_early, "\n")
  cat("Treatments:", paste(x$treatments, collapse = ", "), "\n")
  cat("Seed:", x$seed, "\n\n")

  # Print first few rows of the optimised design
  # cat("Optimised Design (first 6 rows):\n")
  # print(utils::head(x$design_df))

  return(invisible(x))
}
