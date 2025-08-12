#' Optimise Experimental Design Layout Using Simulated Annealing
#'
#' @description
#' Optimises the spatial layout of experimental designs using simulated
#' annealing to minimise treatment adjacency and maintain treatment balance
#' across spatial factors. Works with regular or irregular spatial designs.
#'
#' @param data A data frame containing the experimental design with spatial
#' coordinates
#' @param swap A column name of the items to be swapped (e.g., `treatment`,
#'   `variety`, `genotype`, etc). For hierarchical designs, provide a named 
#'   list where each name corresponds to a hierarchy level (e.g., 
#'   `list(wp = "wholeplot_treatment", sp = "subplot_treatment")`). 
#'   See details for more information.
#' @param swap_within A string specifying the variable that defines a boundary
#'   within which to swap treatments. Specify `"1"` or `"none"` for no boundary
#'   (default: `"1"`). Other examples might be `"block"` or `"replicate"` or
#'   even `"site"`. For hierarchical designs, provide a named list with names 
#'   matching `swap` to optimise a hierarchical design such as a split-plot. 
#'   See details for more information.
#' @param spatial_factors A one-sided formula specifying spatial factors to
#'   consider for balance (default: `~row + col`).
#' @param iterations Maximum number of iterations for the simulated annealing
#'   algorithm (default: 10000). For hierarchical designs, can be a named list 
#'   with names matching `swap`.
#' @param early_stop_iterations Number of iterations without improvement before
#'   early stopping (default: 2000). For hierarchical designs, can be a named 
#'   list with names matching `swap`.
#' @param obj_function Objective function used to calculate score (lower is
#'   better) (default: [objective_function()]). For hierarchical designs, can 
#'   be a named list with names matching `swap`.
#' @param quiet Logical; if TRUE, suppresses progress messages (default: FALSE)
#' @param seed A numeric value for random seed. If provided, it ensures
#'   reproducibility of results (default: `NULL`).
#' @param ... Other arguments passed through to objective functions.
#'
#' @returns A list containing:
#' - **design_df** - Data frame of optimised design
#' - **score** - Final optimisation score
#' - **scores** - Vector of scores across iterations (for simple designs) or 
#'   named list of score vectors (for hierarchical designs)
#' - **temperatures** - Vector of temperatures across iterations
#' - **iterations_run** - Total number of iterations performed
#' - **stopped_early** - Logical indicating if optimisation stopped early 
#'   (for simple designs) or named logical vector (for hierarchical designs)
#' - **treatments** - Vector of unique treatments (for simple designs) or 
#'   named list of treatment vectors (for hierarchical designs)
#' - **seed** - Random seed used for reproducibility of the design. If not set
#'   in the function, the seed is set to the third element of `.Random.seed`.
#'
#' @details
#' This function provides a very general interface for producing experimental
#' designs of different types. For hierarchical designs such as split-plots,
#' strip plots, split-split plots and similar nested structures, the key
#' arguments (`swap`, `swap_within`, `iterations`, `early_stop_iterations` and
#' `obj_function`) can be provided as named lists where each name corresponds
#' to a level in the hierarchy (e.g., "wholeplot", "subplot"). When these
#' arguments are provided as named lists with matching names, the optimisation
#' is applied sequentially at each hierarchical level, starting with the first
#' level and progressing through the hierarchy. This allows for different
#' optimisation parameters and objective functions to be applied at different
#' levels of the design structure. For simple (non-hierarchical) designs, these
#' arguments can be provided as single values. For more examples and detailed
#' usage, see the package vignettes.
#'
#'
#' @importFrom stringi stri_sort
#' @importFrom stats runif
#' @importFrom rlang check_dots_used
#'
#' @examples
#' # Create a simple design with 3 replicates of 4 treatments in a 4x3 layout
#' df <- data.frame(
#'   row = rep(1:4, times = 5),
#'   col = rep(1:5, each = 4),
#'   treatment = rep(LETTERS[1:4], 5)
#' )
#'
#' # Optimise the design
#' result <- speed(df, swap = "treatment", seed = 42)
#' autoplot(result)
#'
#' # Hierarchical split-plot design
#' df_split <- data.frame(
#'   row = rep(1:12, each = 4),
#'   col = rep(1:4, times = 12),
#'   block = rep(1:4, each = 12),
#'   wholeplot = rep(1:12, each = 4),
#'   wholeplot_treatment = rep(rep(LETTERS[1:3], each = 4), times = 4),
#'   subplot_treatment = rep(letters[1:4], 12)
#' )
#'
#' result <- speed(df_split,
#'                 swap = list(wp = "wholeplot_treatment",
#'                             sp = "subplot_treatment"),
#'                 swap_within = list(wp = "block", sp = "wholeplot"),
#'                 seed = 42)
#'
#' # Plot wholeplot allocations within blocks
#' autoplot(result, treatments = "wholeplot_treatment")
#' # Plot subplot allocations within wholeplots
#' autoplot(result, treatments = "subplot_treatment", block = "wholeplot")
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
  rlang::check_dots_used()

  # Check if this is a hierarchical design
  is_hierarchical <- is.list(swap) && !is.null(names(swap))

  # Infer row and column columns
  inferred <- infer_row_col(data)
  row_column <- inferred$row
  col_column <- inferred$col

  # If row and column columns are not inferred, set adj_weight to 0
  if (!inferred$inferred) {
    old_options <- options()
    on.exit(options(old_options), add = TRUE)
    options(speed.adj_weight = 0)
  } else {
    # Sort the data frame to start with to ensure consistency in calculating the adjacency later
    data <- data[do.call(order, data[c(row_column, col_column)]), ]
  }

  if (is_hierarchical) {
    return(speed_hierarchical(data, swap, swap_within, spatial_factors,
                              iterations, early_stop_iterations, obj_function,
                              quiet, seed, row_column = row_column, col_column = col_column, ...))
  } else {
    # Convert swap and swap_within to character if they are not already - NSE
    # swap <- as.character(substitute(swap))
    # swap <- eval(substitute(swap), envir = parent.frame())
    swap <- wrappable_nse(swap)
    swap_within <- wrappable_nse(swap_within)

    return(speed_simple(data, swap, swap_within, spatial_factors,
                        iterations, early_stop_iterations, obj_function,
                        quiet, seed, row_column = row_column, col_column = col_column, ...))
  }
}

#' Speed function for simple (non-hierarchical) designs
#' @keywords internal
# fmt: skip
speed_simple <- function(data,
                         swap,
                         swap_within,
                         spatial_factors,
                         iterations,
                         early_stop_iterations,
                         obj_function,
                         quiet,
                         seed,
                         ...) {
  # Extract options
  swap_count <- getOption("speed.swap_count", 1)
  swap_all_blocks <- getOption("speed.swap_all_blocks", FALSE)
  adaptive_swaps <- getOption("speed.adaptive_swaps", FALSE)
  start_temp <- getOption("speed.start_temp", 100)
  cooling_rate <- getOption("speed.cooling_rate", 0.99)
  random_initialisation <- getOption("speed.random_initialisation", FALSE)

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
                       cooling_rate,
                       random_initialisation)
  # Handle swap_within
  layout_df <- data
  if (swap_within == "1" || swap_within == "none") {
    layout_df$swap_group <- factor(rep(1, nrow(data)))
    swap_within <- "swap_group"
  }

  # Set seed for reproducibility
  if (is.null(seed)) {
    # dummy_seed <- runif(1)
    seed <- .GlobalEnv$.Random.seed[3]
  }
  set.seed(seed)

  # random initialisation
  if (random_initialisation) {
    layout_df <- shuffle_items(layout_df, swap, swap_within, seed)
  }

  spatial_cols <- all.vars(spatial_factors)
  treatments <- layout_df[[swap]]

  # Initialise design
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

  # Main optimisation loop
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
      level = NULL,
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
      # Record final score and temperature before breaking
      if (iter < iterations) {
        scores[iter + 1] <- current_score
        temperatures[iter + 1] <- temp
        scores <- scores[1:(iter + 1)]
        temperatures <- temperatures[1:(iter + 1)]
      } else {
        scores <- scores[1:iter]
        temperatures <- temperatures[1:iter]
      }
      break
    }
  }

  if (!is.null(best_design$swap_group)) {
    best_design$swap_group <- NULL
  }

  # Finalise output
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

#' Speed function for hierarchical designs
#' @keywords internal
# fmt: skip
speed_hierarchical <- function(data,
                               swap,
                               swap_within,
                               spatial_factors,
                               iterations,
                               early_stop_iterations,
                               obj_function,
                               quiet,
                               seed,
                               ...) {
  # Extract options
  swap_count <- getOption("speed.swap_count", 1)
  swap_all_blocks <- getOption("speed.swap_all_blocks", FALSE)
  adaptive_swaps <- getOption("speed.adaptive_swaps", FALSE)
  start_temp <- getOption("speed.start_temp", 100)
  cooling_rate <- getOption("speed.cooling_rate", 0.99)

  # Handle hierarchical parameters
  hierarchy_levels <- names(swap)

  # Convert single values to named lists for all levels
  if (!is.list(iterations)) {
    iterations <- setNames(rep(iterations, length(hierarchy_levels)), hierarchy_levels)
  }
  if (!is.list(early_stop_iterations)) {
    early_stop_iterations <- setNames(rep(early_stop_iterations, length(hierarchy_levels)), hierarchy_levels)
  }
  if (!is.list(obj_function)) {
    obj_function <- setNames(rep(list(obj_function), length(hierarchy_levels)), hierarchy_levels)
  }

  # Handle swap_within defaults
  if (is.character(swap_within) && (swap_within == "1" || swap_within == "none")) {
    swap_within <- setNames(rep("1", length(hierarchy_levels)), hierarchy_levels)
  }

  # Verify hierarchical inputs
  .verify_hierarchical_inputs(data, swap, swap_within, spatial_factors,
                              iterations, early_stop_iterations, obj_function,
                              quiet, seed)

  layout_df <- data
  spatial_cols <- all.vars(spatial_factors)

  # Handle swap_within for each level
  for (level in hierarchy_levels) {
    if (swap_within[[level]] == "1" || swap_within[[level]] == "none") {
      layout_df[[paste0("swap_group_", level)]] <- factor(rep(1, nrow(data)))
      swap_within[[level]] <- paste0("swap_group_", level)
    }
  }

  # Initialise design
  current_design <- layout_df
  best_design <- current_design

  # Set seed for reproducibility
  if (is.null(seed)) {
    seed <- .GlobalEnv$.Random.seed[3]
  }
  set.seed(seed)

  # Sequential optimisation for each hierarchy level
  all_scores <- list()
  all_temperatures <- list()
  total_iterations <- 0  # TODO: Track total iterations across all levels

  for (level in hierarchy_levels) {
    if (!quiet) cat("Optimising level:", level, "\n")

    # Calculate initial score for this level
    current_score_obj <- obj_function[[level]](current_design, swap[[level]], spatial_cols, ...)
    current_score <- current_score_obj$score

    if (!is.numeric(current_score)) {
      stop("`score` from `objective_function` must be numeric.")
    }

    best_score_obj <- current_score_obj
    best_score <- current_score
    temp <- start_temp
    scores <- numeric(iterations[[level]])
    temperatures <- numeric(iterations[[level]])
    last_improvement_iter <- 0

    # Optimisation loop for this level
    for (iter in 1:iterations[[level]]) {
      scores[iter] <- current_score
      temperatures[iter] <- temp

      if (adaptive_swaps) {
        current_swap_count <- max(1, round(swap_count * temp / start_temp))
        current_swap_all_blocks <- runif(1) < (temp / start_temp) && swap_all_blocks
      } else {
        current_swap_count <- swap_count
        current_swap_all_blocks <- swap_all_blocks
      }

      # Generate new design by swapping treatments at this level
      new_design <- generate_neighbour(
        current_design,
        swap,
        swap_within,
        level,
        swap_count = current_swap_count,
        swap_all_blocks = current_swap_all_blocks
      )

      # Calculate new score
      new_score_obj <- obj_function[[level]](new_design$design,
                                             swap[[level]],
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
          "Level:", level,
          "Iteration:", iter,
          "Score:", current_score,
          "Best:", best_score,
          "Since Improvement:", iter - last_improvement_iter,
          "\n"
        )
      }

      # Early stopping
      if (iter - last_improvement_iter >= early_stop_iterations[[level]] || new_score < .Machine$double.eps) {
        if (!quiet) cat("Early stopping at iteration", iter, "for level", level, "\n")
        # Record final score and temperature before breaking
        if (iter < iterations[[level]]) {
          scores[iter + 1] <- current_score
          temperatures[iter + 1] <- temp
          scores <- scores[1:(iter + 1)]
          temperatures <- temperatures[1:(iter + 1)]
        } else {
          scores <- scores[1:iter]
          temperatures <- temperatures[1:iter]
        }
        break
      }
    }

    all_scores[[level]] <- scores
    all_temperatures[[level]] <- temperatures
    total_iterations <- total_iterations + length(scores)
  }

  # Collect and set up output and results
  treatments <- list()
  level_scores <- numeric()
  for (level in hierarchy_levels) {
    # Clean up temporary swap_group columns
    col_name <- paste0("swap_group_", level)
    if (col_name %in% names(best_design)) {
      best_design[[col_name]] <- NULL
    }

    # Collect treatments for each level
    treatments[[level]] <- stringi::stri_sort(unique(as.vector(best_design[[swap[[level]]]])), numeric = TRUE)

    # Calculate final combined score
    level_scores[level] <- obj_function[[level]](best_design, swap[[level]], spatial_cols, ...)$score
  }

  # Check which levels stopped early
  stopped_early <- sapply(hierarchy_levels, function(level) {
    length(all_scores[[level]]) < iterations[[level]]
  })
  names(stopped_early) <- hierarchy_levels

  # Finalise output
  output <- list(
    design_df = best_design,
    score = sum(level_scores),
    scores = all_scores,
    temperatures = all_temperatures,
    iterations_run = total_iterations,
    stopped_early = stopped_early,
    treatments = treatments,
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
  cat("Iterations Run:", x$iterations_run, "\n")
  cat("Stopped Early:", x$stopped_early, "\n")

  # Handle treatments display for hierarchical vs simple designs
  if (is.list(x$treatments)) {
    # Hierarchical design - show each level with its name
    cat("Treatments:\n")
    for (level_name in names(x$treatments)) {
      cat("  ", level_name, ": ", paste(x$treatments[[level_name]], collapse = ", "), "\n", sep = "")
    }
  } else {
    # Simple design - show treatments as before
    cat("Treatments:", paste(x$treatments, collapse = ", "), "\n")
  }

  cat("Seed:", x$seed, "\n\n")

  return(invisible(x))
}
