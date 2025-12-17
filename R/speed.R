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
#' @param grid_factors A named list specifying grid factors to construct a
#'   matrix for calculating adjacency score, `dim1` for row and `dim2` for
#'   column. (default: `list(dim1 = "row", dim2 = "col")`).
#' @param iterations Maximum number of iterations for the simulated annealing
#'   algorithm (default: 10000). For hierarchical designs, can be a named list
#'   with names matching `swap`.
#' @param early_stop_iterations Number of iterations without improvement before
#'   early stopping (default: 2000). For hierarchical designs, can be a named
#'   list with names matching `swap`.
#' @param obj_function Objective function used to calculate score (lower is
#'   better) (default: [objective_function()]). For hierarchical designs, can
#'   be a named list with names matching `swap`.
#' @param swap_all Logical; Whether to swap all matching items or a single item
#'   at a time (default: FALSE)
#' @param optimise A list of named arguments describing optimising parameters;
#'   see more in example.
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
#'                 swap_all = TRUE,
#'                 seed = 42)
#'
#' # Plot wholeplot allocations within blocks
#' autoplot(result, treatments = "wholeplot_treatment")
#' # Plot subplot allocations within wholeplots
#' autoplot(result, treatments = "subplot_treatment", block = "wholeplot")
#'
#' # Using optimise parameter
#' # Multi-Environment Trial (MET) design
#' # With 7 replicates of 100 lines in this design, each line will appear
#' # twice at two different sites and once at the rest of the sites.
#' lines <- rep(1:100, 7)
#' df_site <- initialise_design_df(1, 28, 5, 14, 5)
#' df_initial <- rbind(df_site, df_site, df_site, df_site, df_site)
#' df_initial$lines <- lines
#' df_initial$site <- rep(c("a", "b", "c", "d", "e"), each = 140)
#'
#' df_initial$site_row <- paste(df_initial$site, df_initial$row, sep = "_")
#' df_initial$site_col <- paste(df_initial$site, df_initial$col, sep = "_")
#' df_initial$site_block <- paste(df_initial$site, df_initial$block, sep = "_")
#'
#' optimize <- list(
#'   connectivity = list(spatial_factors = ~site),
#'   balance = list(swap_within = "site", spatial_factors = ~ site_col + site_block)
#' )
#'
#' options(speed.random_initialisation = TRUE, speed.adj_weight = 0)
#' result <- speed(
#'   data = df_initial,
#'   swap = "lines",
#'   optimise = optimize,
#'   seed = 112,
#'   quiet = TRUE
#' )
#'
#' head(table(result$design_df$lines, result$design_df$site))
#'
#' options(speed.random_initialisation = FALSE, speed.adj_weight = 1)
#'
#' # Plot the MET design with facets
#' autoplot(result, treatments = "lines") +
#' ggplot2::facet_wrap(~site)
#'
#' @export
# fmt: skip
speed <- function(data,
                  # TODO: default for swap
                  swap,
                  swap_within = "1",
                  spatial_factors = ~ row + col,
                  grid_factors = list(dim1 = "row", dim2 = "col"),
                  iterations = 10000,
                  early_stop_iterations = 2000,
                  obj_function = objective_function,
                  swap_all = FALSE,
                  optimise = NULL,
                  optimize_params = optim_params(),
                  quiet = FALSE,
                  seed = NULL,
                  ...) {
  rlang::check_dots_used()

  if (is.null(optimise)) {
    # Check if this is a legacy hierarchical design
    is_legacy <- is.list(swap) && !is.null(names(swap))
    if (is_legacy) {
      .verify_hierarchical_inputs(data, swap, swap_within, spatial_factors, iterations, early_stop_iterations,
                                  obj_function, quiet, seed)
    } else {
      .verify_speed_inputs(data, swap, swap_within, spatial_factors, iterations, early_stop_iterations, quiet,
                           seed)
    }
  }

  # prepare inputs
  optimize <- create_speed_input(swap, swap_within, spatial_factors, grid_factors, iterations,
                                 early_stop_iterations, obj_function, swap_all, optimize_params, optimise)

  # Infer row and column columns
  inferred <- infer_row_col(data, grid_factors, quiet)
  row_column <- inferred$row
  col_column <- inferred$col

  # convert to factors
  factored <- to_factor(data)
  data <- factored$df

  if (inferred$inferred) {
    # Sort the data frame to start with to ensure consistency in calculating the adjacency later
    data <- data[do.call(order, data[c(row_column, col_column)]), ]
  }

  # dummy group for swapping within whole design
  dummy_group <- paste0("dummy_", as.integer(Sys.time()))
  data[[dummy_group]] <- factor(rep(1, nrow(data)))

  # Handle swap_within for each level
  for (level in names(optimize)) {
    opt <- optimize[[level]]
    if (opt$swap_within == "1" || opt$swap_within == "none") {
      optimize[[level]]$swap_within <- dummy_group
    }
  }

  design <- speed_hierarchical(data, optimize, quiet, seed, inferred$inferred, row_column = row_column,
                               col_column = col_column, ...)
  design$design_df[[dummy_group]] <- NULL
  design$design_df <- to_types(design$design_df, factored$input_types)

  # to print deprecate warning at the end
  optim_params()
  return(design)
}

#' Speed function for hierarchical designs
#' @keywords internal
# fmt: skip
speed_hierarchical <- function(data, optimize, quiet, seed, row_col_inferred, ...) {
  # Set seed for reproducibility
  if (is.null(seed)) {
    seed <- .GlobalEnv$.Random.seed[3]
  }

  hierarchy_levels <- names(optimize)
  layout_df <- random_initialize(data, optimize, seed, ...)

  # Initialise design
  current_design <- layout_df
  best_design <- current_design

  # Sequential optimisation for each hierarchy level
  all_scores <- list()
  all_temperatures <- list()
  total_iterations <- 0  # TODO: Track total iterations across all levels

  # Set seed for reproducibility
  set.seed(seed)
  for (level in hierarchy_levels) {
    if (!quiet) cat("Optimising level:", level, "\n")
    opt <- optimize[[level]]
    optimize_params <- do.call(optim_params, opt$optimize_params)
    start_temp <- optimize_params$start_temp
    swap_count <- optimize_params$swap_count
    swap_all_blocks <- optimize_params$swap_all_blocks
    adj_weight <- ifelse(row_col_inferred, optimize_params$adj_weight, 0)
    bal_weight <- optimize_params$bal_weight
    spatial_cols <- all.vars(opt$spatial_factors)

    # Calculate initial score for this level
    current_score_obj <- opt$obj_function(current_design, opt$swap, spatial_cols, adj_weight = adj_weight,
                                          bal_weight = bal_weight, ...)
    current_score <- current_score_obj$score

    if (!is.numeric(current_score)) {
      stop("`score` from `objective_function` must be numeric.")
    }

    best_score_obj <- current_score_obj
    best_score <- current_score
    temp <- start_temp
    scores <- numeric(opt$iterations)
    temperatures <- numeric(opt$iterations)
    last_improvement_iter <- 0

    # Optimisation loop for this level
    for (iter in 1:opt$iterations) {
      scores[iter] <- current_score
      temperatures[iter] <- temp

      if (optimize_params$adaptive_swaps) {
        current_swap_count <- max(1, round(swap_count * temp / start_temp))
        current_swap_all_blocks <- runif(1) < (temp / start_temp) && swap_all_blocks
      } else {
        current_swap_count <- swap_count
        current_swap_all_blocks <- swap_all_blocks
      }

      # Generate new design by swapping treatments at this level
      new_design <- generate_neighbour(current_design,opt$swap, opt$swap_within, current_swap_count,
                                       current_swap_all_blocks,opt$swap_all)

      # Calculate new score
      new_score_obj <- opt$obj_function(new_design$design,opt$swap, spatial_cols, adj_weight = adj_weight,
                                        bal_weight = bal_weight, current_score_obj = current_score_obj,
                                        swapped_items = new_design$swapped_items,...)
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
      temp <- temp * optimize_params$cooling_rate

      # Progress reporting
      if (!quiet && iter %% 1000 == 0) {
        cat("Level:", level,
            "Iteration:", iter,
            "Score:", current_score,
            "Best:", best_score,
            "Since Improvement:", iter - last_improvement_iter,
            "\n")
      }

      # Early stopping
      if (iter - last_improvement_iter >= opt$early_stop_iterations || new_score < .Machine$double.eps) {
        if (!quiet) cat("Early stopping at iteration", iter, "for level", level, "\n")
        # Record final score and temperature before breaking
        if (iter < opt$iterations) {
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
    opt <- optimize[[level]]
    optimize_params <- do.call(optim_params, opt$optimize_params)
    adj_weight <- ifelse(row_col_inferred, optimize_params$adj_weight, 0)
    bal_weight <- optimize_params$bal_weight
    spatial_cols <- all.vars(opt$spatial_factors)

    # treatments and score for each level
    treatments[[level]] <- stringi::stri_sort(unique(as.vector(best_design[[opt$swap]])), numeric = TRUE)
    level_scores[level] <- opt$obj_function(best_design,opt$swap, spatial_cols, adj_weight = adj_weight,
                                            bal_weight = bal_weight,...)$score
  }

  # Check which levels stopped early
  stopped_early <- sapply(hierarchy_levels, function(level) {
    length(all_scores[[level]]) < optimize[[level]]$iterations
  })
  names(stopped_early) <- hierarchy_levels

  # Finalise output
  if (length(hierarchy_levels) == 1) {
    output <- list(
      design_df = best_design,
      score = level_scores[[1]],
      scores = all_scores[[1]],
      temperatures = all_temperatures[[1]],
      iterations_run = total_iterations[[1]],
      stopped_early = stopped_early[[1]],
      treatments = treatments[[1]],
      seed = seed
    )
  } else {
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
  }

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
