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
#' @param optimise_params Parameters used to control the behaviour of
#'   simulated annealing algorithm. See [optim_params()] for more details.
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
#' - **metadata** - A list describing how the design was produced: the captured
#'   `call`, the ordered `levels`, the resolved `row_column` / `col_column`
#'   names, and a `per_level` list recording each level's swap variable,
#'   spatial factors, adjacency/balance weights, requested iterations, starting
#'   temperature, cooling rate, objective function and achieved score. Used by
#'   [summary()][summary.design()] to recompute per-level evaluation metrics.
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
#' optimise <- list(
#'   connectivity = list(spatial_factors = ~site),
#'   balance = list(swap_within = "site", spatial_factors = ~ site_col + site_block)
#' )
#'
#' result <- speed(
#'   data = df_initial,
#'   swap = "lines",
#'   optimise = optimise,
#'   optimise_params = optim_params(random_initialisation = TRUE, adj_weight = 0),
#'   seed = 112,
#'   quiet = TRUE
#' )
#'
#' head(table(result$design_df$lines, result$design_df$site))
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
                  optimise_params = optim_params(),
                  quiet = FALSE,
                  seed = NULL,
                  ...) {
  rlang::check_dots_used()
  call <- match.call()

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
    rownames(data) <- seq_len(nrow(data))
  }

  # dummy group for swapping within whole design
  dummy_group <- paste0("dummy_", as.integer(Sys.time()))
  data[[dummy_group]] <- factor(rep(1, nrow(data)))

  # prepare inputs
  optimise <- create_speed_input(swap, swap_within, spatial_factors, grid_factors, iterations,
                                 early_stop_iterations, obj_function, swap_all, optimise_params, optimise,
                                 inferred$inferred)

  # Handle swap_within for each level
  for (level in names(optimise)) {
    opt <- optimise[[level]]
    if (opt$swap_within == "1" || opt$swap_within == "none") {
      optimise[[level]]$swap_within <- dummy_group
    }
  }

  dots <- list(...)
  .reject_optim_params_in_dots(dots)
  dots <- .prep_dots(dots, optimise, data)

  design <- do.call(speed_hierarchical, c(
    list(data = data, optimise = optimise, quiet = quiet, seed = seed,
         row_column = row_column, col_column = col_column),
    dots
  ))
  # Attach the captured call here rather than threading it through `do.call()`:
  # a language object passed via `do.call(quote = FALSE)` would be evaluated,
  # re-invoking speed() recursively.
  design$metadata$call <- call
  design$design_df[[dummy_group]] <- NULL
  design$design_df <- to_types(design$design_df, factored$input_types)

  # to print deprecate warning at the end
  optim_params()
  return(design)
}

#' Speed function for hierarchical designs
#' @keywords internal
# fmt: skip
speed_hierarchical <- function(data, optimise, quiet, seed, call = NULL, ...) {
  # Set seed for reproducibility
  if (is.null(seed)) {
    seed <- .GlobalEnv$.Random.seed[3]
  }

  hierarchy_levels <- names(optimise)
  layout_df <- random_initialise(data, optimise, seed, ...)

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
    opt <- optimise[[level]]
    optimise_params <- do.call(optim_params, opt$optimise_params)
    start_temp <- optimise_params$start_temp
    swap_count <- optimise_params$swap_count
    swap_all_blocks <- optimise_params$swap_all_blocks
    adj_weight <- optimise_params$adj_weight
    bal_weight <- optimise_params$bal_weight
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

      if (optimise_params$adaptive_swaps) {
        current_swap_count <- max(1, round(swap_count * temp / start_temp))
        current_swap_all_blocks <- runif(1) < (temp / start_temp) && swap_all_blocks
      } else {
        current_swap_count <- swap_count
        current_swap_all_blocks <- swap_all_blocks
      }

      # Generate new design by swapping treatments at this level
      new_design <- generate_neighbour(current_design, opt$swap, opt$swap_within, current_swap_count,
                                       current_swap_all_blocks, opt$swap_all)

      # Calculate new score
      new_score_obj <- opt$obj_function(new_design$design,opt$swap, spatial_cols, adj_weight = adj_weight,
                                        bal_weight = bal_weight, current_score_obj = current_score_obj,
                                        swapped_items = new_design$swapped_items, ...)
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
      temp <- temp * optimise_params$cooling_rate

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
  per_level_meta <- list()
  treatments <- list()
  level_scores <- numeric()
  for (level in hierarchy_levels) {
    opt <- optimise[[level]]
    optimise_params <- do.call(optim_params, opt$optimise_params)
    adj_weight <- optimise_params$adj_weight
    bal_weight <- optimise_params$bal_weight
    spatial_cols <- all.vars(opt$spatial_factors)

    # treatments and score for each level
    treatments[[level]] <- stringi::stri_sort(unique(as.vector(best_design[[opt$swap]])), numeric = TRUE)
    level_scores[level] <- opt$obj_function(best_design, opt$swap, spatial_cols, adj_weight = adj_weight,
                                            bal_weight = bal_weight, ...)$score
    per_level_meta[[level]] <- list(
      swap            = opt$swap,
      spatial_factors = opt$spatial_factors,
      spatial_cols    = spatial_cols,
      adj_weight      = adj_weight,
      bal_weight      = bal_weight,
      iterations      = opt$iterations,
      start_temp      = optimise_params$start_temp,
      cooling_rate    = optimise_params$cooling_rate,
      obj_function    = opt$obj_function,
      final_score     = level_scores[[level]]
    )
  }

  .dots <- list(...)
  metadata <- list(
    call       = call,
    levels     = hierarchy_levels,
    row_column = .dots$row_column %||% "row",
    col_column = .dots$col_column %||% "col",
    per_level  = per_level_meta
  )

  # Check which levels stopped early
  stopped_early <- sapply(hierarchy_levels, function(level) {
    length(all_scores[[level]]) < optimise[[level]]$iterations
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
      seed = seed,
      metadata = metadata
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
      seed = seed,
      metadata = metadata
    )
  }

  class(output) <- c("design", class(output))
  return(output)
}


#' Print method for speed design objects
#'
#' Prints a compact "identity card" for the design: its layout, treatments and
#' replication, optimised score, convergence and seed. The full treatment list
#' is always shown. For richer, statistically meaningful evaluation metrics use
#' [summary()][summary.design()].
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
  meta <- x$metadata
  df <- x$design_df
  hierarchical <- is.list(x$treatments)

  # Uniform field layout: a fixed-width label, then the value. Continuation /
  # per-level lines are indented to line up under the value column.
  pad <- 14
  lab <- function(s) formatC(s, width = -pad)
  indent <- strrep(" ", pad)
  # Integer formatting with thousands separators (e.g. 2,525).
  fmt_int <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)

  # Compact convergence: "run / total", with a note only when it stopped early.
  iter_line <- function(run, requested, stopped) {
    base <- sprintf("%s / %s", fmt_int(run), fmt_int(requested %||% run))
    if (isTRUE(stopped)) paste0(base, " (stopped early)") else base
  }

  # Print a multi-level field: first level on the label line, the rest indented.
  level_field <- function(label, value_fn) {
    lv <- names(x$treatments)
    for (i in seq_along(lv)) {
      cat(if (i == 1) lab(label) else indent,
          lv[i], ": ", value_fn(lv[i]), "\n", sep = "")
    }
  }

  cat("Optimised Experimental Design\n")
  cat("-----------------------------\n")

  # Layout: <nrows> rows x <ncols> cols (<n> plots), or just plot count when no
  # row/column factors are recorded.
  rc <- meta$row_column
  cc <- meta$col_column
  n_plots <- nrow(df)
  if (!is.null(rc) && !is.null(cc) && all(c(rc, cc) %in% names(df))) {
    layout <- sprintf(
      "%d rows x %d cols (%s plots)",
      length(unique(df[[rc]])), length(unique(df[[cc]])), fmt_int(n_plots)
    )
  } else {
    layout <- sprintf("%s plots", fmt_int(n_plots))
  }
  cat(lab("Layout:"), layout, "\n", sep = "")

  # Treatments. Simple: count + replication, then the full list. Hierarchical:
  # count + list per level (per-level replication is reported by summary()).
  if (hierarchical) {
    level_field("Treatments:", function(lv) {
      sprintf("%d (%s)", length(x$treatments[[lv]]),
              paste(x$treatments[[lv]], collapse = ", "))
    })
  } else {
    swap <- meta$per_level[[1]]$swap
    n_trt <- length(x$treatments)
    if (!is.null(swap) && swap %in% names(df)) {
      reps <- table(df[[swap]])
      if (length(unique(reps)) == 1) {
        rep_str <- sprintf("%d (%s reps each)", n_trt, fmt_int(reps[[1]]))
      } else {
        # Unequal replication: report the distribution, e.g.
        # "20 (12 x 1 rep, 8 x 3 reps)" rather than a bare flag.
        rep_dist <- table(reps)
        parts <- vapply(names(rep_dist), function(r) {
          sprintf("%d x %s rep%s", rep_dist[[r]], r, if (r == "1") "" else "s")
        }, character(1))
        rep_str <- sprintf("%d (%s)", n_trt, paste(parts, collapse = ", "))
      }
    } else {
      rep_str <- as.character(n_trt)
    }
    cat(lab("Treatments:"), rep_str, "\n", sep = "")
    cat(indent, paste(x$treatments, collapse = ", "), "\n", sep = "")
  }

  cat(lab("Score:"), x$score, "\n", sep = "")

  # Iterations: run / total (+ stopped-early note). For hierarchical designs the
  # per-level run count is the length of that level's score trace.
  if (hierarchical) {
    level_field("Iterations:", function(lv) {
      iter_line(length(x$scores[[lv]]), meta$per_level[[lv]]$iterations,
                x$stopped_early[[lv]])
    })
  } else {
    cat(lab("Iterations:"),
        iter_line(x$iterations_run, meta$per_level[[1]]$iterations, x$stopped_early),
        "\n", sep = "")
  }

  cat(lab("Seed:"), x$seed, "\n", sep = "")
  cat("\nUse summary() for design evaluation metrics.\n")

  return(invisible(x))
}

#' Reject `...` arguments that must travel through [optim_params()].
#'
#' Stops with a message pointing the user at `optimise_params = optim_params(...)`
#' when any of the listed names is found in `dots`.
#'
#' @param dots A named list captured from `...`.
#' @return `NULL`, invisibly. Called for its side effect.
#' @keywords internal
.reject_optim_params_in_dots <- function(dots) {
  forbidden <- intersect(names(dots), c("adj_weight", "bal_weight"))
  if (length(forbidden) == 0) return(invisible(NULL))
  stop(
    "Argument(s) ", paste(sprintf("`%s`", forbidden), collapse = ", "),
    " must be passed via `optim_params()`, not directly to `speed()`. ",
    "For example: `optimise_params = optim_params(",
    paste0(forbidden[1], " = ..."), ")`.",
    call. = FALSE
  )
}

#' Prep `dots$relationship` once with the union of treatments seen at
#' every swap level.
#'
#' @param dots A named list captured from `...`.
#' @param optimise Per-level `optimise` list as built by [create_speed_input()].
#' @param data The (factor-converted) design data frame.
#' @return `dots`, with `relationship` replaced by the prepped form when present.
#' @keywords internal
.prep_dots <- function(dots, optimise, data) {
  if (is.null(dots$relationship)) return(dots)
  swap_cols <- unique(vapply(optimise, function(o) o$swap, character(1)))
  treatments <- unlist(lapply(swap_cols, function(s) as.character(data[[s]])))
  dots$relationship <- prep_relationship(dots$relationship, treatments)
  dots
}
