optimize_design_tabu <- function(
    nrows,
    ncols,
    treatments,
    blocks = NULL,
    iterations = 10000,
    start_temp = 100,
    cooling_rate = 0.99,
    adj_weight = 1,
    bal_weight = 1,
    early_stop_iterations = 2000,
    ntabu = 20,
    quiet = FALSE) {
  # Create initial design
  current_design <- create_initial_design(nrows, ncols, treatments, blocks)
  best_design <- current_design

  # Calculate initial scores
  current_score <- calculate_objective(current_design, treatments, blocks, adj_weight, bal_weight)
  best_score <- current_score

  # Set initial temperature
  temp <- start_temp

  # Create vectors to track progress
  scores <- numeric(iterations)
  temperatures <- numeric(iterations)

  # For early stopping
  last_improvement_iter <- 0

  tabu_list <- new.env(hash = TRUE)
  tabu_seq <- c()

  # Run simulated annealing
  for (iter in 1:iterations) {
    # Store current values
    scores[iter] <- current_score
    temperatures[iter] <- temp
    is_best <- FALSE

    new_design <- generate_neighbor(current_design, blocks)
    new_score <- calculate_objective(new_design, treatments, blocks, adj_weight, bal_weight)
    design_hash <- rlang::hash(new_design)

    if (new_score < best_score) {
      best_design <- new_design
      best_score <- new_score
      last_improvement_iter <- iter # Record when we last improved
      is_best <- TRUE
    }

    # lower is better
    if (!is.null(tabu_list[[design_hash]]) && new_score < current_score) {
      current_design <- new_design
      current_score <- new_score
      is_best <- TRUE
    } else {
      # Accept with probability dependent on temperature
      p <- exp((current_score - new_score) / temp)
      if (runif(1) < p) {
        current_design <- new_design
        current_score <- new_score
      }
    }

    if (is_best) {
      tabu_list[[design_hash]] <- iter
      tabu_seq <- c(tabu_seq, design_hash)
    }

    if (length(tabu_seq) > ntabu) {
      tabu_pop <- tabu_seq[1]
      tabu_seq <- tabu_seq[-1]
      tabu_list[[tabu_pop]] <- NULL
    }

    # Cool down temperature
    temp <- temp * cooling_rate

    # Report progress - modified to respect quiet parameter
    if (!quiet && iter %% 1000 == 0) {
      cat(
        "Iteration:", iter, "Score:", current_score, "Best:", best_score,
        "Iterations since improvement:", iter - last_improvement_iter, "\n"
      )
    }

    # Check for early stopping
    if (iter - last_improvement_iter >= early_stop_iterations) {
      if (!quiet) {
        cat(
          "Early stopping at iteration", iter,
          "- No improvement for", early_stop_iterations, "iterations\n"
        )
      }

      break
    }
  }

  return(list(
    design = best_design,
    score = best_score,
    adjacency_score = calculate_adjacency_score(best_design),
    balance_score = calculate_balance_score(best_design, treatments),
    scores = scores,
    temperatures = temperatures,
    iterations_run = length(scores),
    stopped_early = length(scores) < iterations
  ))
}

speed_tabu <- function(
    df,
    permute,
    swap = ~1,
    spatialFactors = ~ Row + Col,
    swap_count = 1,
    swap_all_blocks = FALSE,
    adaptive_swaps = FALSE,
    iterations = 10000,
    early_stop_iterations = 2000,
    quiet = FALSE,
    tabu_size = 20,
    candidate_moves = 5,
    diversification_freq = 50,
    adj_weight = 1,
    bal_weight = 1) {
  # Input validation
  {
    if (!is.data.frame(df)) {
      stop("df must be an initial data frame of the design")
    }
    layout_df <- df
    if (!inherits(permute, "formula")) {
      stop("permute must be a one sided formula")
    }
    permute_var <- deparse(permute[[2]])
    if (!(permute_var %in% names(layout_df))) {
      stop("permute column not found in data frame")
    }
    permute_vals <- layout_df[[permute_var]]
    if (!inherits(swap, "formula")) {
      stop("swap must be a one sided formula")
    }
    swap_var <- deparse(swap[[2]])
    if (swap_var == "1") {
      swap_vals <- factor(rep(1, nrow(df)))
    } else if (!(swap_var %in% names(layout_df))) {
      stop("swap column not found in data frame")
    } else {
      swap_vals <- layout_df[[swap_var]]
    }
    if (!inherits(spatialFactors, "formula")) {
      stop("spatialFactors must be a one sided formula")
    }
    spatial_fac <- all.vars(spatialFactors)
    if (!all(spatial_fac %in% names(layout_df))) {
      stop("One or more spatialFactors not found in data frame")
    }
  }

  # Derive dimensions
  row <- as.integer(as.character(layout_df$Row))
  col <- as.integer(as.character(layout_df$Col))
  nrows <- max(row)
  ncols <- max(col)
  permute_mat <- matrix(permute_vals, nrow = nrows, ncol = ncols, byrow = FALSE)
  swap_mat <- matrix(swap_vals, nrow = nrows, ncol = ncols, byrow = FALSE)
  treatments <- unique(as.vector(permute_mat))

  # Initialize design
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

  # Scoring functions
  calculate_adjacency_score <- function(design) {
    row_adjacencies <- rowSums(design[, -ncol(design)] == design[, -1], na.rm = TRUE)
    col_adjacencies <- colSums(design[-nrow(design), ] == design[-1, ], na.rm = TRUE)
    sum(row_adjacencies) + sum(col_adjacencies)
  }

  calculate_balance_score <- function(layout_df, permute_var, spatial_fac) {
    bscore <- sapply(spatial_fac, function(el) {
      sum(apply(table(layout_df[[el]], layout_df[[permute_var]]), 1, var))
    })
    sum(bscore)
  }

  calculate_objective <- function(design, permute_var, layout_df, spatial_fac) {
    layout_df[[permute_var]] <- as.vector(design)
    adj <- calculate_adjacency_score(design)
    bal <- calculate_balance_score(layout_df, permute_var, spatial_fac)
    adj_weight * adj + bal_weight * bal
  }

  # Generate a candidate move
  generate_neighbor <- function(design, swap_mat, swap_count, swap_all_blocks) {
    new_design <- design
    swap_levels <- unique(as.vector(swap_mat))
    if (!swap_all_blocks) swap_levels <- sample(swap_levels, 1)
    for (level in swap_levels) {
      for (i in 1:swap_count) {
        level <- sample(swap_levels, 1)
        positions <- which(swap_mat == level, arr.ind = TRUE)
        if (nrow(positions) < 2) next
        idx <- sample(seq_len(nrow(positions)), 2)
        pos1 <- positions[idx[1], ]
        pos2 <- positions[idx[2], ]
        tmp <- new_design[pos1[1], pos1[2]]
        new_design[pos1[1], pos1[2]] <- new_design[pos2[1], pos2[2]]
        new_design[pos2[1], pos2[2]] <- tmp
      }
    }
    return(new_design)
  }

  # Initialize tabu list - will store iteration when move was made
  tabu_list <- new.env(hash = TRUE)

  # Initialize tracking variables
  current_score <- calculate_objective(current_design, permute_var, layout_df, spatial_fac)
  best_score <- current_score
  scores <- numeric(iterations)
  last_improvement_iter <- 0

  # Main tabu search loop
  for (iter in 1:iterations) {
    scores[iter] <- current_score

    # Adjust parameters if adaptive
    if (adaptive_swaps) {
      freq_factor <- min(1, (iter - last_improvement_iter) / diversification_freq)
      current_swap_count <- max(1, round(swap_count * (1 + freq_factor)))
      current_swap_all_blocks <- freq_factor > 0.7 && swap_all_blocks
    } else {
      current_swap_count <- swap_count
      current_swap_all_blocks <- swap_all_blocks
    }

    # Generate candidate moves
    best_candidate <- NULL
    best_candidate_score <- Inf

    for (c in 1:candidate_moves) {
      # Generate a candidate move
      candidate_design <- generate_neighbor(
        current_design,
        swap_mat,
        current_swap_count,
        current_swap_all_blocks
      )
      design_hash <- rlang::hash(candidate_design)

      # Apply move to get new design
      candidate_score <- calculate_objective(candidate_design, permute_var, layout_df, spatial_fac)

      # Aspiration criterion: accept tabu move if it gives the best solution found so far
      if (
        candidate_score < best_score ||
          (!is_tabu(tabu_list, design_hash, iter, tabu_size) && candidate_score < best_candidate_score)
      ) {
        best_candidate <- list(design = candidate_design, score = candidate_score, hash = design_hash)
        best_candidate_score <- candidate_score
      }
    }

    # If no valid move found (all were tabu), generate a random non-tabu move
    if (is.null(best_candidate)) {
      repeat {
        candidate_design <- generate_neighbor(
          current_design,
          swap_mat,
          current_swap_count,
          current_swap_all_blocks
        )
        design_hash <- rlang::hash(candidate_design)

        if (!is_tabu(tabu_list, design_hash, iter, tabu_size)) {
          candidate_score <- calculate_objective(candidate_design, permute_var, layout_df, spatial_fac)
          best_candidate <- list(design = candidate_design, score = candidate_score, hash = design_hash)
          break
        }
      }
    }

    # Update current solution
    current_design <- best_candidate$design
    current_score <- best_candidate$score

    # Add design tabu list
    tabu_list[[best_candidate$hash]] <- iter

    # Maintain tabu list size (optional - can just let the tenure expire)
    if (iter %% 100 == 0) {
      tabu_keys <- ls(tabu_list)
      for (key in tabu_keys) {
        if (iter - get(key, envir = tabu_list) > tabu_size * 2) {
          rm(list = key, envir = tabu_list)
        }
      }
    }

    # Update best solution if improved
    if (current_score < best_score) {
      best_design <- current_design
      best_score <- current_score
      last_improvement_iter <- iter
    }

    # Implement diversification if no improvement for a while
    if (iter - last_improvement_iter == diversification_freq) {
      if (!quiet) cat("Diversifying search at iteration", iter, "\n")

      # Increase swap count temporarily for greater diversification
      temp_swap_count <- min(5, swap_count * 3)
      current_design <- generate_neighbor(current_design, swap_mat, temp_swap_count, TRUE)
      current_score <- calculate_objective(current_design, permute_var, layout_df, spatial_fac)
    }

    # Reporting
    if (!quiet && iter %% 1000 == 0) {
      cat(
        "Iteration:", iter, "Score:", current_score, "Best:", best_score,
        "Since Improvement:", iter - last_improvement_iter,
        "Tabu Size:", length(ls(tabu_list)), "\n"
      )
    }

    # Early stopping
    if (iter - last_improvement_iter >= early_stop_iterations) {
      if (!quiet) cat("Early stopping at iteration", iter, "\n")
      scores <- scores[1:iter]
      break
    }
  }

  # Prepare output
  design_df <- layout_df
  design_df[[permute_var]] <- as.vector(best_design)

  return(list(
    design = best_design,
    design_df = design_df,
    score = best_score,
    adjacency_score = calculate_adjacency_score(best_design),
    balance_score = calculate_balance_score(design_df, permute_var, spatial_fac),
    scores = scores,
    iterations_run = length(scores),
    stopped_early = length(scores) < iterations,
    treatments = treatments,
    final_tabu_size = length(ls(tabu_list))
  ))
}

is_tabu <- function(tabu_list, hash, iter, tabu_size) {
  # is.null(tabu_list[move_key]) is faster
  return(!is.null(tabu_list[[hash]]) && (iter - tabu_list[[hash]] <= tabu_size))
}



# nrows <- 5
# ncols <- 8
# nblocks <- 8
# treatments <- rep(paste0("T", 1:5), nblocks)
# df_initial <- data.frame(
#   Row = factor(rep(1:nrows, ncols)),
#   Col = factor(rep(1:ncols, each = nrows)),
#   block = factor(rep(1:nblocks, each = nrows * ncols / nblocks)),
#   treatment = factor(sample(treatments, length(treatments)))
# )
#
# speed_design <- speed_tabu(
#   df_initial,
#   permute = ~treatment,
#   swap = ~1,
#   candidate_moves = 9,
#   spatialFactors = ~ block + Row,
#   iterations = 40000,
#   early_stop_iterations = 10000,
#   swap_count = 3,
#   adaptive_swaps = TRUE
# )


# initiate_design_speed <- function(nrows, ncols, treatments) {
#
# }
