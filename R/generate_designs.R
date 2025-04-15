# Spatial Optimal Design Implementation with Optional Block Constraints
library(ggplot2)

create_initial_design <- function(nrows, ncols, treatments, blocks = NULL) {
    if (is.null(blocks)) {
        # No blocks: create a simple random design - already vectorized
        design <- matrix(sample(treatments, nrows * ncols, replace = TRUE),
                         nrow = nrows, ncol = ncols)
        return(design)
    } else {
        # With blocks: vectorized version
        design <- matrix(NA, nrow = nrows, ncol = ncols)
        block_ids <- unique(as.vector(blocks))

        # Pre-allocate all treatments for all blocks at once
        block_sizes <- sapply(block_ids, function(b) sum(blocks == b))
        all_block_treatments <- lapply(block_sizes, function(size) {
            if (size > length(treatments)) {
                sample(treatments, size, replace = TRUE)
            } else {
                sample(treatments, size, replace = FALSE)
            }
        })

        # Assign all treatments at once using matrix indexing
        for (b in seq_along(block_ids)) {
            block_mask <- blocks == block_ids[b]
            design[block_mask] <- all_block_treatments[[b]]
        }

        return(design)
    }
}

# Calculate the number of adjacent same treatments
calculate_adjacency_score <- function(design) {
  nrows <- nrow(design)
  ncols <- ncol(design)

  # Check row adjacencies using matrix operations
  row_adjacencies <- rowSums(design[, 1:(ncols-1)] == design[, 2:ncols], na.rm = TRUE)

  # Check column adjacencies using matrix operations
  col_adjacencies <- colSums(design[1:(nrows-1), ] == design[2:nrows, ], na.rm = TRUE)

  # Total score is sum of all adjacencies
  score <- sum(row_adjacencies) + sum(col_adjacencies)

  return(score)
}

# Count treatment frequencies in rows and columns - vectorized version
calculate_balance_score <- function(design, treatments) {
  nrows <- nrow(design)
  ncols <- ncol(design)

  # Initialize row and column count matrices
  row_counts <- matrix(0, nrow = nrows, ncol = length(treatments))
  col_counts <- matrix(0, nrow = ncols, ncol = length(treatments))

  # Count treatments by row and column in a vectorized way
  for (t_idx in 1:length(treatments)) {
    t <- treatments[t_idx]
    row_counts[, t_idx] <- rowSums(design == t, na.rm = TRUE)
    col_counts[, t_idx] <- colSums(design == t, na.rm = TRUE)
  }

  # Calculate variance of counts (lower is better)
  row_var <- sum(apply(row_counts, 2, var))
  col_var <- sum(apply(col_counts, 2, var))

  return(row_var + col_var)
}

# Check if design satisfies block constraints
check_block_constraints <- function(design, blocks, treatments) {
  if (is.null(blocks)) {
    return(TRUE)  # No blocks means no constraints to check
  }

  # Flatten the design and blocks matrices
  design_flat <- as.vector(design)
  blocks_flat <- as.vector(blocks)

  # Group treatments by block
  block_groups <- split(design_flat, blocks_flat)

  # Check for duplicates in each block using a vectorized approach
  has_duplicates <- any(vapply(block_groups, function(block_treatments) {
    length(block_treatments) != length(unique(block_treatments))
  }, logical(1)))

  return(!has_duplicates)
}

# Calculate overall objective function value
calculate_objective <- function(design, treatments, blocks = NULL,
                               adj_weight = 1, bal_weight = 1) {
  adjacency_score <- calculate_adjacency_score(design)
  balance_score <- calculate_balance_score(design, treatments)

  # Check if design satisfies block constraints (if blocks are provided)
  if (!is.null(blocks)) {
    valid_blocks <- check_block_constraints(design, blocks, treatments)

    # If design violates block constraints, return a very high score
    if (!valid_blocks) {
      return(1e9)  # Effectively infinity for optimization purposes
    }
  }

  return(adj_weight * adjacency_score + bal_weight * balance_score)
}

# Generate neighbor with flexible block handling
generate_neighbor <- function(design, blocks = NULL) {
  nrows <- nrow(design)
  ncols <- ncol(design)

  if (is.null(blocks)) {
    # No blocks: swap any two random positions
    pos1 <- c(sample(1:nrows, 1), sample(1:ncols, 1))
    pos2 <- c(sample(1:nrows, 1), sample(1:ncols, 1))

    # Ensure positions are different
    while (pos1[1] == pos2[1] && pos1[2] == pos2[2]) {
      pos2 <- c(sample(1:nrows, 1), sample(1:ncols, 1))
    }
  } else {
    # With blocks: swap within the same block
    block_ids <- unique(as.vector(blocks))
    selected_block <- sample(block_ids, 1)

    # Get positions for this block
    block_positions <- which(blocks == selected_block, arr.ind = TRUE)

    # Choose two random positions within the block
    swap_indices <- sample(1:nrow(block_positions), 2)
    pos1 <- block_positions[swap_indices[1], ]
    pos2 <- block_positions[swap_indices[2], ]
  }

  # Create new design by swapping
  new_design <- design
  temp_val <- new_design[pos1[1], pos1[2]]
  new_design[pos1[1], pos1[2]] <- new_design[pos2[1], pos2[2]]
  new_design[pos2[1], pos2[2]] <- temp_val

  return(new_design)
}

# Simulated annealing algorithm with early stopping
optimize_design <- function(nrows, ncols, treatments, blocks = NULL,
                           iterations = 10000, start_temp = 100, cooling_rate = 0.99,
                           adj_weight = 1, bal_weight = 1,
                           early_stop_iterations = 2000, quiet = FALSE) {

  # Create initial design
  current_design <- create_initial_design(nrows, ncols, treatments, blocks)
  best_design <- current_design

  # Calculate initial scores
  current_score <- calculate_objective(current_design, treatments, blocks,
                                      adj_weight, bal_weight)
  best_score <- current_score

  # Set initial temperature
  temp <- start_temp

  # Create vectors to track progress
  scores <- numeric(iterations)
  temperatures <- numeric(iterations)

  # For early stopping
  last_improvement_iter <- 0

  # Run simulated annealing
  for (iter in 1:iterations) {
    # Store current values
    scores[iter] <- current_score
    temperatures[iter] <- temp

    # Create a new design by swapping positions
    new_design <- generate_neighbor(current_design, blocks)

    # Calculate new score
    new_score <- calculate_objective(new_design, treatments, blocks,
                                    adj_weight, bal_weight)

    # Decide whether to accept the new design
    if (new_score < current_score) {
      # Accept if better
      current_design <- new_design
      current_score <- new_score

      # Update best if better
      if (new_score < best_score) {
        best_design <- new_design
        best_score <- new_score
        last_improvement_iter <- iter  # Record when we last improved
      }
    } else {
      # Accept with probability dependent on temperature
      p <- exp((current_score - new_score) / temp)
      if (runif(1) < p) {
        current_design <- new_design
        current_score <- new_score
      }
    }

    # Cool down temperature
    temp <- temp * cooling_rate

    # Report progress - modified to respect quiet parameter
        if (!quiet && iter %% 1000 == 0) {
            cat("Iteration:", iter, "Score:", current_score, "Best:", best_score,
                "Iterations since improvement:", iter - last_improvement_iter, "\n")
        }

        # Check for early stopping
        if (iter - last_improvement_iter >= early_stop_iterations) {
            if (!quiet) {
                cat("Early stopping at iteration", iter,
                    "- No improvement for", early_stop_iterations, "iterations\n")
            }

            # Trim tracking vectors to actual number of iterations run
            scores <- scores[1:iter]
            temperatures <- temperatures[1:iter]
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

# Evaluate treatment distribution - improved version using vectorization
evaluate_distribution <- function(design, treatments) {
  nrows <- nrow(design)
  ncols <- ncol(design)

  # Create empty result matrices
  row_counts <- matrix(0, nrow = nrows, ncol = length(treatments),
                      dimnames = list(paste0("Row", 1:nrows), treatments))

  col_counts <- matrix(0, nrow = ncols, ncol = length(treatments),
                      dimnames = list(paste0("Col", 1:ncols), treatments))

  # Count treatments by row and column in a vectorized way
  for (t in treatments) {
    row_counts[, t] <- rowSums(design == t, na.rm = TRUE)
    col_counts[, t] <- colSums(design == t, na.rm = TRUE)
  }

  return(list(row_counts = row_counts, col_counts = col_counts))
}

# Function to visualize the design
plot_design <- function(design, blocks = NULL, title = "Experimental Design") {
  design_df <- expand.grid(row = 1:nrow(design), col = 1:ncol(design))
  design_df$treatment <- as.vector(design)

  if (!is.null(blocks)) {
    design_df$block <- as.vector(blocks)
  }

  p <- ggplot(design_df, aes(x = col, y = -row, fill = factor(treatment))) +
    geom_tile(color = "black") +
    geom_text(aes(label = treatment), size = 3) +
    scale_fill_viridis_d() +
    labs(title = title, fill = "Treatment") +
    theme_minimal() +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = 0.5),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12)) +
    coord_equal() +
    scale_x_continuous(breaks = 1:ncol(design)) +
    scale_y_continuous(breaks = -1:-nrow(design),
                      labels = 1:nrow(design))

  # Add block outlines if blocks are provided
  if (!is.null(blocks)) {
    # Find boundaries between blocks
    block_boundaries <- data.frame()

    # Check horizontal boundaries
    for (i in 1:nrow(blocks)) {
      for (j in 1:(ncol(blocks)-1)) {
        if (blocks[i, j] != blocks[i, j+1]) {
          block_boundaries <- rbind(block_boundaries,
                                   data.frame(x = j + 0.5, y = -i,
                                             xend = j + 0.5, yend = -(i-1)))
        }
      }
    }

    # Check vertical boundaries
    for (i in 1:(nrow(blocks)-1)) {
      for (j in 1:ncol(blocks)) {
        if (blocks[i, j] != blocks[i+1, j]) {
          block_boundaries <- rbind(block_boundaries,
                                   data.frame(x = j, y = -(i + 0.5),
                                             xend = j + 1, yend = -(i + 0.5)))
        }
      }
    }

    # Add boundaries to plot
    if (nrow(block_boundaries) > 0) {
      p <- p + geom_segment(data = block_boundaries,
                           aes(x = x, y = y, xend = xend, yend = yend),
                           inherit.aes = FALSE,
                           linewidth = 1.5, color = "red")
    }
  }

  return(p)
}

# Function to plot optimization progress
plot_progress <- function(result) {
  df <- data.frame(
    iteration = 1:length(result$scores),
    score = result$scores,
    temperature = result$temperatures
  )

  p1 <- ggplot(df, aes(x = iteration, y = score)) +
    geom_line() +
    labs(title = "Objective Score Over Iterations",
         x = "Iteration", y = "Score") +
    theme_minimal()

  p2 <- ggplot(df, aes(x = iteration, y = temperature)) +
    geom_line() +
    labs(title = "Temperature Over Iterations",
         x = "Iteration", y = "Temperature") +
    theme_minimal()

  print(p1)
  print(p2)
}

# Function to create block structure for a grid
create_block_structure <- function(nrows, ncols, block_rows, block_cols) {
  # Calculate number of blocks in each dimension
  num_block_rows <- nrows / block_rows
  num_block_cols <- ncols / block_cols

  if (num_block_rows != floor(num_block_rows) || num_block_cols != floor(num_block_cols)) {
    stop("Grid dimensions must be divisible by block dimensions")
  }

  # Initialize blocks matrix
  blocks <- matrix(0, nrow = nrows, ncol = ncols)

  # Assign block IDs
  block_id <- 1
  for (i in 1:num_block_rows) {
    for (j in 1:num_block_cols) {
      row_start <- (i - 1) * block_rows + 1
      row_end <- i * block_rows
      col_start <- (j - 1) * block_cols + 1
      col_end <- j * block_cols

      blocks[row_start:row_end, col_start:col_end] <- block_id
      block_id <- block_id + 1
    }
  }

  return(blocks)
}




# # Example usage for a smaller test case (for quicker demonstration)
# nrows <- 5
# ncols <- 8
# treatments <- paste0("T", 1:5)  # 5 treatments
# blocks <- create_block_structure(nrows, ncols, 5, 1)  # 5 blocks of 5x1
#
# # Run optimization with early stopping
# set.seed(123)  # For reproducibility
# result_sm <- optimize_design(nrows, ncols, treatments, blocks,
#                              iterations = 10000,
#                              start_temp = 100,
#                              cooling_rate = 0.99,
#                              adj_weight = 1,
#                              bal_weight = 1,
#                              early_stop_iterations = 2000)
#
#
# # Test with large case
# nrows <- 40
# ncols <- 20
# treatments <- paste0("T", 1:25)  # 25 treatments
# blocks <- create_block_structure(nrows, ncols, 5, 5)  # 5 blocks of 2x5
#
# # Run optimization with early stopping
# set.seed(123)  # For reproducibility
# result <- optimize_design(nrows, ncols, treatments, blocks,
#                           iterations = 100000,
#                           start_temp = 100,
#                           cooling_rate = 0.99,
#                           adj_weight = 1,
#                           bal_weight = 1,
#                           early_stop_iterations = 2000)
#
# # Visualize results
# cat("Final design has adjacency score:", result$adjacency_score,
#     "and balance score:", result$balance_score, "\n")
# cat("Optimization ran for", result$iterations_run, "iterations\n")
# if (result$stopped_early) {
#     cat("Optimization stopped early due to lack of improvement\n")
# }
#
# # Plot the design with block boundaries
# design_plot <- plot_design(result$design, blocks, "Spatially Optimized Design with Blocks")
# print(design_plot)
#
# # Plot optimization progress
# plot_progress(result)
#
#
# # Use the evaluation function
# distribution_sm <- evaluate_distribution(result_sm$design, treatments)
# print("Treatment counts by row:")
# print(distribution$row_counts)
# table(distribution$row_counts)
# print("Treatment counts by column:")
# print(distribution$col_counts)
# table(distribution$col_counts)
