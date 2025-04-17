optimize_design_piepho <- function(nrows, ncols, treatments, blocks = NULL,
                                   iterations = 10000, start_temp = 100, cooling_rate = 0.99,
                                   adj_weight = 1, bal_weight = 1,
                                   early_stop_iterations = 2000, quiet = FALSE) {
  # Create initial design
  current_design <- create_initial_design(nrows, ncols, treatments, blocks)
  best_design <- current_design

  # Calculate initial scores
  current_score <- calculate_objective_piepho(
    current_design, treatments, blocks,
    adj_weight, bal_weight
  )
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
    new_score <- calculate_objective_piepho(
      new_design, treatments, blocks,
      adj_weight, bal_weight
    )

    # Decide whether to accept the new design
    if (new_score < current_score) {
      # Accept if better
      current_design <- new_design
      current_score <- new_score

      # Update best if better
      if (new_score < best_score) {
        best_design <- new_design
        best_score <- new_score
        last_improvement_iter <- iter # Record when we last improved
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

calculate_objective_piepho <- function(design, treatments, blocks = NULL,
                                       adj_weight = 1, bal_weight = 1) {
  ed <- 1 / calculate_ed(design)$min_mst
  nb <- calculate_nb(design)$max_nb
  adjacency_score <- calculate_adjacency_score(design)
  balance_score <- calculate_balance_score(design, treatments)

  # Check if design satisfies block constraints (if blocks are provided)
  if (!is.null(blocks)) {
    valid_blocks <- check_block_constraints(design, blocks, treatments)

    # If design violates block constraints, return a very high score
    if (!valid_blocks) {
      return(1e9) # Effectively infinity for optimization purposes
    }
  }

  return(adj_weight * adjacency_score + bal_weight * balance_score + ed + nb)
}
