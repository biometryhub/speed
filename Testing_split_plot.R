
# Hierarchical split-plot design
df_split <- data.frame(
  row = rep(1:8, each = 4),
  col = rep(1:4, times = 8),
  block = rep(1:4, each = 8),
  wholeplot = rep(1:8, each = 4),
  wholeplot_treatment = rep(rep(LETTERS[1:2], each = 4), times = 4),
  subplot_treatment = rep(letters[1:4], 8)
)

options(speed.adj_weight = 1)
result <- speed(df_split,
                swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                swap_within = list(wp = "block", sp = "wholeplot"))

autoplot(result, treatments = "wholeplot_treatment")
autoplot(result, treatments = "subplot_treatment")


new_design <- generate_sequential_neighbour(design, swap, swap_within, level, swap_count, swap_all_blocks)

class(new_design$design) <- c("design", class(new_design$design))
autoplot(new_design$design, treatments = "wholeplot_treatment")
autoplot(new_design$design, treatments = "subplot_treatment")


for(i in 1:50) {
  new_design <- generate_sequential_neighbour(new_design$design, swap, swap_within, level, swap_count, swap_all_blocks)
  new_score_obj <- objective_function(new_design$design,
                                      swap[[level]],
                                      spatial_cols = ~row+col,
                                      current_score_obj = current_score_obj,
                                      swapped_items = new_design$swapped_items)
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
}



# Optimization loop for this level
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
  if (iter - last_improvement_iter >= early_stop_iterations[[level]] || new_score == 0) {
    if (!quiet) cat("Early stopping at iteration", iter, "for level", level, "\n")
    scores <- scores[1:iter]
    temperatures <- temperatures[1:iter]
    break
  }
}


