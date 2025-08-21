df_fac <- data.frame(
  row = rep(1:12, times = 6),  # 12 rows total
  col = rep(1:6, each = 12),  # 6 columns
  block = rep(1:6, each = 12),  # 6 columns
  A = rep(rep(paste0("A", 1:4), each = 3), times = 6),  # A1, A2, A3, A4
  B = rep(paste0("B", 1:3), times = 24)  # B1, B2, B3
)
df_fac$combination <- paste(df_fac$A, df_fac$B, sep = "_")

class(df_fac) <- c("design", class(df_fac))

autoplot(df_fac, treatment = A)
autoplot(df_fac, treatment = B)
autoplot(df_fac, treatment = combination)

# Set up for multiple seed testing
set.seed(123)  # For reproducible random seed generation
n_seeds <- 50  # Number of different seeds to test
test_seeds <- sample(1:10000, n_seeds)

# Initialize data frames to store results
results_custom <- data.frame(
  seed = integer(),
  adj_combination = numeric(),
  adj_A = numeric(),
  adj_B = numeric(),
  balance_comb = numeric(),
  balance_A = numeric(),
  balance_B = numeric(),
  stringsAsFactors = FALSE
)

results_default <- data.frame(
  seed = integer(),
  adj_combination = numeric(),
  adj_A = numeric(),
  adj_B = numeric(),
  balance_comb = numeric(),
  balance_A = numeric(),
  balance_B = numeric(),
  stringsAsFactors = FALSE
)

# Loop through different seeds
for (i in 1:n_seeds) {
  current_seed <- test_seeds[i]
  cat("Testing seed: ", current_seed, " (", i, "/", n_seeds, ")\n", sep = "")

  # Test with custom objective function
  options(speed.cooling_rate = 0.999)
  fac_result_AB <- speed(df_fac,
                         swap = "combination",
                         # swap_within = "block",
                         obj_function = objective_function_factorial,
                         factors = list(A = "A", B = "B"),
                         iterations = 50000,
                         early_stop_iterations = 10000,
                         interaction_weight = 0.1,
                         quiet = TRUE, seed = current_seed)

  fac_result_AB$design_df$A <- substr(fac_result_AB$design_df$combination, 1, 2)
  fac_result_AB$design_df$B <- substr(fac_result_AB$design_df$combination, 4, 5)

  # Calculate scores for custom objective
  adj_comb_custom <- calculate_adjacency_score(fac_result_AB$design_df, "combination")
  adj_A_custom <- calculate_adjacency_score(fac_result_AB$design_df, "A")
  adj_B_custom <- calculate_adjacency_score(fac_result_AB$design_df, "B")
  balance_comb_custom <- calculate_balance_score(fac_result_AB$design_df, "combination", c("row", "col"))
  balance_A_custom <- calculate_balance_score(fac_result_AB$design_df, "A", c("row", "col"))
  balance_B_custom <- calculate_balance_score(fac_result_AB$design_df, "B", c("row", "col"))

  # Store custom results
  results_custom <- rbind(results_custom, data.frame(
    seed = current_seed,
    adj_combination = adj_comb_custom,
    adj_A = adj_A_custom,
    adj_B = adj_B_custom,
    balance_comb = balance_comb_custom,
    balance_A = balance_A_custom,
    balance_B = balance_B_custom
  ))

  # Test with default objective function
  options(speed.cooling_rate = 0.99)
  fac_result <- speed(df_fac,
                      swap = "combination",
                      # swap_within = "block",
                      quiet = TRUE, seed = current_seed)

  fac_result$design_df$A <- substr(fac_result$design_df$combination, 1, 2)
  fac_result$design_df$B <- substr(fac_result$design_df$combination, 4, 5)

  # Calculate scores for default objective
  adj_comb_default <- calculate_adjacency_score(fac_result$design_df, "combination")
  adj_A_default <- calculate_adjacency_score(fac_result$design_df, "A")
  adj_B_default <- calculate_adjacency_score(fac_result$design_df, "B")
  balance_comb_default <- calculate_balance_score(fac_result$design_df, "combination", c("row", "col"))
  balance_A_default <- calculate_balance_score(fac_result$design_df, "A", c("row", "col"))
  balance_B_default <- calculate_balance_score(fac_result$design_df, "B", c("row", "col"))

  # Store default results
  results_default <- rbind(results_default, data.frame(
    seed = current_seed,
    adj_combination = adj_comb_default,
    adj_A = adj_A_default,
    adj_B = adj_B_default,
    balance_comb = balance_comb_default,
    balance_A = balance_A_default,
    balance_B = balance_B_default
  ))
}

# Summary statistics
cat("\n=== CUSTOM OBJECTIVE FUNCTION RESULTS ===\n")
cat("Adjacency Scores:\n")
cat("  Combination: mean =", round(mean(results_custom$adj_combination), 3),
    ", sd =", round(sd(results_custom$adj_combination), 3), "\n")
cat("  Factor A: mean =", round(mean(results_custom$adj_A), 3),
    ", sd =", round(sd(results_custom$adj_A), 3), "\n")
cat("  Factor B: mean =", round(mean(results_custom$adj_B), 3),
    ", sd =", round(sd(results_custom$adj_B), 3), "\n")
cat("Balance Scores:\n")
cat("  Combination: mean =", round(mean(results_custom$balance_comb), 3),
    ", sd =", round(sd(results_custom$balance_comb), 3), "\n")
cat("  Factor A: mean =", round(mean(results_custom$balance_A), 3),
    ", sd =", round(sd(results_custom$balance_A), 3), "\n")
cat("  Factor B: mean =", round(mean(results_custom$balance_B), 3),
    ", sd =", round(sd(results_custom$balance_B), 3), "\n")

cat("\n=== DEFAULT OBJECTIVE FUNCTION RESULTS ===\n")
cat("Adjacency Scores:\n")
cat("  Combination: mean =", round(mean(results_default$adj_combination), 3),
    ", sd =", round(sd(results_default$adj_combination), 3), "\n")
cat("  Factor A: mean =", round(mean(results_default$adj_A), 3),
    ", sd =", round(sd(results_default$adj_A), 3), "\n")
cat("  Factor B: mean =", round(mean(results_default$adj_B), 3),
    ", sd =", round(sd(results_default$adj_B), 3), "\n")
cat("Balance Scores:\n")
cat("  Combination: mean =", round(mean(results_default$balance_comb), 3),
    ", sd =", round(sd(results_default$balance_comb), 3), "\n")
cat("  Factor A: mean =", round(mean(results_default$balance_A), 3),
    ", sd =", round(sd(results_default$balance_A), 3), "\n")
cat("  Factor B: mean =", round(mean(results_default$balance_B), 3),
    ", sd =", round(sd(results_default$balance_B), 3), "\n")

# Create comparison plots if ggplot2 is available
if (require(ggplot2, quietly = TRUE)) {
  # Prepare data for plotting
  results_custom$method <- "Custom Objective"
  results_default$method <- "Default Objective"
  combined_results <- rbind(results_custom, results_default)

  # Plot adjacency scores
  library(reshape2)
  adj_data <- melt(combined_results[, c("method", "adj_combination", "adj_A", "adj_B")],
                   id.vars = "method", variable.name = "measure", value.name = "score")

  p1 <- ggplot(adj_data, aes(x = method, y = score, fill = method)) +
    geom_boxplot() +
    facet_wrap(~measure, scales = "free_y",
               labeller = labeller(measure = c("adj_combination" = "Combination",
                                             "adj_A" = "Factor A",
                                             "adj_B" = "Factor B"))) +
    labs(title = "Adjacency Scores Comparison", y = "Adjacency Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p1)

  # Plot balance scores
  balance_data <- melt(combined_results[, c("method", "balance_comb", "balance_A", "balance_B")],
                       id.vars = "method", variable.name = "measure", value.name = "score")

  p2 <- ggplot(balance_data, aes(x = method, y = score, fill = method)) +
    geom_boxplot() +
    facet_wrap(~measure, scales = "free_y",
               labeller = labeller(measure = c("balance_comb" = "Combination",
                                               "balance_A" = "Factor A",
                                             "balance_B" = "Factor B"))) +
    labs(title = "Balance Scores Comparison", y = "Balance Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p2)
}
