# Latin Square Design Frequency Analysis
# Generate 50000 4x4 Latin squares and analyze their distribution

library(speed)

# Set up initial design
df <- data.frame(
  row = rep(1:4, times = 4),
  col = rep(1:4, each = 4),
  treatment = rep(LETTERS[1:4], 4)
)

# Enable random initialization
options(speed.random_initialisation = TRUE)

# Storage for unique designs and their counts
design_library <- list()
design_counts <- integer(0)
iteration_counts <- list()  # Store iteration counts for each occurrence

# Number of simulations
n_simulations <- 50000

# Progress reporting
cat("Generating", n_simulations, "Latin square designs...\n")
start_time <- Sys.time()

for (i in 1:n_simulations) {
  # Generate optimized design
  result <- speed(df, swap = "treatment", quiet = TRUE, early_stop_iterations = 10000)

  # Sort by row, then column, and extract treatment vector
  sorted_design <- result$design_df[order(result$design_df$row, result$design_df$col), ]
  design_signature <- paste(sorted_design$treatment, collapse = "")

  # Check if this design already exists
  design_idx <- which(names(design_library) == design_signature)

  if (length(design_idx) == 0) {
    # New design - add to library
    design_library[[design_signature]] <- sorted_design$treatment
    design_counts <- c(design_counts, 1)
    names(design_counts)[length(design_counts)] <- design_signature
    # Initialize iteration counts list for this design
    iteration_counts[[design_signature]] <- c(result$iterations_run)
  } else {
    # Existing design - increment counter and record iterations
    design_counts[design_idx] <- design_counts[design_idx] + 1
    iteration_counts[[design_signature]] <- c(iteration_counts[[design_signature]], result$iterations_run)
  }

  # Progress reporting every 5000 iterations
  if (i %% 5000 == 0) {
    cat("Completed", i, "iterations. Unique designs found:", length(design_library), "\n")
  }
}

end_time <- Sys.time()
elapsed_time <- difftime(end_time, start_time, units = "secs")

# Summary statistics
cat("\n========== RESULTS ==========\n")
cat("Total simulations:", n_simulations, "\n")
cat("Unique designs found:", length(design_library), "\n")
cat("Time elapsed:", round(elapsed_time, 2), "seconds\n\n")

# Sort by frequency (most common first)
sorted_counts <- sort(design_counts, decreasing = TRUE)

cat("Design frequencies:\n")
cat("-------------------\n")
for (i in 1:length(sorted_counts)) {
  freq <- sorted_counts[i]
  pct <- round(100 * freq / n_simulations, 2)
  cat(sprintf("Design %d: %d occurrences (%.2f%%)\n", i, freq, pct))
}

# Statistical tests
cat("\n========== STATISTICAL ANALYSIS ==========\n")
cat("Expected count per design (if equally probable):",
    round(n_simulations / length(design_library), 2), "\n")
cat("Min count:", min(design_counts), "\n")
cat("Max count:", max(design_counts), "\n")
cat("Mean count:", round(mean(design_counts), 2), "\n")
cat("Std dev:", round(sd(design_counts), 2), "\n")

# Iteration statistics
all_iterations <- unlist(iteration_counts)
cat("\nIteration statistics (across all designs):\n")
cat("  Mean iterations:", round(mean(all_iterations), 2), "\n")
cat("  Median iterations:", median(all_iterations), "\n")
cat("  Min iterations:", min(all_iterations), "\n")
cat("  Max iterations:", max(all_iterations), "\n")
cat("  Std dev:", round(sd(all_iterations), 2), "\n")

# Chi-squared test for uniform distribution
expected_freq <- n_simulations / length(design_library)
chi_sq <- sum((design_counts - expected_freq)^2 / expected_freq)
df_chi <- length(design_library) - 1
p_value <- pchisq(chi_sq, df = df_chi, lower.tail = FALSE)

cat("\nChi-squared test for uniformity:\n")
cat("  Chi-squared statistic:", round(chi_sq, 4), "\n")
cat("  Degrees of freedom:", df_chi, "\n")
cat("  P-value:", format(p_value, scientific = TRUE), "\n")
if (p_value < 0.05) {
  cat("  Result: Designs are NOT equally probable (p < 0.05)\n")
} else {
  cat("  Result: No significant deviation from equal probability (p >= 0.05)\n")
}

# Visualize the distribution
cat("\n========== VISUALIZATION ==========\n")
barplot(sorted_counts,
        main = "Frequency of Unique Latin Square Designs",
        xlab = "Design Index (sorted by frequency)",
        ylab = "Count",
        col = "steelblue",
        las = 2)
abline(h = expected_freq, col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Observed", "Expected (uniform)"),
       col = c("steelblue", "red"), lty = c(1, 2), lwd = c(1, 2))

# Optional: Print first few unique designs for inspection
cat("\n========== SAMPLE DESIGNS ==========\n")
cat("Showing first 3 unique designs:\n\n")
for (i in 1:min(3, length(design_library))) {
  cat("Design", i, "(count:", sorted_counts[i], "):\n")
  design_vec <- design_library[[names(sorted_counts)[i]]]
  design_matrix <- matrix(design_vec, nrow = 4, ncol = 4, byrow = TRUE)
  print(design_matrix)
  cat("\n")
}

# Save results to file
cat("\n========== SAVING RESULTS ==========\n")

# Create results object with all relevant data
latin_square_results <- list(
  # Simulation parameters
  n_simulations = n_simulations,
  unique_designs = length(design_library),
  elapsed_time = elapsed_time,

  # Design data
  design_library = design_library,
  design_counts = design_counts,
  sorted_counts = sorted_counts,
  iteration_counts = iteration_counts,

  # Statistical summary
  summary_stats = list(
    expected_freq = expected_freq,
    min_count = min(design_counts),
    max_count = max(design_counts),
    mean_count = mean(design_counts),
    sd_count = sd(design_counts),
    mean_iterations = mean(all_iterations),
    median_iterations = median(all_iterations),
    min_iterations = min(all_iterations),
    max_iterations = max(all_iterations),
    sd_iterations = sd(all_iterations)
  ),

  # Chi-squared test results
  chi_squared_test = list(
    statistic = chi_sq,
    df = df_chi,
    p_value = p_value
  ),

  # Metadata
  timestamp = Sys.time(),
  r_version = R.version.string,
  speed_version = packageVersion("speed")
)

# Save as RData file
output_file <- paste0("latin_square_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData")
save(latin_square_results, file = output_file)
cat("Results saved to:", output_file, "\n")

# Also save as RDS (more compact, single object)
output_file_rds <- paste0("latin_square_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
saveRDS(latin_square_results, file = output_file_rds)
cat("Results also saved to:", output_file_rds, "\n")

cat("\nTo load results on another computer, use:\n")
cat("  load('", output_file, "')  # Loads object as 'latin_square_results'\n", sep = "")
cat("  # OR\n")
cat("  results <- readRDS('", output_file_rds, "')\n", sep = "")
