# Test 1: Check if the function runs without errors
test_that("speed runs without errors", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  result <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 100,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(result, "design")
})

# Test 2: Validate output structure
test_that("speed returns correct output structure", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  result <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 1000,
    seed = 42,
    quiet = TRUE
  )

  expect_named(
    result,
    c(
      "design_df",
      "score",
      "scores",
      "temperatures",
      "iterations_run",
      "stopped_early",
      "treatments",
      "seed"
    )
  )

  # Check data types
  expect_true(is.data.frame(result$design_df))
  expect_true(is.numeric(result$score))
  expect_true(is.numeric(result$scores))
  expect_true(is.numeric(result$temperatures))
  expect_true(is.logical(result$stopped_early))
  expect_true(is.character(result$treatments))

  # Check values
  expect_equal(nrow(result$design_df), 20)
  expect_equal(ncol(result$design_df), 3)
  expect_equal(result$score, 1)
  expect_equal(length(result$scores), 1000)
  expect_equal(length(result$temperatures), 1000)
  expect_equal(result$iterations_run, 1000)
  expect_equal(result$stopped_early, FALSE)
  expect_equal(result$treatments, c("A", "B", "C", "D"))

  vdiffr::expect_doppelganger("speed_small", autoplot(result))
})

# Test 3: Check input validation for missing columns
test_that("speed throws error for missing columns", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  expect_error(
    speed(
      data = test_data,
      swap = "nonexistent_column",
      spatial_factors = ~ row + col,
      iterations = 100,
      quiet = TRUE
    ),
    "'nonexistent_column' not found in row, col, treatment"
  )
})

# Test 4: Check input validation for invalid spatial factors
test_that("speed throws error for invalid spatial factors", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  expect_error(
    speed(
      data = test_data,
      swap = "treatment",
      spatial_factors = ~invalid_column,
      iterations = 100,
      quiet = TRUE
    ),
    "'invalid_column' not found in row, col, treatment"
  )
})

# Test 5: Check early stopping behaviour
test_that("speed stops early when no improvement", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  result <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 100,
    early_stop_iterations = 10,
    seed = 42,
    quiet = TRUE
  )
  expect_true(result$stopped_early)
  expect_lt(result$iterations_run, 100)
})

# Test 6: Check reproducibility with seed
test_that("speed produces reproducible results with seed", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  result1 <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 1000,
    seed = 123,
    quiet = TRUE
  )
  result2 <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 1000,
    seed = 123,
    quiet = TRUE
  )
  expect_equal(result1$design_df, result2$design_df)
  expect_equal(result1$score, result2$score)
  vdiffr::expect_doppelganger("speed_seed", autoplot(result1))
})

# Test 7: Check behaviour with swap_within boundaries
test_that("speed respects swap_within boundaries", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = c(rep(LETTERS[1:5], each = 2), rep(LETTERS[6:10], each = 2)),
    block = rep(1:2, each = 10)
  )

  result <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col,
    iterations = 1000,
    seed = 42,
    quiet = TRUE
  )

  # Ensure treatments are swapped only within blocks
  block1_treatments <- unique(result$design_df$treatment[result$design_df$block == 1])
  block2_treatments <- unique(result$design_df$treatment[result$design_df$block == 2])
  expect_true(all(block1_treatments %in% LETTERS[1:5]))
  expect_true(all(block2_treatments %in% LETTERS[6:10]))
  expect_false(any(block1_treatments %in% block2_treatments))
  expect_false(any(block2_treatments %in% block1_treatments))

  vdiffr::expect_doppelganger("speed_blocks", autoplot(result))
})

# Test 8: Irregular layout with missing plots
test_that("speed handles irregular layouts with missing plots", {
  # skip()
  irregular_data <- data.frame(
    row = rep(1:4, each = 3),
    col = rep(1:3, times = 4),
    treatment = rep(LETTERS[1:3], 4)
  )

  irregular_data$treatment[c(1, 9, 11)] <- NA

  result <- speed(
    data = irregular_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 1000,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(result, "design")

  # Check values
  expect_identical(which(is.na(result$design_df$treatment)), which(is.na(irregular_data$treatment)))

  expect_equal(result$score, 1)

  vdiffr::expect_doppelganger("speed_missing_plots", autoplot(result))
})

# Test 9: Multiple spatial factors
test_that("speed handles multiple spatial factors", {
  multi_factor_data <- data.frame(
    row = rep(1:5, each = 5),
    col = rep(1:5, times = 5),
    block = rep(1:5, each = 5),
    treatment = rep(LETTERS[1:5], 5)
  )
  result <- speed(
    data = multi_factor_data,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col + block,
    iterations = 10000,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(result, "design")

  vdiffr::expect_doppelganger("speed_multi_spatial_factors", autoplot(result))
})

# Test 11: Non-uniform treatment distribution
test_that("speed handles non-uniform treatment distributions", {
  non_uniform_data <- data.frame(
    row = rep(1:8, each = 9),
    col = rep(1:9, times = 8),
    treatment = c(rep(LETTERS[1], 27), rep(LETTERS[2:6], 9))
  )
  result <- speed(
    data = non_uniform_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 2000,
    early_stop_iterations = 400,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(result, "design")

  # Check values
  expect_equal(result$stopped_early, TRUE)

  expect_equal(result$score, 16.8)
  expect_equal(result$iterations_run, 1458)

  vdiffr::expect_doppelganger("speed_non_uniform", autoplot(result))
})

# Test 12: Custom objective function
test_that("speed works with a custom objective function", {
  custom_obj_function <- function(adj_weight = 0.5, bal_weight = 2) {
    function(design, swap, spatial_cols, ...) {
      adj_score <- calculate_adjacency_score(design, swap)
      bal_score <- calculate_balance_score(design, swap, spatial_cols)
      return(list(score = adj_weight * adj_score + bal_weight * bal_score))
    }
  }
  custom_obj_data <- data.frame(
    row = rep(1:4, each = 4),
    col = rep(1:4, times = 4),
    treatment = rep(LETTERS[1:4], 4)
  )
  # skip()  # Test not working
  result <- speed(
    data = custom_obj_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    obj_function = custom_obj_function(),
    iterations = 1000,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(result, "design")

  # Check values
  expect_equal(result$score, 0)
  expect_equal(result$iterations_run, 1000)
  expect_equal(result$stopped_early, FALSE)

  vdiffr::expect_doppelganger("speed_custom_obj_func", autoplot(result))
})

# Test 10: Large grid layout
test_that("speed handles large grid layouts", {
  large_grid_data <- data.frame(
    row = rep(1:20, each = 20),
    col = rep(1:20, times = 20),
    treatment = rep(LETTERS[1:10], 40)
  )
  result <- speed(
    data = large_grid_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 2000,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(result, "design")

  vdiffr::expect_doppelganger("speed_large_grid", autoplot(result))
})

# Test 15: Large layout with blocking
test_that("speed handles large layouts with blocking", {
  large_blocked_data <- data.frame(
    row = rep(1:16, each = 16),
    col = rep(1:16, times = 16),
    block = rep(1:4, each = 64),
    treatment = rep(LETTERS[1:16], 16)
  )
  result <- speed(
    data = large_blocked_data,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col + block,
    iterations = 5000,
    early_stop_iterations = 1000,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(result, "design")

  # Check values
  # expect_equal(result$score, 4.2667, tolerance = 0.0001)
  expect_equal(result$iterations_run, 5000)
  expect_equal(result$stopped_early, FALSE)

  expect_snapshot(result$design_df)

  vdiffr::expect_doppelganger("speed_large_blocks", autoplot(result))
})

# Test 16: Check large irregular layout
test_that("speed handles irregular layouts with 400 unique plots", {
  # skip()
  irregular_large_data <- data.frame(
    row = rep(1:20, each = 20),
    col = rep(1:20, times = 20),
    treatment = rep(LETTERS[1:10], 40)
  )

  # Remove random plots
  set.seed(123)
  irregular_large_data$treatment[sample(1:nrow(irregular_large_data), 50)] <- NA

  # Optimize the design
  result <- speed(
    data = irregular_large_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 4000,
    seed = 42,
    quiet = TRUE
  )

  # Check the result
  expect_s3_class(result, "design")
  # Check values
  # expect_equal(result$score, 12.3333, tolerance = 0.0001)
  expect_equal(result$iterations_run, 4000)
  expect_equal(result$stopped_early, FALSE)

  expect_identical(which(is.na(result$design_df$treatment)), which(is.na(irregular_large_data$treatment)))

  vdiffr::expect_doppelganger("speed_large_missing", autoplot(result))
})

# Test 16: Check large irregular layout
test_that("speed handles irregular layouts with a clump of missing plots", {
  # Clumped missing plots
  irregular_large_data <- data.frame(
    row = rep(1:20, each = 20),
    col = rep(1:20, times = 20),
    treatment = rep(LETTERS[1:10], 40)
  )

  # Keep only unique row-column combinations
  irregular_large_data[
    irregular_large_data$row %in% 13:16 & irregular_large_data$col %in% 7:10,
    "treatment"
  ] <- NA

  # Optimize the design
  result <- speed(
    data = irregular_large_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 4000,
    seed = 42,
    quiet = TRUE
  )

  # Check the result
  expect_s3_class(result, "design")
  # Check values
  # expect_equal(result$score, 13.69, tolerance = 0.0001)
  expect_equal(result$iterations_run, 4000)
  expect_equal(result$stopped_early, FALSE)

  expect_identical(which(is.na(result$design_df$treatment)), which(is.na(irregular_large_data$treatment)))

  vdiffr::expect_doppelganger("speed_large_missing_clump", autoplot(result))
})

# Test 16: Check large irregular layout
test_that("speed handles irregular layouts with L shaped plots", {
  # Large section of missing plots
  irregular_large_data <- data.frame(
    row = rep(1:20, each = 20),
    col = rep(1:20, times = 20),
    treatment = rep(LETTERS[1:10], 40)
  )

  # Keep only unique row-column combinations
  irregular_large_data[
    irregular_large_data$row %in% 1:14 & irregular_large_data$col %in% 13:20,
    "treatment"
  ] <- NA

  # Optimize the design
  result <- speed(
    data = irregular_large_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 4000,
    seed = 42,
    quiet = TRUE
  )

  # Check the result
  expect_s3_class(result, "design")
  # Check values
  # expect_equal(result$score, 13.69, tolerance = 0.0001)
  expect_equal(result$iterations_run, 4000)
  expect_equal(result$stopped_early, FALSE)

  expect_identical(which(is.na(result$design_df$treatment)), which(is.na(irregular_large_data$treatment)))

  vdiffr::expect_doppelganger("speed_large_missing_L", autoplot(result))
})

# TODO: Test cases to add/update
# - Add more detailed checking of current designs
# - NSE
# - Prints progress when quiet = FALSE
# - different designs
# - Adaptive swaps
# - Seed not set
# - Objective function not numeric result
# - swap_all_blocks
# - Print method output
# - Plotting
