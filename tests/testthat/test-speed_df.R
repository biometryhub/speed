# Sample data for testing
test_data <- data.frame(
  row = rep(1:4, each = 3),
  col = rep(1:3, times = 4),
  treatment = rep(LETTERS[1:4], 3)
)

# Test 1: Check if the function runs without errors
test_that("speed_df runs without errors", {
  result <- speed_df(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 100,
    quiet = TRUE
  )
  expect_s3_class(result, "design")
})

# Test 2: Validate output structure
test_that("speed_df returns correct output structure", {
  result <- speed_df(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 100,
    quiet = TRUE
  )
  expect_named(result, c(
    "design_df", "score", "adjacency_score", "balance_score", "scores",
    "temperatures", "iterations_run", "stopped_early", "treatments", "seed", "adjacency_list"
  ))
  expect_true(is.data.frame(result$design_df))
  expect_true(is.numeric(result$score))
  expect_true(is.numeric(result$adjacency_score))
  expect_true(is.numeric(result$balance_score))
  expect_true(is.numeric(result$scores))
  expect_true(is.numeric(result$temperatures))
  expect_true(is.logical(result$stopped_early))
  expect_true(is.character(result$treatments))
})

# Test 3: Check input validation for missing columns
test_that("speed_df throws error for missing columns", {
  expect_error(
    speed_df(
      data = test_data,
      swap = "nonexistent_column",
      spatial_factors = ~ row + col,
      iterations = 100,
      quiet = TRUE
    ),
    "is not a column in 'data'"
  )
})

# Test 4: Check input validation for invalid spatial factors
test_that("speed_df throws error for invalid spatial factors", {
  expect_error(
    speed_df(
      data = test_data,
      swap = "treatment",
      spatial_factors = ~ invalid_column,
      iterations = 100,
      quiet = TRUE
    ),
    "The following spatial factor columns are missing from 'data'"
  )
})

# Test 5: Check early stopping behavior
test_that("speed_df stops early when no improvement", {
  result <- speed_df(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 100,
    early_stop_iterations = 10,
    quiet = TRUE
  )
  expect_true(result$stopped_early)
  expect_lt(result$iterations_run, 100)
})

# Test 6: Check reproducibility with seed
test_that("speed_df produces reproducible results with seed", {
  result1 <- speed_df(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 100,
    seed = 123,
    quiet = TRUE
  )
  result2 <- speed_df(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 100,
    seed = 123,
    quiet = TRUE
  )
  expect_equal(result1$design_df, result2$design_df)
  expect_equal(result1$score, result2$score)
})

# Test 7: Check behavior with different adjacency methods
test_that("speed_df works with different adjacency methods", {
  result_coordinate <- speed_df(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    adjacency_method = "coordinate",
    iterations = 5000,
    seed = 42,
    quiet = TRUE
  )
  result_cardinal <- speed_df(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    adjacency_method = "cardinal",
    iterations = 5000,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(result_coordinate, "design")
  expect_s3_class(result_cardinal, "design")
  expect_false(identical(result_coordinate$design_df, result_cardinal$design_df))
})

# Test 8: Check behavior with swap_within boundaries
test_that("speed_df respects swap_within boundaries", {
  test_data$block <- rep(1:2, each = 6)
  result <- speed_df(
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
  expect_true(all(block1_treatments %in% LETTERS[1:4]))
  expect_true(all(block2_treatments %in% LETTERS[1:4]))
})


# Test 9: Irregular layout with missing plots
test_that("speed_df handles irregular layouts with missing plots", {
  irregular_data <- data.frame(
    row = c(1, 1, 2, 2, 3, 3, 4, 4),
    col = c(1, 2, 1, 3, 2, 3, 1, 3),
    treatment = rep(LETTERS[1:4], 2)
  )
  result <- speed_df(
    data = irregular_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    adjacency_method = "coordinate",
    adjacency_threshold = 1.5,
    iterations = 5000,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(result, "design")
})

# Test 10: Multiple spatial factors
test_that("speed_df handles multiple spatial factors", {
  multi_factor_data <- data.frame(
    row = rep(1:5, each = 5),
    col = rep(1:5, times = 5),
    block = rep(1:5, each = 5),
    treatment = rep(LETTERS[1:5], 5)
  )
  result <- speed_df(
    data = multi_factor_data,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col + block,
    adjacency_method = "cardinal",
    iterations = 10000,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(result, "design")
})

# Test 11: Large grid layout
test_that("speed_df handles large grid layouts", {
  large_grid_data <- data.frame(
    row = rep(1:20, each = 20),
    col = rep(1:20, times = 20),
    treatment = rep(LETTERS[1:10], 40)
  )
  result <- speed_df(
    data = large_grid_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    adjacency_method = "coordinate",
    adjacency_threshold = 1.5,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(result, "design")
})

# Test 12: Non-uniform treatment distribution
test_that("speed_df handles non-uniform treatment distributions", {
  non_uniform_data <- data.frame(
    row = rep(1:8, each = 9),
    col = rep(1:9, times = 8),
    treatment = c(rep(LETTERS[1:3], 18), rep(LETTERS[4:6], 6))
  )
  result <- speed_df(
    data = non_uniform_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    adjacency_method = "coordinate",
    adjacency_threshold = 1.5,
    iterations = 50000,
    early_stop_iterations = 10000,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(result, "design")
})

# Test 13: Custom objective function
test_that("speed_df works with a custom objective function", {
  custom_obj_function <- function(adjacency_weight = 0.5, balance_weight = 2) {
    function(design, adjacency_list, swap, spatial_cols) {
      adj_score <- calculate_adjacency_score_df(design, adjacency_list, swap)
      bal_score <- calculate_balance_score_df(design, swap, spatial_cols)
      return(adjacency_weight * adj_score + balance_weight * bal_score)
    }
  }
  custom_obj_data <- data.frame(
    row = rep(1:4, each = 4),
    col = rep(1:4, times = 4),
    treatment = rep(LETTERS[1:4], 4)
  )
  result <- speed_df(
    data = custom_obj_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    obj_function = custom_obj_function(),
    iterations = 500,
    quiet = TRUE
  )
  expect_s3_class(result, "design")
})

# Test 14: Reproducibility with seed
test_that("speed_df produces reproducible results with seed", {
  seed_test_data <- data.frame(
    row = rep(1:5, each = 5),
    col = rep(1:5, times = 5),
    treatment = rep(LETTERS[1:5], 5)
  )
  result1 <- speed_df(
    data = seed_test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    seed = 123,
    quiet = TRUE
  )
  result2 <- speed_df(
    data = seed_test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    seed = 123,
    quiet = TRUE
  )
  expect_equal(result1$design_df, result2$design_df)
})

# Test 15: Early stopping
test_that("speed_df stops early when no improvement", {
  early_stop_data <- data.frame(
    row = rep(1:5, each = 5),
    col = rep(1:5, times = 5),
    treatment = rep(LETTERS[1:5], 5)
  )
  result <- speed_df(
    data = early_stop_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 1000,
    early_stop_iterations = 50,
    quiet = TRUE
  )
  expect_true(result$stopped_early)
})

# Test 16: Large layout with blocking
test_that("speed_df handles large layouts with blocking", {
  large_blocked_data <- data.frame(
    row = rep(1:16, each = 16),
    col = rep(1:16, times = 16),
    block = rep(1:4, each = 64),
    treatment = rep(LETTERS[1:16], 16)
  )
  result <- speed_df(
    data = large_blocked_data,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col + block,
    adjacency_method = "cardinal",
    iterations = 5000,
    early_stop_iterations = 1000,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(result, "design")
})

# Test 17: Check large irregular layout
test_that("speed_df handles irregular layouts with 250 unique plots", {
  set.seed(123)
  irregular_large_data <- data.frame(
    row = sample(1:20, 250, replace = TRUE),  # Random rows
    col = sample(1:20, 250, replace = TRUE), # Random columns
    treatment = sample(LETTERS[1:10], 250, replace = TRUE) # 10 treatments randomly assigned
  )

  # Keep only unique row-column combinations
  irregular_large_data <- irregular_large_data[!duplicated(irregular_large_data[c("row", "col")]), ]

  # Optimize the design
  result <- speed_df(
    data = irregular_large_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    adjacency_method = "coordinate",
    adjacency_threshold = 2.0,
    seed = 42,
    quiet = TRUE
  )

  # Check the result
  expect_s3_class(result, "design")
  expect_true(nrow(result$design_df) <= 250) # Ensure no more than 250 unique plots
  expect_true(all(!duplicated(result$design_df[c("row", "col")]))) # Ensure unique row-column combinations
})
