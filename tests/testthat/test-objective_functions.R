test_that("objective_function_signature throws error", {
  layout_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(letters[1:3], 3)
  )
  
  expect_error(
    objective_function_signature(layout_df, "treatment", c("row", "col")),
    "This is a dummy function for documentation purposes only"
  )
})

test_that("objective_function works with default parameters", {
  layout_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(letters[1:3], 3)
  )
  
  result <- objective_function(layout_df, "treatment", c("row", "col"))
  
  expect_type(result, "list")
  expect_named(result, "score")
  expect_type(result$score, "double")
  expect_length(result$score, 1)
})

test_that("objective_function works with custom weights", {
  layout_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(letters[1:3], 3)
  )
  
  # Test with custom weights
  result1 <- objective_function(layout_df, "treatment", c("row", "col"), 
                               adj_weight = 2, bal_weight = 0.5)
  result2 <- objective_function(layout_df, "treatment", c("row", "col"), 
                               adj_weight = 1, bal_weight = 1)
  
  expect_type(result1, "list")
  expect_type(result2, "list")
  expect_named(result1, "score")
  expect_named(result2, "score")
  
  # Scores should be different with different weights
  expect_false(identical(result1$score, result2$score))
})

test_that("objective_function handles zero weights correctly", {
  layout_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(letters[1:3], 3)
  )
  
  # Test with zero adjacency weight
  result_no_adj <- objective_function(layout_df, "treatment", c("row", "col"), 
                                     adj_weight = 0, bal_weight = 1)
  expect_equal(result_no_adj$score, 
               calculate_balance_score(layout_df, "treatment", c("row", "col")))
  
  # Test with zero balance weight
  result_no_bal <- objective_function(layout_df, "treatment", c("row", "col"), 
                                     adj_weight = 1, bal_weight = 0)
  expect_equal(result_no_bal$score, 
               calculate_adjacency_score(layout_df, "treatment"))
  
  # Test with both weights zero
  result_no_weights <- objective_function(layout_df, "treatment", c("row", "col"), 
                                         adj_weight = 0, bal_weight = 0)
  expect_equal(result_no_weights$score, 0)
})

test_that("objective_function warns for two treatments with adjacency weight", {
  layout_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(c("A", "B"), length.out = 9)
  )
  
  expect_warning(
    result <- objective_function(layout_df, "treatment", c("row", "col"), 
                                adj_weight = 1, bal_weight = 1),
    "Only 2 treatments detected in 'treatment'. Adjacency optimization becomes deterministic \\(checkerboard pattern\\). Setting adjacency weight to 0."
  )
  
  # Should only include balance score when adjacency weight is set to 0
  expected_score <- calculate_balance_score(layout_df, "treatment", c("row", "col"))
  expect_equal(result$score, expected_score)
})

test_that("objective_function does not warn for two treatments with zero adjacency weight", {
  layout_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(c("A", "B"), length.out = 9)
  )
  
  expect_no_warning(
    result <- objective_function(layout_df, "treatment", c("row", "col"), 
                                adj_weight = 0, bal_weight = 1)
  )
  
  expected_score <- calculate_balance_score(layout_df, "treatment", c("row", "col"))
  expect_equal(result$score, expected_score)
})

test_that("objective_function works with single treatment", {
  layout_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep("A", 9)
  )
  
  expect_warning(
    result <- objective_function(layout_df, "treatment", c("row", "col")),
    "Only 2 treatments detected"
  )
  
  # With single treatment, adjacency should be 0, balance should be 0
  expect_equal(result$score, 0)
})

test_that("objective_function uses speed options correctly", {
  layout_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(letters[1:3], 3)
  )
  
  # Set options
  old_adj <- getOption("speed.adj_weight")
  old_bal <- getOption("speed.bal_weight")
  
  options(speed.adj_weight = 3, speed.bal_weight = 0.2)
  
  result_with_options <- objective_function(layout_df, "treatment", c("row", "col"))
  result_explicit <- objective_function(layout_df, "treatment", c("row", "col"),
                                       adj_weight = 3, bal_weight = 0.2)
  
  expect_equal(result_with_options$score, result_explicit$score)
  
  # Restore options
  if (is.null(old_adj)) {
    options(speed.adj_weight = NULL)
  } else {
    options(speed.adj_weight = old_adj)
  }
  if (is.null(old_bal)) {
    options(speed.bal_weight = NULL)
  } else {
    options(speed.bal_weight = old_bal)
  }
})

test_that("objective_function handles different spatial column configurations", {
  layout_df <- data.frame(
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4),
    treatment = rep(letters[1:4], 2)
  )
  
  # Test with single spatial column
  result_row <- objective_function(layout_df, "treatment", "row")
  result_col <- objective_function(layout_df, "treatment", "col")
  result_both <- objective_function(layout_df, "treatment", c("row", "col"))
  
  expect_type(result_row, "list")
  expect_type(result_col, "list")
  expect_type(result_both, "list")
  
  # Scores should be different
  expect_false(identical(result_row$score, result_col$score))
  expect_false(identical(result_row$score, result_both$score))
})

test_that("objective_function score is rounded to 10 decimal places", {
  layout_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(letters[1:3], 3)
  )
  
  result <- objective_function(layout_df, "treatment", c("row", "col"))
  
  # Check that the score is rounded
  expect_equal(result$score, round(result$score, 10))
})

test_that("objective_function handles extra parameters via ...", {
  layout_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(letters[1:3], 3)
  )
  
  # Should not error with extra parameters
  expect_no_error(
    result <- objective_function(layout_df, "treatment", c("row", "col"),
                                extra_param = "test", another_param = 123)
  )
  
  expect_type(result, "list")
  expect_named(result, "score")
})
