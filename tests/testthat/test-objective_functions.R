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

  # expect_warning(
  result <- objective_function(layout_df, "treatment", c("row", "col"))#,
  # "Only 2 treatments detected"
  # )

  # With single treatment, everything should be adjacent
  expect_equal(result$score, 12)
})

# test_that("objective_function uses speed options correctly", {
#   layout_df <- data.frame(
#     row = rep(1:3, each = 3),
#     col = rep(1:3, times = 3),
#     treatment = rep(letters[1:3], 3)
#   )
#
#   # Set options
#   old_adj <- getOption("speed.adj_weight")
#   old_bal <- getOption("speed.bal_weight")
#
#   options(speed.adj_weight = 3, speed.bal_weight = 0.2)
#
#   result_with_options <- objective_function(layout_df, "treatment", c("row", "col"))
#   result_explicit <- objective_function(layout_df, "treatment", c("row", "col"),
#                                         adj_weight = 3, bal_weight = 0.2)
#
#   expect_equal(result_with_options$score, result_explicit$score)
#
#   # Restore options
#   if (is.null(old_adj)) {
#     options(speed.adj_weight = NULL)
#   } else {
#     options(speed.adj_weight = old_adj)
#   }
#   if (is.null(old_bal)) {
#     options(speed.bal_weight = NULL)
#   } else {
#     options(speed.bal_weight = old_bal)
#   }
# })

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

# Tests for objective_function_piepho
test_that("objective_function_piepho works with basic design", {
  design_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(letters[1:3], 3)
  )

  pair_mapping <- create_pair_mapping(design_df$treatment)

  result <- objective_function_piepho(design_df, "treatment", c("row", "col"),
                                      pair_mapping = pair_mapping)

  expect_type(result, "list")
  expect_named(result, c("score", "ed", "bal", "adj", "nb"))
  expect_type(result$score, "double")
  expect_length(result$score, 1)
  expect_type(result$ed, "list")
  expect_type(result$bal, "double")
  expect_type(result$adj, "integer")
  expect_type(result$nb, "list")
})

test_that("objective_function_piepho handles different treatment replication patterns", {
  # Design with 2 reps of each treatment
  design_2_reps <- data.frame(
    row = rep(1:3, each = 2),
    col = rep(1:2, times = 3),
    treatment = rep(letters[1:3], each = 2)
  )

  # Design with 3 reps of each treatment
  design_3_reps <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(letters[1:3], 3)
  )

  # Design with 4 reps of each treatment
  design_4_reps <- data.frame(
    row = rep(1:4, each = 3),
    col = rep(1:3, times = 4),
    treatment = rep(letters[1:3], 4)
  )

  pair_mapping_2 <- create_pair_mapping(design_2_reps$treatment)
  pair_mapping_3 <- create_pair_mapping(design_3_reps$treatment)
  pair_mapping_4 <- create_pair_mapping(design_4_reps$treatment)

  result_2_reps <- objective_function_piepho(design_2_reps, "treatment", c("row", "col"),
                                             pair_mapping = pair_mapping_2)
  result_3_reps <- objective_function_piepho(design_3_reps, "treatment", c("row", "col"),
                                             pair_mapping = pair_mapping_3)
  result_4_reps <- objective_function_piepho(design_4_reps, "treatment", c("row", "col"),
                                             pair_mapping = pair_mapping_4)

  # All should return valid results
  expect_type(result_2_reps, "list")
  expect_type(result_3_reps, "list")
  expect_type(result_4_reps, "list")

  # Check ED structure
  expect_named(result_2_reps$ed, c("msts", "total_mst", "inv_total_mst"))
  expect_named(result_3_reps$ed, c("msts", "total_mst", "inv_total_mst"))
  expect_named(result_4_reps$ed, c("msts", "total_mst", "inv_total_mst"))

  expect_type(result_2_reps$ed$msts, "double")
  expect_type(result_3_reps$ed$msts, "double")
  expect_type(result_4_reps$ed$msts, "double")
  expect_named(result_2_reps$ed$msts, letters[1:3])
  expect_named(result_3_reps$ed$msts, letters[1:3])
  expect_named(result_4_reps$ed$msts, letters[1:3])
  expect_true(is.finite(result_2_reps$ed$total_mst))
  expect_true(is.finite(result_3_reps$ed$total_mst))
  expect_true(is.finite(result_4_reps$ed$total_mst))
  expect_gte(result_2_reps$ed$total_mst, 0)
  expect_gte(result_3_reps$ed$total_mst, 0)
  expect_gte(result_4_reps$ed$total_mst, 0)

  # Scores should be different for different replication patterns
  expect_false(identical(result_2_reps$score, result_3_reps$score))
  expect_false(identical(result_3_reps$score, result_4_reps$score))
})

test_that("objective_function_piepho handles incremental calculation with current_score_obj and swapped_items", {
  design_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = c("a", "b", "c", "b", "a", "c", "c", "a", "b")
  )

  pair_mapping <- create_pair_mapping(design_df$treatment)

  # Calculate full score first
  full_result <- objective_function_piepho(design_df, "treatment", c("row", "col"),
                                           pair_mapping = pair_mapping)

  # Now test incremental calculation by swapping two treatments
  swapped_items <- c("a", "b")
  incremental_result <- objective_function_piepho(design_df, "treatment", c("row", "col"),
                                                  current_score_obj = full_result,
                                                  swapped_items = swapped_items,
                                                  pair_mapping = pair_mapping)

  expect_type(incremental_result, "list")
  expect_named(incremental_result, c("score", "ed", "bal", "adj", "nb"))

  # Test that incremental calculation works differently from full calculation
  # The incremental result should have the same overall structure but potentially different values
  expect_type(incremental_result$ed, "list")
  expect_type(incremental_result$score, "double")

  # Verify that when current_score_obj is provided, the function handles it without error
  # The specific implementation details of how swapped_items affects the ed calculation
  # are tested implicitly through the function not erroring and returning valid results
  expect_true(is.finite(incremental_result$score))
  expect_named(incremental_result$ed, c("msts", "total_mst", "inv_total_mst"))
  expect_type(incremental_result$ed$msts, "double")
  expect_true(setequal(names(incremental_result$ed$msts), names(full_result$ed$msts)))
})

test_that("objective_function_piepho works without pair_mapping", {
  design_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(letters[1:3], 3)
  )

  # Should work without pair_mapping (falls back to internal calculation)
  expect_no_error({
    result <- objective_function_piepho(design_df, "treatment", c("row", "col"))
  })

  result <- objective_function_piepho(design_df, "treatment", c("row", "col"))
  expect_type(result, "list")
  expect_named(result, c("score", "ed", "bal", "adj", "nb"))
})

test_that("objective_function_piepho handles different spatial column configurations", {
  design_df <- data.frame(
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4),
    block = rep(1:2, each = 4),
    treatment = rep(letters[1:4], 2)
  )

  pair_mapping <- create_pair_mapping(design_df$treatment)

  # Test with single spatial column
  result_row <- objective_function_piepho(design_df, "treatment", "row",
                                          pair_mapping = pair_mapping)
  result_col <- objective_function_piepho(design_df, "treatment", "col",
                                          pair_mapping = pair_mapping)
  result_block <- objective_function_piepho(design_df, "treatment", "block",
                                            pair_mapping = pair_mapping)
  result_multiple <- objective_function_piepho(design_df, "treatment", c("row", "col", "block"),
                                               pair_mapping = pair_mapping)

  expect_type(result_row, "list")
  expect_type(result_col, "list")
  expect_type(result_block, "list")
  expect_type(result_multiple, "list")

  # Balance scores should be different for different spatial configurations
  expect_false(identical(result_row$bal, result_col$bal))
  expect_false(identical(result_row$bal, result_multiple$bal))
})

test_that("objective_function_piepho uses custom row and column names", {
  design_df <- data.frame(
    Row = rep(1:3, each = 3),
    Column = rep(1:3, times = 3),
    treatment = rep(letters[1:3], 3)
  )

  pair_mapping <- create_pair_mapping(design_df$treatment)

  # Should work with custom row and column names
  expect_no_error({
    result <- objective_function_piepho(design_df, "treatment", c("Row", "Column"),
                                        pair_mapping = pair_mapping,
                                        row_column = "Row",
                                        col_column = "Column")
  })

  result <- objective_function_piepho(design_df, "treatment", c("Row", "Column"),
                                      pair_mapping = pair_mapping,
                                      row_column = "Row",
                                      col_column = "Column")
  expect_type(result, "list")
  expect_named(result, c("score", "ed", "bal", "adj", "nb"))
})

test_that("objective_function_piepho score is properly rounded to 10 decimal places", {
  design_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(letters[1:3], 3)
  )

  pair_mapping <- create_pair_mapping(design_df$treatment)
  result <- objective_function_piepho(design_df, "treatment", c("row", "col"),
                                      pair_mapping = pair_mapping)

  # Check that the score is rounded to 10 decimal places
  expect_equal(result$score, round(result$score, 10))
})

test_that("objective_function_piepho handles designs with missing values", {
  design_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = c("a", "b", NA, "b", "a", "c", "c", NA, "b")
  )

  pair_mapping <- create_pair_mapping(design_df$treatment[!is.na(design_df$treatment)])

  # Should handle designs with NA values
  expect_no_error({
    result <- objective_function_piepho(design_df, "treatment", c("row", "col"),
                                        pair_mapping = pair_mapping)
  })

  result <- objective_function_piepho(design_df, "treatment", c("row", "col"),
                                      pair_mapping = pair_mapping)
  expect_type(result, "list")
  expect_named(result, c("score", "ed", "bal", "adj", "nb"))
})

test_that("objective_function_piepho errors on single treatment design", {
  design_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep("a", 9)
  )

  expect_error(create_pair_mapping(design_df$treatment), "n < m")
})

test_that("objective_function_piepho individual components are reasonable", {
  design_df <- data.frame(
    row = rep(1:4, each = 3),
    col = rep(1:3, times = 4),
    treatment = rep(letters[1:3], 4)
  )

  pair_mapping <- create_pair_mapping(design_df$treatment)
  result <- objective_function_piepho(design_df, "treatment", c("row", "col"),
                                      pair_mapping = pair_mapping)

  # Check individual components are sensible
  expect_true(is.finite(result$score))
  expect_true(is.finite(result$bal))
  expect_true(is.finite(result$adj))
  expect_gte(result$adj, 0)  # Adjacency score should be non-negative
  expect_gte(result$bal, 0)  # Balance score should be non-negative

  # Check nb component structure
  expect_named(result$nb, c("nb", "max_nb", "max_pairs", "var"))
  expect_true(is.finite(result$nb$max_nb))
  expect_true(is.finite(result$nb$var))
  expect_gte(result$nb$var, 0)  # Variance should be non-negative

  # Check ed component structure
  expect_type(result$ed, "list")
  expect_named(result$ed, c("msts", "total_mst", "inv_total_mst"))
  expect_type(result$ed$msts, "double")
  expect_true(all(is.finite(result$ed$msts)))
  expect_true(is.finite(result$ed$total_mst))
  expect_true(is.finite(result$ed$inv_total_mst))
  expect_gte(min(result$ed$msts), 0)
  expect_gte(result$ed$total_mst, 0)
})

test_that("objective_function_piepho handles extra parameters via ...", {
  design_df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(letters[1:3], 3)
  )

  pair_mapping <- create_pair_mapping(design_df$treatment)

  # Should not error with extra parameters
  expect_no_error({
    result <- objective_function_piepho(design_df, "treatment", c("row", "col"),
                                        pair_mapping = pair_mapping,
                                        extra_param = "test", another_param = 123)
  })

  expect_type(result, "list")
  expect_named(result, c("score", "ed", "bal", "adj", "nb"))
})

test_that("objective_function_factorial works", {
  treatment_a <- paste0("A", 1:8)
  treatment_b <- paste0("B", 1:3)
  treatments <- with(expand.grid(treatment_a, treatment_b), paste(Var1, Var2, sep = "-"))
  df <- initialise_design_df(treatments, 24, 3, 8, 3)
  df <- shuffle_items(df, "treatment", "block", 112)

  subtreatments <- stringi::stri_split_fixed(
    as.character(df$treatment),
    "-",
    n = 2,
    simplify = TRUE
  )
  df[c("treatment_a", "treatment_b")] <- subtreatments

  score_treatment <- calculate_balance_score(df, "treatment", c("row", "col"))
  score_treatment_a <- objective_function(df, "treatment_a", c("row", "col"))$score
  score_treatment_b <- objective_function(df, "treatment_b", c("row", "col"))$score
  expected_score <- score_treatment + score_treatment_a + score_treatment_b

  result <- objective_function_factorial(df, "treatment", c("row", "col"))

  expect_type(result, "list")
  expect_named(result, "score")
  expect_type(result$score, "double")
  expect_equal(result$score, expected_score)
})

test_that("objective_function_factorial falls back to objective_function with invalid separator", {
  treatment_a <- paste0("A", 1:8)
  treatment_b <- paste0("B", 1:3)
  treatments <- with(expand.grid(treatment_a, treatment_b), paste(Var1, Var2, sep = "-"))
  df <- initialise_design_df(treatments, 24, 3, 8, 3)
  df <- shuffle_items(df, "treatment", "block", 112)

  expected_score <- objective_function(df, "treatment", c("row", "col"))$score

  result <- objective_function_factorial(df, "treatment", c("row", "col"), factorial_separator = "")
  expect_equal(result$score, expected_score)

  result <- objective_function_factorial(df, "treatment", c("row", "col"), factorial_separator = NULL)
  expect_equal(result$score, expected_score)
})
