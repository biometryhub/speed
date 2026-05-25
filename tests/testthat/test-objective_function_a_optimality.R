balanced_3x3 <- function() {
  # Perfect Latin square: no treatment repeats in any row or column
  data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = c("A", "B", "C", "B", "C", "A", "C", "A", "B")
  )
}

clustered_3x3 <- function() {
  # All of one treatment together: worst spatial arrangement
  data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = c("A", "A", "A", "B", "B", "B", "C", "C", "C")
  )
}

test_that("returns a list with a numeric scalar score", {
  result <- objective_function_a_optimality(balanced_3x3(), "treatment", c("row", "col"))

  expect_type(result, "list")
  expect_true("score" %in% names(result))
  expect_type(result$score, "double")
  expect_length(result$score, 1)
})

test_that("returns blocking_data for caching", {
  result <- objective_function_a_optimality(balanced_3x3(), "treatment", c("row", "col"))

  expect_true("blocking_data" %in% names(result))
  expect_type(result$blocking_data, "list")
  expect_true(all(c("Z", "ZtZ_inv", "valid") %in% names(result$blocking_data)))
})

test_that("balanced design scores lower than or equal to clustered design", {
  bal <- objective_function_a_optimality(balanced_3x3(), "treatment", c("row", "col"))
  bad <- objective_function_a_optimality(clustered_3x3(), "treatment", c("row", "col"))

  expect_lte(bal$score, bad$score)
})

test_that("adj_weight = 0 disables adjacency penalty", {
  df <- clustered_3x3()

  with_adj <- objective_function_a_optimality(df, "treatment", c("row", "col"), adj_weight = 1)
  no_adj   <- objective_function_a_optimality(df, "treatment", c("row", "col"), adj_weight = 0)

  # adj_weight = 0: no adjacency added; score should be strictly lower for the
  # clustered design which has many adjacencies
  expect_lt(no_adj$score, with_adj$score)
})

test_that("cache reuse gives same score as cold start", {
  df1 <- balanced_3x3()
  df2 <- clustered_3x3()  # different treatment layout

  cold <- objective_function_a_optimality(df1, "treatment", c("row", "col"))

  # Re-score df2 but pass blocking_data from df1 call (Z doesn't change between swaps)
  warm <- objective_function_a_optimality(
    df2, "treatment", c("row", "col"),
    current_score_obj = cold
  )
  cold2 <- objective_function_a_optimality(df2, "treatment", c("row", "col"))

  expect_equal(warm$score, cold2$score)
})

test_that("NA rows (buffer plots) are excluded without error", {
  df <- balanced_3x3()
  df$treatment[c(1, 9)] <- NA  # two buffer plots

  expect_no_error(
    result <- objective_function_a_optimality(df, "treatment", c("row", "col"))
  )
  expect_type(result$score, "double")
  expect_length(result$score, 1)
})

test_that("single spatial factor works correctly", {
  df <- balanced_3x3()
  result <- objective_function_a_optimality(df, "treatment", "row")

  expect_type(result$score, "double")
  expect_length(result$score, 1)
})

test_that("no spatial factors (empty spatial_cols) returns a finite score", {
  df <- balanced_3x3()
  # Pass a column with all same value so it is dropped as single-level
  df$dummy <- 1L
  result <- objective_function_a_optimality(df, "treatment", "dummy")

  expect_true(is.finite(result$score))
})

test_that("bal_weight is accepted and ignored without error", {
  result1 <- objective_function_a_optimality(balanced_3x3(), "treatment", c("row", "col"),
                                              bal_weight = 0)
  result2 <- objective_function_a_optimality(balanced_3x3(), "treatment", c("row", "col"),
                                              bal_weight = 5)
  expect_equal(result1$score, result2$score)
})

test_that("compatible with speed() as obj_function", {
  df <- data.frame(
    row = rep(1:4, times = 5),
    col = rep(1:5, each = 4),
    treatment = rep(LETTERS[1:4], 5)
  )

  expect_no_error(
    result <- speed(
      df,
      swap = "treatment",
      obj_function = objective_function_a_optimality,
      optimise_params = optim_params(adj_weight = 0),
      iterations = 200,
      early_stop_iterations = 100,
      quiet = TRUE,
      seed = 42
    )
  )

  expect_s3_class(result, "design")
  expect_true(is.numeric(result$score))
})

# --- block_col tests ---

test_that("block_col changes the score compared to no block_col", {
  df <- balanced_3x3()
  # Use only "row" in spatial_cols; adding "col" via block_col should change the score
  no_block   <- objective_function_a_optimality(df, "treatment", "row", adj_weight = 0)
  with_block <- objective_function_a_optimality(df, "treatment", "row",
                                                 block_col = "col", adj_weight = 0)

  # Blocking by columns as well must change the information matrix
  expect_false(isTRUE(all.equal(no_block$score, with_block$score)))
})

test_that("block_col is cached in blocking_data and reused correctly", {
  df1 <- balanced_3x3()
  df2 <- clustered_3x3()

  # Use "row" in spatial_cols and "col" as block_col so that both contribute to Z
  cold <- objective_function_a_optimality(df1, "treatment", "row",
                                           block_col = "col", adj_weight = 0)
  warm <- objective_function_a_optimality(df2, "treatment", "row",
                                           block_col = "col", adj_weight = 0,
                                           current_score_obj = cold)
  cold2 <- objective_function_a_optimality(df2, "treatment", "row",
                                            block_col = "col", adj_weight = 0)

  expect_equal(warm$score, cold2$score)
})

# --- Vinv tests ---

test_that("Vinv = identity gives same score as Vinv = NULL", {
  df <- balanced_3x3()
  n <- nrow(df)

  no_vinv  <- objective_function_a_optimality(df, "treatment", c("row", "col"),
                                               adj_weight = 0)
  id_vinv  <- objective_function_a_optimality(df, "treatment", c("row", "col"),
                                               Vinv = diag(n), adj_weight = 0)

  expect_equal(no_vinv$score, id_vinv$score, tolerance = 1e-8)
})

test_that("AR1 Vinv runs without error and returns finite score", {
  df <- balanced_3x3()
  Vinv <- ar1_ar1_vinv(df, rho_r = 0.4, rho_c = 0.4)

  expect_no_error(
    result <- objective_function_a_optimality(df, "treatment", c("row", "col"),
                                               Vinv = Vinv, adj_weight = 0)
  )
  expect_true(is.finite(result$score))
})

test_that("Vinv caching: warm start gives same score as cold start", {
  df1 <- balanced_3x3()
  df2 <- clustered_3x3()
  Vinv <- ar1_ar1_vinv(df1, rho_r = 0.3, rho_c = 0.3)

  cold1 <- objective_function_a_optimality(df1, "treatment", c("row", "col"),
                                            Vinv = Vinv, adj_weight = 0)
  warm  <- objective_function_a_optimality(df2, "treatment", c("row", "col"),
                                            Vinv = Vinv, adj_weight = 0,
                                            current_score_obj = cold1)
  cold2 <- objective_function_a_optimality(df2, "treatment", c("row", "col"),
                                            Vinv = Vinv, adj_weight = 0)

  expect_equal(warm$score, cold2$score)
})

# --- error_structure tests ---

test_that("error_structure ar1_ar1 gives same result as pre-computed Vinv", {
  df <- balanced_3x3()
  Vinv <- ar1_ar1_vinv(df, rho_r = 0.3, rho_c = 0.5)

  via_vinv   <- objective_function_a_optimality(df, "treatment", c("row", "col"),
                                                 Vinv = Vinv, adj_weight = 0)
  via_struct <- objective_function_a_optimality(df, "treatment", c("row", "col"),
                                                 error_structure = list(type = "ar1_ar1",
                                                                        rho_r = 0.3,
                                                                        rho_c = 0.5),
                                                 adj_weight = 0)

  expect_equal(via_vinv$score, via_struct$score, tolerance = 1e-10)
})

test_that("error_structure takes precedence over Vinv when both supplied", {
  df <- balanced_3x3()
  Vinv_ar1 <- ar1_ar1_vinv(df, rho_r = 0.3, rho_c = 0.3)
  dummy_Vinv <- diag(nrow(df))  # different from ar1 Vinv

  # error_structure should win
  via_struct   <- objective_function_a_optimality(df, "treatment", c("row", "col"),
                                                   error_structure = list(type = "ar1_ar1",
                                                                          rho_r = 0.3, rho_c = 0.3),
                                                   adj_weight = 0)
  via_override <- objective_function_a_optimality(df, "treatment", c("row", "col"),
                                                   Vinv = dummy_Vinv,
                                                   error_structure = list(type = "ar1_ar1",
                                                                          rho_r = 0.3, rho_c = 0.3),
                                                   adj_weight = 0)

  expect_equal(via_struct$score, via_override$score, tolerance = 1e-10)
})

test_that("unknown error_structure type raises an error", {
  df <- balanced_3x3()
  expect_error(
    objective_function_a_optimality(df, "treatment", c("row", "col"),
                                     error_structure = list(type = "spatial_power"),
                                     adj_weight = 0),
    "Unknown"
  )
})
