# Factorial designs scored with `objective_function_factorial()`.

test_that("speed handles factorial designs", {
  treatment_a <- paste0("A", 1:5)
  treatment_b <- paste0("B", 1:3)
  treatments <- with(
    expand.grid(treatment_a, treatment_b),
    paste(Var1, Var2, sep = "-")
  )
  df <- initialise_design_df(treatments, 15, 3, 5, 3)

  result <- speed(
    data = df,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col,
    obj_function = objective_function_factorial,
    optimise_params = optim_params(adaptive_swaps = TRUE),
    early_stop_iterations = 100,
    iterations = 500,
    seed = 112,
    quiet = TRUE
  )
  df_result <- result$design_df

  expect_equal(nrow(result$design_df), 45)
  expect_setequal(result$treatments, df$treatment)
  expect_lt(
    result$score,
    objective_function_factorial(df, "treatment", c("row", "col"))$score
  )
})

test_that("speed handles factorial designs with alternative separator", {
  treatment_a <- paste0("A", 1:5)
  treatment_b <- paste0("B", 1:3)
  treatments <- with(
    expand.grid(treatment_a, treatment_b),
    paste(Var1, Var2, sep = ":")
  )
  df <- initialise_design_df(treatments, 15, 3, 5, 3)

  result <- speed(
    data = df,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col,
    obj_function = objective_function_factorial,
    factorial_separator = ":",
    optimise_params = optim_params(adaptive_swaps = TRUE),
    early_stop_iterations = 100,
    iterations = 500,
    seed = 42,
    quiet = TRUE
  )
  df_result <- result$design_df

  expect_equal(nrow(result$design_df), 45)
  expect_setequal(result$treatments, df$treatment)
  # the baseline must use the same separator, or it scores the design as a
  # single non-factorial treatment factor and is not comparable
  expect_lt(
    result$score,
    objective_function_factorial(
      df,
      "treatment",
      c("row", "col"),
      factorial_separator = ":"
    )$score
  )
})

test_that("speed handles 3-way factorial designs", {
  treatment_a <- paste0("A", 1:5)
  treatment_b <- paste0("B", 1:3)
  treatment_c <- paste0("C", 1:3)
  treatments <- with(
    expand.grid(treatment_a, treatment_b, treatment_c),
    paste(Var1, Var2, Var3, sep = "-")
  )
  df <- initialise_design_df(treatments, 15, 9, 5, 9)

  result <- speed(
    data = df,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col,
    obj_function = objective_function_factorial,
    optimise_params = optim_params(adaptive_swaps = TRUE),
    early_stop_iterations = 100,
    iterations = 500,
    seed = 112,
    quiet = TRUE
  )
  df_result <- result$design_df

  expect_equal(nrow(result$design_df), 135)
  expect_setequal(result$treatments, df$treatment)
  expect_lt(
    result$score,
    objective_function_factorial(df, "treatment", c("row", "col"))$score
  )
})

test_that("3-way factorial designs run to optimisation", {
  skip_on_ci()
  skip_on_cran()
  treatment_a <- paste0("A", 1:5)
  treatment_b <- paste0("B", 1:3)
  treatment_c <- paste0("C", 1:3)
  treatments <- with(
    expand.grid(treatment_a, treatment_b, treatment_c),
    paste(Var1, Var2, Var3, sep = "-")
  )
  df <- initialise_design_df(treatments, 15, 9, 5, 9)

  result <- speed(
    data = df,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col,
    obj_function = objective_function_factorial,
    optimise_params = optim_params(adaptive_swaps = TRUE),
    early_stop_iterations = 2000,
    iterations = 50000,
    seed = 112,
    quiet = TRUE
  )
  df_result <- result$design_df

  expect_equal(nrow(result$design_df), 135)
  expect_setequal(result$treatments, df$treatment)

  # The factorial objective has no derivable lower bound, so `optimal_score` is
  # NA; convergence is instead shown by stopping on the no-improvement window
  # rather than the iteration cap.
  expect_true(is.na(result$metadata$per_level[[1]]$optimal_score))
  expect_true(result$stopped_early)
  expect_lt(result$iterations_run, 50000)

  # The 500-iteration test above leaves this design around 230; converging
  # takes it to roughly 70-80, so the gap is what the long budget buys.
  expect_lt(result$score, 150)
})
