test_that(".balance_score_min matches the score of an evenly split layout", {
  # 3 rows of 3 plots and 3 columns of 3 plots, 3 treatments: every level
  # divides evenly, so the lowest achievable balance score is 0
  df <- data.frame(
    row = rep(1:3, 3),
    col = rep(1:3, each = 3),
    treatment = c(1, 2, 3, 3, 1, 2, 2, 3, 1)
  )
  expect_equal(.balance_score_min(df, "treatment", c("row", "col")), 0)
  expect_equal(
    .balance_score_min(df, "treatment", c("row", "col")),
    calculate_balance_score(df, "treatment", c("row", "col"))
  )
})

test_that(".balance_score_min uses the as-equal-as-possible split per level", {
  # Each of the 3 rows holds 4 plots over 3 treatments: r = 1, so
  # r(t - r) / (t(t - 1)) = 1/3 each. The 4 columns hold 3 plots over 3
  # treatments and contribute nothing.
  df <- data.frame(
    row = rep(1:3, times = 4),
    col = rep(1:4, each = 3),
    treatment = rep(c("A", "B", "C"), 4)
  )
  expect_equal(.balance_score_min(df, "treatment", "row"), 1)
  expect_equal(.balance_score_min(df, "treatment", "col"), 0)
  expect_equal(.balance_score_min(df, "treatment", c("row", "col")), 1)
})

test_that(".balance_score_min never exceeds an achieved balance score", {
  set.seed(1)
  df <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )
  bound <- .balance_score_min(df, "treatment", c("row", "col"))
  for (i in 1:50) {
    df$treatment <- sample(df$treatment)
    expect_gte(
      calculate_balance_score(df, "treatment", c("row", "col")),
      bound
    )
  }
})

test_that(".balance_score_min handles a single treatment", {
  df <- data.frame(row = 1:4, col = rep(1, 4), treatment = rep("A", 4))
  expect_equal(.balance_score_min(df, "treatment", c("row", "col")), 0)
})

test_that(".optimal_score bounds the default objective", {
  df <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )
  spatial_cols <- c("row", "col")

  # Rows of 4 plots over 4 treatments contribute 0; each of the 4 columns holds
  # 5 plots and contributes 1 * 3 / (4 * 3), so the bound is 1
  expect_equal(
    .optimal_score(df, "treatment", spatial_cols, objective_function),
    1
  )
  # This starting layout attains it once laid out in row/column order
  df <- df[order(df$row, df$col), ]
  expect_equal(
    objective_function(df, "treatment", spatial_cols)$score,
    1
  )
})

test_that(".optimal_score applies bal_weight and ignores adj_weight", {
  df <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )
  spatial_cols <- c("row", "col")

  expect_equal(
    .optimal_score(
      df,
      "treatment",
      spatial_cols,
      objective_function,
      bal_weight = 2
    ),
    2
  )
  expect_equal(
    .optimal_score(
      df,
      "treatment",
      spatial_cols,
      objective_function,
      adj_weight = 5
    ),
    1
  )
  expect_equal(
    .optimal_score(
      df,
      "treatment",
      spatial_cols,
      objective_function,
      bal_weight = 0
    ),
    0
  )
})

test_that(".optimal_score gives up when no bound can be derived", {
  df <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )
  spatial_cols <- c("row", "col")

  # Non-default objective
  expect_true(is.na(
    .optimal_score(df, "treatment", spatial_cols, objective_function_piepho)
  ))
  expect_true(is.na(
    .optimal_score(df, "treatment", spatial_cols, objective_function_factorial)
  ))

  # A relationship matrix may score neighbour pairs below zero
  rel <- prep_relationship(matrix(
    0.5,
    nrow = 4,
    ncol = 4,
    dimnames = list(LETTERS[1:4], LETTERS[1:4])
  ))
  expect_true(is.na(
    .optimal_score(
      df,
      "treatment",
      spatial_cols,
      objective_function,
      relationship = rel
    )
  ))

  # Negative weights invert the direction of optimisation
  expect_true(is.na(
    .optimal_score(
      df,
      "treatment",
      spatial_cols,
      objective_function,
      adj_weight = -1
    )
  ))
  expect_true(is.na(
    .optimal_score(
      df,
      "treatment",
      spatial_cols,
      objective_function,
      bal_weight = -1
    )
  ))

  # Swaps can move an NA treatment between spatial levels
  df$treatment[1] <- NA
  expect_true(is.na(
    .optimal_score(df, "treatment", spatial_cols, objective_function)
  ))
})

test_that(".is_optimal tolerates rounding and rejects a missing bound", {
  expect_true(.is_optimal(1, 1))
  expect_true(.is_optimal(1 + 1e-12, 1))
  expect_true(.is_optimal(0.5, 1))
  expect_false(.is_optimal(1.001, 1))
  expect_false(.is_optimal(0, NA_real_))
})

test_that("speed stops without iterating on an already-optimal design", {
  # Rows hold 4 of the 4 treatments; columns of 5 plots force a balance of 1
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  expect_message(
    output <- capture_output(
      result <- speed(
        data = test_data,
        swap = "treatment",
        spatial_factors = ~ row + col,
        iterations = 5000,
        seed = 42,
        quiet = FALSE
      )
    ),
    "row and col are used as row and column, respectively"
  )

  expect_match(output, "Optimal score reached at iteration 1")
  expect_equal(result$score, 1)
  expect_equal(result$iterations_run, 1)
  expect_true(result$stopped_early)
  # The design is returned untouched, only re-sorted by row then column
  sorted <- test_data[order(test_data$row, test_data$col), ]
  expect_equal(
    as.character(result$design_df$treatment),
    as.character(sorted$treatment)
  )
})

test_that("speed stops as soon as the optimal score is reached mid-run", {
  test_data <- data.frame(
    row = rep(1:4, times = 5),
    col = rep(1:5, each = 4),
    treatment = rep(LETTERS[1:4], 5)
  )

  expect_message(
    output <- capture_output(
      result <- speed(
        data = test_data,
        swap = "treatment",
        spatial_factors = ~ row + col,
        iterations = 5000,
        seed = 42,
        quiet = FALSE
      )
    ),
    "row and col are used as row and column, respectively"
  )

  # The 4 rows of 5 plots over 4 treatments leave a balance floor of 1
  expect_match(output, "Optimal score reached at iteration")
  expect_equal(result$score, 1)
  expect_gt(result$iterations_run, 1)
  expect_lt(result$iterations_run, 5000)
  expect_true(result$stopped_early)
})

test_that("speed keeps running when the optimal score is out of reach", {
  # Unequal replication puts the bound out of reach
  test_data <- data.frame(
    row = rep(1:3, times = 4),
    col = rep(1:4, each = 3),
    treatment = c(rep("A", 6), rep("B", 3), rep("C", 3))
  )

  expect_message(
    output <- capture_output(
      result <- speed(
        data = test_data,
        swap = "treatment",
        spatial_factors = ~ row + col,
        iterations = 500,
        early_stop_iterations = 500,
        seed = 42,
        quiet = FALSE
      )
    ),
    "row and col are used as row and column, respectively"
  )

  expect_no_match(output, "Optimal score reached")
  expect_gt(result$score, result$metadata$per_level[[1]]$optimal_score)
  expect_equal(result$iterations_run, 500)
})

test_that("speed does not stop on the optimal score for custom objectives", {
  custom_objective <- function(layout_df, swap, spatial_cols, ...) {
    return(list(
      score = calculate_balance_score(layout_df, swap, spatial_cols)
    ))
  }

  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  expect_message(
    output <- capture_output(
      result <- speed(
        data = test_data,
        swap = "treatment",
        spatial_factors = ~ row + col,
        obj_function = custom_objective,
        iterations = 200,
        early_stop_iterations = 200,
        seed = 42,
        quiet = FALSE
      )
    ),
    "row and col are used as row and column, respectively"
  )

  expect_no_match(output, "Optimal score reached")
  expect_true(is.na(result$metadata$per_level[[1]]$optimal_score))
  expect_equal(result$iterations_run, 200)
})

test_that("summary reports the optimal score per level", {
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  result <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 500,
    seed = 42,
    quiet = TRUE
  )

  s <- summary(result)
  expect_equal(s$per_level[[1]]$score$optimal, 1)
  expect_output(print(s), "Optimal:")
  expect_output(print(s), "reached")
})
