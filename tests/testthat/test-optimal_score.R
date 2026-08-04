test_that(".balance_score_min matches the score of an evenly split layout", {
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
  df <- data.frame(
    row = rep(1:3, times = 4),
    col = rep(1:4, each = 3),
    treatment = rep(c("A", "B", "C"), 4)
  )

  # var(1, 1, 1, 2) * 4 = 1
  expect_equal(.balance_score_min(df, "treatment", "row"), 1)
  expect_equal(.balance_score_min(df, "treatment", "col"), 0)
  expect_equal(.balance_score_min(df, "treatment", c("row", "col")), 1)
})

test_that(".balance_score_min is the minimum over every arrangement", {
  reps <- c(A = 2, B = 2, C = 2, D = 2)
  df <- data.frame(
    treatment = rep(names(reps), reps),
    row = rep(1:2, each = 4),
    col = rep(1:4, times = 2),
    block = rep(rep(1:2, each = 2), times = 2)
  )
  spatial_cols <- c("row", "col", "block")

  bound <- .balance_score_min(df, "treatment", spatial_cols)
  # col: var(0, 0, 1, 1) * 4 = 4/3, row: 0, block: 0
  expect_equal(bound, 4 / 3)

  # every distinct permutation
  arrangements <- list()
  assign_treatment <- function(assigned, k) {
    if (k > length(reps)) {
      arrangements[[length(arrangements) + 1L]] <<- assigned
      return(invisible(NULL))
    }
    for (idx in combn(which(is.na(assigned)), reps[[k]], simplify = FALSE)) {
      next_assigned <- assigned
      next_assigned[idx] <- names(reps)[k]
      assign_treatment(next_assigned, k + 1L)
    }
    return(invisible(NULL))
  }
  assign_treatment(rep(NA_character_, sum(reps)), 1L)
  # 8!/2!/2!/2!/2!
  expect_length(arrangements, factorial(8) / 2^4)

  scores <- vapply(
    arrangements,
    function(arrangement) {
      df$treatment <- arrangement
      return(calculate_balance_score(df, "treatment", spatial_cols))
    },
    numeric(1)
  )
  expect_equal(min(scores), bound)
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

  # Each of the 4 columns holds 5 plots over 4 treatments and contributes
  # 1 * 3 / (4 * 3); the 4-plot rows divide evenly
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

  # A negative ring weight rewards like-neighbours, so adjacency can go below zero
  expect_true(is.na(
    .optimal_score(
      df,
      "treatment",
      spatial_cols,
      objective_function,
      ring_weights = c(1, -1)
    )
  ))
  expect_equal(
    .optimal_score(
      df,
      "treatment",
      spatial_cols,
      objective_function,
      ring_weights = c(1, 2)
    ),
    1
  )

  # An NA weight is unknown rather than non-negative
  expect_true(is.na(
    .optimal_score(
      df,
      "treatment",
      spatial_cols,
      objective_function,
      bal_weight = NA_real_
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

  # Names merely prefixed with `relationship` or `ring_weights` are not covered:
  # `$` partial-matches those, which disables the bound
  expect_equal(
    .optimal_score(
      df,
      "treatment",
      spatial_cols,
      objective_function,
      ring_type = "chebyshev",
      row_column = "row"
    ),
    1
  )
})

test_that(".balance_score_min bounds a layout with missing plots", {
  # The bound uses the non-NA count per spatial level, which only stays fixed
  # because neighbour generation never relocates an NA - pinned below
  df <- data.frame(
    row = factor(rep(1:4, each = 3)),
    col = factor(rep(1:3, times = 4)),
    blk = factor(rep(1:2, each = 6)),
    treatment = factor(rep(LETTERS[1:3], 4))
  )
  df$treatment[c(1, 9, 11)] <- NA
  spatial_cols <- c("row", "col")

  bound <- .balance_score_min(df, "treatment", spatial_cols)
  expect_equal(bound, 1)

  na_positions <- which(is.na(df$treatment))
  current <- df
  for (i in 1:200) {
    for (swap_all in c(FALSE, TRUE)) {
      current <- generate_neighbour(
        current,
        "treatment",
        "blk",
        1,
        swap_all,
        swap_all
      )$design
      expect_identical(which(is.na(current$treatment)), na_positions)
      expect_gte(
        calculate_balance_score(current, "treatment", spatial_cols),
        bound
      )
    }
  }
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

test_that("speed does not stop on the optimal score for negative ring weights", {
  # A negative ring weight rewards like-neighbours, so the balance floor is not
  # a floor for the whole score. `bal_weight = 20` keeps the achievable score
  # positive, clear of the `.Machine$double.eps` early stop.
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
        ring_dists = c(1, 2),
        ring_weights = c(1, -1),
        optimise_params = optim_params(bal_weight = 20),
        iterations = 4000,
        seed = 7,
        quiet = FALSE
      )
    ),
    "row and col are used as row and column, respectively"
  )

  expect_no_match(output, "Optimal score reached")
  expect_true(is.na(result$metadata$per_level[[1]]$optimal_score))
  expect_gt(result$iterations_run, 1)
  expect_lt(result$score, 20)
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
