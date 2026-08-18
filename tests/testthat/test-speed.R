# Core `speed()` behaviour: entry point, validation, stopping rules,
# seeds and value round-tripping. Layout, hierarchy, factorial and output
# tests live in the sibling test-speed-*.R files.

test_that("speed runs without errors", {
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

test_that("speed returns correct output structure", {
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
    optimise_params = optim_params(stop_at_optimal = FALSE),
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
      "seed",
      "metadata"
    )
  )

  expect_true(is.data.frame(result$design_df))
  expect_true(is.numeric(result$score))
  expect_true(is.numeric(result$scores))
  expect_true(is.numeric(result$temperatures))
  expect_true(is.logical(result$stopped_early))
  expect_true(is.character(result$treatments))

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

test_that("speed throws error for missing columns", {
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

test_that("speed throws error for invalid spatial factors", {
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

test_that("speed stops early when no improvement", {
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

test_that("speed stops without iterating on an already-optimal design", {
  #      [,1] [,2] [,3] [,4]
  # [1,] "A"  "B"  "C"  "D"
  # [2,] "B"  "C"  "D"  "A"
  # [3,] "C"  "D"  "A"  "B"
  # [4,] "D"  "A"  "B"  "C"
  # [5,] "A"  "B"  "C"  "D"
  test_data <- expand.grid(row = 1:5, col = 1:4)
  test_data$treatment <- LETTERS[1:4]

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

  expect_match(output, "Optimal score reached at iteration 1 for")
  expect_equal(result$score, 1)
  expect_equal(result$iterations_run, 1)
  expect_true(result$stopped_early)
  # The design is returned untouched, only re-sorted by row then column
  sorted <- test_data[order(test_data$row, test_data$col), ]
  expect_equal(result$design_df$treatment, sorted$treatment)
})

test_that("speed stops as soon as the optimal score is reached mid-run", {
  test_data <- expand.grid(row = 1:4, col = 1:5)
  test_data$treatment <- LETTERS[1:4]

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

test_that("speed keeps running when stop_at_optimal is FALSE", {
  test_data <- expand.grid(row = 1:4, col = 1:5)
  test_data$treatment <- LETTERS[1:4]

  expect_message(
    output <- capture_output(
      result <- speed(
        data = test_data,
        swap = "treatment",
        spatial_factors = ~ row + col,
        optimise_params = optim_params(stop_at_optimal = FALSE),
        iterations = 500,
        early_stop_iterations = 500,
        seed = 42,
        quiet = FALSE
      )
    ),
    "row and col are used as row and column, respectively"
  )

  expect_no_match(output, "Optimal score reached")
  # the bound is still reported, it just no longer stops the run
  expect_equal(result$metadata$per_level[[1]]$optimal_score, 1)
  expect_equal(result$iterations_run, 500)
})

test_that("speed keeps running when the optimal score is out of reach", {
  # having high rep for 1 treatment makes optimal score out of reach
  test_data <- expand.grid(row = 1:3, col = 1:4)
  test_data$treatment <- c(rep("A", 6), rep(c("B", "C"), 3))

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

  test_data <- expand.grid(row = 1:5, col = 1:4)
  test_data$treatment <- LETTERS[1:4]

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

test_that("speed does not stop on the optimal score (NA) for negative weights", {
  test_data <- expand.grid(row = 1:5, col = 1:4)
  test_data$treatment <- LETTERS[1:4]

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

test_that("speed produces reproducible results with seed", {
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

test_that("speed respects swap_within boundaries", {
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

  block1_treatments <- unique(result$design_df$treatment[
    result$design_df$block == 1
  ])
  block2_treatments <- unique(result$design_df$treatment[
    result$design_df$block == 2
  ])
  expect_true(all(block1_treatments %in% LETTERS[1:5]))
  expect_true(all(block2_treatments %in% LETTERS[6:10]))
  expect_false(any(block1_treatments %in% block2_treatments))
  expect_false(any(block2_treatments %in% block1_treatments))

  vdiffr::expect_doppelganger("speed_blocks", autoplot(result))
})

test_that("speed runs with random initialisation", {
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
    optimise_params = optim_params(stop_at_optimal = FALSE),
    seed = 42,
    quiet = TRUE
  )

  result_random <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 1000,
    optimise_params = optim_params(
      random_initialisation = TRUE,
      stop_at_optimal = FALSE
    ),
    seed = 42,
    quiet = TRUE
  )

  expect_named(
    result_random,
    c(
      "design_df",
      "score",
      "scores",
      "temperatures",
      "iterations_run",
      "stopped_early",
      "treatments",
      "seed",
      "metadata"
    )
  )

  expect_true(is.data.frame(result_random$design_df))
  expect_true(is.numeric(result_random$score))
  expect_true(is.numeric(result_random$scores))
  expect_true(is.numeric(result_random$temperatures))
  expect_true(is.logical(result_random$stopped_early))
  expect_true(is.character(result_random$treatments))

  expect_equal(nrow(result_random$design_df), 20)
  expect_equal(ncol(result_random$design_df), 3)
  expect_equal(result_random$score, 1)
  expect_equal(length(result_random$scores), 1000)
  expect_equal(length(result_random$temperatures), 1000)
  expect_equal(result_random$iterations_run, 1000)
  expect_equal(result_random$stopped_early, FALSE)
  expect_equal(result_random$treatments, c("A", "B", "C", "D"))

  expect_false(isTRUE(all.equal(
    result_random$design_df$treatment,
    result$design_df$treatment
  )))
})

test_that("speed runs a with variation of row and column columns", {
  test_data <- data.frame(
    Row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  result <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ Row + col,
    iterations = 1000,
    seed = 42,
    quiet = TRUE
  )

  result$design_df$row <- result$design_df$Row
  vdiffr::expect_doppelganger("speed_Row", autoplot(result))
})

test_that("speed runs a without row", {
  test_data <- data.frame(
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  expect_warning(
    speed(
      data = test_data,
      swap = "treatment",
      spatial_factors = ~col,
      iterations = 1000,
      seed = 42,
      quiet = TRUE
    ),
    paste0(
      "Cannot infer row in the design data frame. speed.adj_weight is set to 0 for this call. If this is not",
      " intended, provide `grid_factors` argument."
    )
  )
})

test_that("speed runs a without column", {
  test_data <- data.frame(
    row = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  expect_warning(
    speed(
      data = test_data,
      swap = "treatment",
      spatial_factors = ~row,
      iterations = 1000,
      seed = 42,
      quiet = TRUE
    ),
    paste0(
      "Cannot infer column in the design data frame. speed.adj_weight is set to 0 for this call. If this is",
      " not intended, provide `grid_factors` argument."
    )
  )
})

test_that("speed runs with grid_factors", {
  test_data <- data.frame(
    lane = rep(1:5, times = 4),
    position = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  result <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ lane + position,
    grid_factors = list(dim1 = "lane", dim2 = "position"),
    iterations = 1000,
    seed = 42,
    quiet = TRUE
  )

  expect_equal(nrow(result$design_df), 20)
  expect_equal(ncol(result$design_df), 3)
  expect_equal(result$score, 1)
})

test_that("`grid_factors$by` is checked before optimising", {
  test_data <- data.frame(
    lane = rep(1:5, times = 4),
    position = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  speed_by <- function(by) {
    return(speed(
      data = test_data,
      swap = "treatment",
      spatial_factors = ~ lane + position,
      grid_factors = list(dim1 = "lane", dim2 = "position", by = by),
      iterations = 10,
      seed = 42,
      quiet = TRUE
    ))
  }

  # A mistyped column would otherwise be ignored and every grid pooled
  expect_error(speed_by("site"), "not found in", fixed = TRUE)
  expect_error(
    speed_by(c("site", "block")),
    "must be a single column name",
    fixed = TRUE
  )
  expect_error(speed_by(1), "must be a single column name", fixed = TRUE)
})

test_that("speed runs without seed", {
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
    quiet = TRUE
  )
  expect_s3_class(result, "design")
})

test_that("speed generates seed automatically from .Random.seed[3] when seed=NULL for simple designs", {
  test_data <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )

  result <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 10, # Very small number for quick test
    seed = NULL, # Explicitly set to NULL
    quiet = TRUE
  )

  expect_true(is.numeric(result$seed))
  expect_true(length(result$seed) == 1)
  expect_false(is.na(result$seed))

  # The seed should be within the range of possible .Random.seed[3] values
  # (This is a reasonable but not strict test since .Random.seed[3] can vary widely)
  expect_true(is.finite(result$seed))

  generated_seed <- result$seed

  result_with_generated_seed <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 10,
    seed = generated_seed,
    quiet = TRUE
  )

  expect_equal(result$design_df, result_with_generated_seed$design_df)
  expect_equal(result$seed, result_with_generated_seed$seed)
})

test_that("speed generates seed automatically from .Random.seed[3] when seed=NULL for hierarchical designs", {
  df_split <- data.frame(
    row = rep(1:9, each = 3),
    col = rep(1:3, times = 9),
    wholeplot_treatment = rep(LETTERS[1:3], times = 9),
    subplot_treatment = rep(rep(letters[1:3], 3), each = 3),
    block = rep(1:3, each = 9)
  )

  result <- speed(
    df_split,
    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
    swap_within = list(wp = "block", sp = "wholeplot_treatment"),
    spatial_factors = ~ row + col,
    iterations = list(wp = 5, sp = 5), # Very small numbers for quick test
    seed = NULL, # Explicitly set to NULL
    quiet = TRUE
  )

  expect_true(is.numeric(result$seed))
  expect_true(length(result$seed) == 1)
  expect_false(is.na(result$seed))

  # The seed should be within the range of possible .Random.seed[3] values
  expect_true(is.finite(result$seed))

  generated_seed <- result$seed

  result_with_generated_seed <- speed(
    df_split,
    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
    swap_within = list(wp = "block", sp = "wholeplot_treatment"),
    spatial_factors = ~ row + col,
    iterations = list(wp = 5, sp = 5),
    seed = generated_seed,
    quiet = TRUE
  )

  expect_equal(result$design_df, result_with_generated_seed$design_df)
  expect_equal(result$seed, result_with_generated_seed$seed)

  expect_true(is.list(result$scores))
  expect_true(is.list(result$temperatures))
  expect_true(is.list(result$treatments))
})

test_that("speed produces different results when seed=NULL across different runs", {
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], each = 5)
  )

  result1 <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 50,
    seed = NULL,
    quiet = TRUE
  )

  set.seed(NULL) # Reset to use current time
  dummy <- runif(10) # Advance the state

  result2 <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 50,
    seed = NULL,
    quiet = TRUE
  )

  expect_false(identical(result1$seed, result2$seed))

  # Note: There's a tiny chance they could be the same by coincidence, but extremely unlikely
  expect_false(identical(
    result1$design_df$treatment,
    result2$design_df$treatment
  ))
})

test_that("speed runs with n random initialisation", {
  df <- expand.grid(col = 1:4, row = 1:5)
  df$treatment <- LETTERS[1:4]
  initial_score <- objective_function(df, "treatment", c("row", "col"))$score

  result <- speed(
    data = df,
    swap = "treatment",
    iterations = 1000,
    early_stop_iterations = 500,
    optimise_params = optim_params(random_initialisation = 10),
    seed = 112,
    quiet = TRUE
  )

  expect_lt(result$scores[1], initial_score)
  expect_equal(result$score, 1)
  expect_true(is.data.frame(result$design_df))
  expect_equal(sort(result$design_df$treatment), sort(df$treatment))
})

test_that("speed runs with legacy options(speed.{option})", {
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
    optimise_params = optim_params(random_initialisation = TRUE),
    seed = 42,
    quiet = TRUE
  )

  withr::with_options(
    list(speed.random_initialisation = TRUE),
    suppressWarnings(expect_warning(
      {
        result_legacy <- speed(
          data = test_data,
          swap = "treatment",
          spatial_factors = ~ row + col,
          iterations = 1000,
          seed = 42,
          quiet = TRUE
        )
      },
      "Setting options with `options\\(speed.\\{option\\}=...\\)` is deprecated. Please use `optim_params\\(\\)` instead."
    ))
  )

  # The two designs are built from differently-written speed() calls, so their
  # captured metadata$call differs by construction - compare the rest.
  result_legacy$metadata$call <- NULL
  result$metadata$call <- NULL
  expect_true(isTRUE(all.equal(result_legacy, result)))
})

test_that("speed honours ring_dists and ring_weights via ...", {
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  base <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 100,
    seed = 42,
    quiet = TRUE
  )
  with_rings <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 100,
    seed = 42,
    ring_dists = c(1, 2),
    ring_weights = c(1, 5),
    quiet = TRUE
  )

  expect_false(isTRUE(all.equal(base$score, with_rings$score)))
})

test_that("speed errors when adj_weight/bal_weight go through ...", {
  test_data <- data.frame(
    row = rep(1:3, times = 3),
    col = rep(1:3, each = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  expect_error(
    speed(
      data = test_data,
      swap = "treatment",
      iterations = 5,
      seed = 1,
      quiet = TRUE,
      bal_weight = 2
    ),
    "must be passed via `optim_params\\(\\)`"
  )
  expect_error(
    speed(
      data = test_data,
      swap = "treatment",
      iterations = 5,
      seed = 1,
      quiet = TRUE,
      adj_weight = 0.5
    ),
    "must be passed via `optim_params\\(\\)`"
  )
})

test_that("speed errors when the objective function returns a non-numeric score", {
  bad_obj_function <- function(design, swap, spatial_cols, ...) {
    list(score = "not a number")
  }
  bad_obj_data <- data.frame(
    row = rep(1:4, each = 4),
    col = rep(1:4, times = 4),
    treatment = rep(LETTERS[1:4], 4)
  )

  expect_error(
    speed(
      data = bad_obj_data,
      swap = "treatment",
      spatial_factors = ~ row + col,
      obj_function = bad_obj_function,
      iterations = 10,
      seed = 42,
      quiet = TRUE
    ),
    "`score` from `objective_function` must be numeric\\."
  )
})

test_that("swap_all_blocks changes the optimisation while preserving block composition", {
  dat_blocks <- data.frame(
    row = rep(1:4, each = 6),
    col = rep(1:6, times = 4),
    treat = rep(LETTERS[1:6], times = 4),
    block = rep(1:4, each = 6)
  )

  result_all <- speed(
    dat_blocks,
    swap = "treat",
    swap_within = "block",
    spatial_factors = ~ row + col,
    iterations = 200,
    optimise_params = optim_params(swap_all_blocks = TRUE),
    seed = 1,
    quiet = TRUE
  )
  result_one <- speed(
    dat_blocks,
    swap = "treat",
    swap_within = "block",
    spatial_factors = ~ row + col,
    iterations = 200,
    optimise_params = optim_params(swap_all_blocks = FALSE),
    seed = 1,
    quiet = TRUE
  )

  expect_s3_class(result_all, "design")
  expect_s3_class(result_one, "design")

  # Both settings only ever swap within a block, so each block must keep its
  # full set of treatments exactly once regardless of swap_all_blocks.
  for (result in list(result_all, result_one)) {
    block_treatment_counts <- table(
      result$design_df$block,
      result$design_df$treat
    )
    expect_true(all(block_treatment_counts == 1))
  }

  # swap_all_blocks alters which plots are swapped each step, so the same seed
  # must lead the optimiser down a different path.
  expect_false(identical(
    result_all$design_df$treat,
    result_one$design_df$treat
  ))
})

# Regression test: tibble input must not warn about deprecated row names (#issue)
test_that("speed does not warn about tibble row names when passed a tibble", {
  skip_if_not_installed("tibble")

  test_data <- tibble::tibble(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  expect_no_warning(
    speed(
      data = test_data,
      swap = "treatment",
      spatial_factors = ~ row + col,
      iterations = 100,
      seed = 42,
      quiet = TRUE
    )
  )
})

# Regression test: numeric columns were returned as factor level codes, because
# restoring them ran as.numeric() over a factor instead of over its labels.
test_that("speed returns numeric treatments as values, not level codes", {
  test_data <- data.frame(
    row = rep(1:4, times = 5),
    col = rep(1:5, each = 4),
    treatment = rep(c(10, 100, 30, 9), 5)
  )

  result <- speed(test_data, swap = "treatment", seed = 42, quiet = TRUE)
  treatment <- result$design_df$treatment

  expect_type(treatment, "double")
  expect_setequal(treatment, c(9, 10, 30, 100))
  # Each treatment is still replicated 5 times, just rearranged.
  expect_equal(sort(treatment), sort(test_data$treatment))
})

test_that("speed returns integer treatments as values, not level codes", {
  test_data <- data.frame(
    row = rep(1:4, times = 5),
    col = rep(1:5, each = 4),
    treatment = rep(c(10L, 100L, 30L, 9L), 5)
  )

  result <- speed(test_data, swap = "treatment", seed = 42, quiet = TRUE)
  treatment <- result$design_df$treatment

  expect_type(treatment, "integer")
  expect_setequal(treatment, c(9L, 10L, 30L, 100L))
  expect_equal(sort(treatment), sort(test_data$treatment))
})

test_that("speed preserves non-consecutive numeric row and column values", {
  # row/col are converted to factors alongside the swap column, so they were
  # corrupted in the same way.
  test_data <- data.frame(
    row = rep(c(2, 4, 6, 8), times = 5),
    col = rep(seq(10, 50, 10), each = 4),
    treatment = rep(LETTERS[1:4], 5)
  )

  result <- speed(test_data, swap = "treatment", seed = 42, quiet = TRUE)

  expect_setequal(result$design_df$row, c(2, 4, 6, 8))
  expect_setequal(result$design_df$col, seq(10, 50, 10))
})

test_that("speed rejects a malformed grid_factors with an actionable message", {
  test_data <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )

  expect_error(
    speed(
      test_data,
      swap = "treatment",
      grid_factors = list(dim1 = "row"),
      quiet = TRUE
    ),
    "`grid_factors` must be a list with `dim1` and `dim2`"
  )

  expect_error(
    speed(test_data, swap = "treatment", grid_factors = "row", quiet = TRUE),
    "`grid_factors` must be a list with `dim1` and `dim2`"
  )
})

test_that("speed rejects per-level grid factors and points at `by`", {
  # `infer_row_col()` resolves one pair of axes for the whole design, so a
  # per-level `grid_factors` cannot be honoured on the legacy hierarchical shape.
  test_data <- data.frame(
    range = rep(1:6, each = 4),
    plot = rep(1:4, times = 6),
    wholeplot_treatment = rep(LETTERS[1:3], each = 8),
    subplot_treatment = rep(letters[1:4], 6),
    block = rep(1:2, each = 12)
  )

  err <- expect_error(
    speed(
      test_data,
      swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
      swap_within = list(wp = "block", sp = "wholeplot_treatment"),
      spatial_factors = ~ range + plot,
      grid_factors = list(
        wp = list(dim1 = "range", dim2 = "plot"),
        sp = list(dim1 = "range", dim2 = "plot")
      ),
      iterations = list(wp = 20, sp = 20),
      seed = 1,
      quiet = TRUE
    ),
    "cannot be set per level"
  )

  # `by` is the mechanism that does split a design into separate grids
  expect_match(
    conditionMessage(err),
    "name the grouping column with `by`",
    fixed = TRUE
  )
})

test_that("random_initialise returns immediately on a zero score", {
  # Zeroing both weights makes the objective identically 0, so the early return
  # is exercised without relying on a shuffle finding a perfect layout.
  test_data <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )

  result <- speed(
    test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 10,
    seed = 1,
    quiet = TRUE,
    optimise_params = optim_params(
      random_initialisation = 3,
      adj_weight = 0,
      bal_weight = 0
    )
  )

  expect_equal(result$score, 0)
  expect_setequal(result$design_df$treatment, LETTERS[1:3])
  expect_equal(sort(result$design_df$treatment), sort(test_data$treatment))
})
