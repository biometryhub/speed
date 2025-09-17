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
    iterations = 2000,
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
    iterations = 20000,
    early_stop_iterations = 5000,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(result, "design")

  # Check values
  expect_equal(result$stopped_early, TRUE)

  expect_equal(result$score, 14)
  expect_equal(result$iterations_run, 13264)

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
  expect_equal(result$iterations_run, 742)
  expect_equal(result$stopped_early, TRUE)

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

  # Optimise the design
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

  # Optimise the design
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

  # Optimise the design
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

test_that("speed runs with random initialisation", {
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

  options(speed.random_initialisation = TRUE)
  result_random <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 1000,
    seed = 42,
    quiet = TRUE
  )
  options(speed.random_initialisation = FALSE)

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
      "seed"
    )
  )

  # Check data types
  expect_true(is.data.frame(result_random$design_df))
  expect_true(is.numeric(result_random$score))
  expect_true(is.numeric(result_random$scores))
  expect_true(is.numeric(result_random$temperatures))
  expect_true(is.logical(result_random$stopped_early))
  expect_true(is.character(result_random$treatments))

  # Check values
  expect_equal(nrow(result_random$design_df), 20)
  expect_equal(ncol(result_random$design_df), 3)
  expect_equal(result_random$score, 1)
  expect_equal(length(result_random$scores), 1000)
  expect_equal(length(result_random$temperatures), 1000)
  expect_equal(result_random$iterations_run, 1000)
  expect_equal(result_random$stopped_early, FALSE)
  expect_equal(result_random$treatments, c("A", "B", "C", "D"))

  expect_false(isTRUE(all.equal(result_random$design_df$treatment, result$design_df$treatment)))
})

# Test split plots
test_that("speed handles split plot designs", {
  # Hierarchical split-plot design
  df_split <- data.frame(
    row = rep(1:12, each = 4),
    col = rep(1:4, times = 12),
    block = rep(1:4, each = 12),
    wholeplot = rep(1:12, each = 4),
    wholeplot_treatment = rep(rep(LETTERS[1:3], each = 4), times = 4),
    subplot_treatment = rep(letters[1:4], 12)
  )

  result <- speed(df_split,
                  swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                  swap_within = list(wp = "block", sp = "wholeplot"),
                  early_stop_iterations = list(wp = 1000, sp = 10000),
                  seed = 1, quiet = TRUE)

  # Check the result
  expect_s3_class(result, "design")
  # Check values
  expect_equal(result$score, 100)
  expect_equal(result$iterations_run, 1726)
  expect_equal(result$stopped_early, c(wp = TRUE, sp = TRUE))

  # expect_identical(which(is.na(result$design_df$treatment)), which(is.na(irregular_large_data$treatment)))

  vdiffr::expect_doppelganger("speed_splitplot_wp", autoplot(result, treatments = "wholeplot_treatment"))
  vdiffr::expect_doppelganger("speed_splitplot_sp", autoplot(result, treatments = "subplot_treatment"))
})

# Test split-split plots
test_that("speed handles split-split plot designs", {
  # Hierarchical split-split plot design
  df_split_split <- data.frame(
    row = rep(1:16, each = 9),
    col = rep(1:9, times = 16),
    block = rep(1:4, each = 36),
    wholeplot = rep(rep(1:3, each = 3), times = 16) + rep(0:3 * 3, each = 36),
    wholeplot_treatment = rep(rep(LETTERS[1:3], each = 3), times = 16),
    subplot = rep(1:48, each = 3),
    subplot_treatment = rep(rep(letters[1:4], each = 3), times = 12),
    subsubplot_treatment = rep(c("x", "y", "z"), 48)
  )

  result <- speed(df_split_split,
                  swap = list(wp = "wholeplot_treatment",
                              sp = "subplot_treatment",
                              ssp = "subsubplot_treatment"),
                  swap_within = list(wp = "block",
                                     sp = "wholeplot",
                                     ssp = "subplot"),
                  iterations = list(wp = 500, sp = 500, ssp = 1000),
                  early_stop_iterations = list(wp = 200, sp = 200, ssp = 400),
                  seed = 42, quiet = TRUE)

  # Check the result
  expect_s3_class(result, "design")

  # Check output structure
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
  expect_true(is.list(result$scores))
  expect_true(is.list(result$temperatures))
  expect_true(is.numeric(result$iterations_run))
  expect_true(is.logical(result$stopped_early))
  expect_true(is.list(result$treatments))

  # Check hierarchical structure
  expect_named(result$scores, c("wp", "sp", "ssp"))
  expect_named(result$temperatures, c("wp", "sp", "ssp"))
  expect_named(result$stopped_early, c("wp", "sp", "ssp"))
  expect_named(result$treatments, c("wp", "sp", "ssp"))

  # Check output dimensions
  expect_equal(nrow(result$design_df), 144)  # 16 rows × 9 cols
  expect_equal(ncol(result$design_df), 8)    # All columns preserved
  expect_equal(length(result$scores), 3)
  expect_equal(sapply(result$scores, length), c(wp = 229, sp = 201, ssp = 1000))

  # Check numerical output
  expect_equal(result$score, 497)
  expect_equal(result$iterations_run, 1430)
  expect_equal(result$stopped_early, c(wp = TRUE, sp = TRUE, ssp = FALSE))
  expect_equal(result$seed, 42)

  # Check treatment levels are preserved
  expect_equal(result$treatments$wp, c("A", "B", "C"))
  expect_equal(result$treatments$sp, c("a", "b", "c", "d"))
  expect_equal(result$treatments$ssp, c("x", "y", "z"))

  # Test visualization for each level
  vdiffr::expect_doppelganger("speed_split_split_wp", autoplot(result, treatments = "wholeplot_treatment"))
  vdiffr::expect_doppelganger("speed_split_split_sp", autoplot(result, treatments = "subplot_treatment"))
  vdiffr::expect_doppelganger("speed_split_split_ssp", autoplot(result, treatments = "subsubplot_treatment"))
})

# Test strip plots
test_that("speed handles strip plot designs", {
  df_strip <- data.frame(
    row = rep(1:12, each = 6),  # 12 rows total (4 rows per block x 6 blocks)
    col = rep(1:6, times = 12),  # 6 columns repeated
    block = rep(rep(1:2, each = 3), times = 4) + rep(0:2*2, each = 24),  # 6 blocks, 12 plots each
    vertical_treatment = rep(rep(LETTERS[1:3], times = 2), times = 12),  # A, B, C
    horizontal_treatment = rep(rep(letters[1:4], each = 6), times = 3),  # a, b, c, d
    plot_in_block = rep(1:12, times = 6)
  )

  result <- speed(df_strip,
                  swap = list(ht = "horizontal_treatment", vt = "vertical_treatment"),
                  swap_within = list(ht = "block", vt = "block"),
                  iterations = list(ht = 500, vt = 500),
                  early_stop_iterations = list(ht = 200, vt = 200),
                  seed = 42, quiet = TRUE)

  # Check the result
  expect_s3_class(result, "design")

  # Check output structure
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
  expect_true(is.list(result$scores))
  expect_true(is.list(result$temperatures))
  expect_true(is.numeric(result$iterations_run))
  expect_true(is.logical(result$stopped_early))
  expect_true(is.list(result$treatments))

  # Check hierarchical structure
  expect_named(result$scores, c("ht", "vt"))
  expect_named(result$temperatures, c("ht", "vt"))
  expect_named(result$stopped_early, c("ht", "vt"))
  expect_named(result$treatments, c("ht", "vt"))

  # Check output dimensions
  expect_equal(nrow(result$design_df), 72)  # 16 rows × 9 cols
  expect_equal(ncol(result$design_df), 6)    # All columns preserved
  expect_equal(length(result$scores), 2)
  expect_equal(sapply(result$scores, length), c(ht = 232, vt = 369))

  # Check numerical output
  expect_equal(result$score, 145)
  expect_equal(result$iterations_run, 601)
  expect_equal(result$stopped_early, c(ht = TRUE, vt = TRUE))
  expect_equal(result$seed, 42)

  # Check treatment levels are preserved
  expect_equal(result$treatments$ht, c("a", "b", "c", "d"))
  expect_equal(result$treatments$vt, c("A", "B", "C"))


  # Test visualization for each level
  vdiffr::expect_doppelganger("speed_strip_ht", autoplot(result, treatments = "horizontal_treatment"))
  vdiffr::expect_doppelganger("speed_strip_vt", autoplot(result, treatments = "vertical_treatment"))
})

# Test autoplot with factor columns
test_that("autoplot handles factor row and column inputs", {
  # Sample data with factor row and col
  test_data <- data.frame(
    row = factor(rep(1:5, times = 4)),
    col = factor(rep(1:4, each = 5)),
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

  # Should not error when plotting with factor columns
  expect_no_error({
    plot <- autoplot(result)
  })

  plot <- autoplot(result)
  expect_contains(class(plot), "ggplot")

  vdiffr::expect_doppelganger("autoplot_factor_row_col", autoplot(result))
})

test_that("autoplot handles factor row only", {
  # Sample data with factor row but numeric col
  test_data <- data.frame(
    row = factor(rep(1:5, times = 4)),
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

  # Should not error when plotting with factor row
  expect_no_error({
    plot <- autoplot(result)
  })

  plot <- autoplot(result)
  expect_contains(class(plot), "ggplot")

  vdiffr::expect_doppelganger("autoplot_factor_row", autoplot(result))
})

test_that("autoplot handles factor column only", {
  # Sample data with numeric row but factor col
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = factor(rep(1:4, each = 5)),
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

  # Should not error when plotting with factor column
  expect_no_error({
    plot <- autoplot(result)
  })

  plot <- autoplot(result)
  expect_contains(class(plot), "ggplot")

  vdiffr::expect_doppelganger("autoplot_factor_col", autoplot(result))
})

test_that("autoplot handles factor columns with blocks", {
  # Sample data with factor row/col and blocks
  test_data <- data.frame(
    row = factor(rep(1:6, each = 4)),
    col = factor(rep(1:4, times = 6)),
    treatment = rep(LETTERS[1:8], 3),
    block = rep(1:3, each = 8)
  )

  result <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col,
    iterations = 100,
    seed = 42,
    quiet = TRUE
  )

  # Should not error when plotting with factor columns and blocks
  expect_no_error({
    plot <- autoplot(result)
  })

  plot <- autoplot(result)
  expect_contains(class(plot), "ggplot")

  vdiffr::expect_doppelganger("autoplot_factor_with_blocks", autoplot(result))
})

test_that("autoplot handles factor columns with custom column names", {
  # Sample data with custom column names that are factors
  test_data <- data.frame(
    Row = factor(rep(1:5, times = 4)),
    Column = factor(rep(1:4, each = 5)),
    trt = rep(LETTERS[1:4], 5)
  )

  result <- speed(
    data = test_data,
    swap = "trt",
    spatial_factors = ~ Row + Column,
    iterations = 100,
    seed = 42,
    quiet = TRUE
  )

  # Should not error when plotting with custom factor column names
  expect_no_error({
    plot <- autoplot(result, row = Row, column = Column, treatments = trt)
  })

  plot <- autoplot(result, row = Row, column = Column, treatments = trt)
  expect_contains(class(plot), "ggplot")

  vdiffr::expect_doppelganger("autoplot_factor_custom_names",
                              autoplot(result, row = Row, column = Column, treatments = trt))
})

test_that("autoplot fails with factor columns with character levels", {
  # Sample data with factor columns that have character levels
  # Too hard to predict plot layout with character levels
  test_data <- data.frame(
    row = factor(rep(paste0("R", 1:5), times = 4)),
    col = factor(rep(paste0("C", 1:4), each = 5)),
    treatment = rep(LETTERS[1:4], 5)
  )

  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          expect_warning(
            expect_error(speed(data = test_data, swap = "treatment", iterations = 100, seed = 42, quiet = TRUE),
                         "invalid 'nrow' value \\(too large or NA\\)"),
            "NAs introduced by coercion"),
          "no non-missing arguments to max; returning -Inf"),
        "no non-missing arguments to max; returning -Inf"),
      "NAs introduced by coercion to integer range"),
    "NAs introduced by coercion")
})

test_that("autoplot handles mixed factor and numeric columns in hierarchical designs", {
  # Hierarchical design with mixed factor/numeric columns
  df_split <- data.frame(
    row = factor(rep(1:12, times = 6)),
    col = rep(1:6, each = 12),  # Keep col as numeric
    block = rep(1:6, each = 12),
    wholeplot_treatment = rep(rep(paste0("WP", LETTERS[1:3]), each = 4), times = 6),
    subplot_treatment = rep(paste0("SP", letters[1:4]), times = 18)
  )

  result <- speed(df_split,
                  swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                  swap_within = list(wp = "block", sp = "wholeplot_treatment"),
                  spatial_factors = ~ row + col,
                  iterations = 100,
                  seed = 42,
                  quiet = TRUE)

  # Should not error with mixed factor/numeric in hierarchical design
  expect_no_error({
    plot_wp <- autoplot(result, treatments = "wholeplot_treatment")
    plot_sp <- autoplot(result, treatments = "subplot_treatment")
  })

  plot_wp <- autoplot(result, treatments = "wholeplot_treatment")
  plot_sp <- autoplot(result, treatments = "subplot_treatment")
  expect_contains(class(plot_wp), "ggplot")
  expect_contains(class(plot_sp), "ggplot")

  vdiffr::expect_doppelganger("autoplot_hierarchical_mixed_factors_wp", plot_wp)
  vdiffr::expect_doppelganger("autoplot_hierarchical_mixed_factors_sp", plot_sp)
})

test_that("autoplot error handling with missing columns still works with factors", {
  # Test that error handling works even when some columns are factors
  test_data <- data.frame(
    row = factor(rep(1:5, times = 4)),
    col = factor(rep(1:4, each = 5)),
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

  # Should give helpful error when specifying non-existent column
  expect_error(
    autoplot(result, row = nonexistent_row),
    "'nonexistent_row' not found"
  )

  expect_error(
    autoplot(result, column = nonexistent_col),
    "'nonexistent_col' not found"
  )

  expect_error(
    autoplot(result, treatments = nonexistent_treatment),
    "'nonexistent_treatment' not found"
  )
})

# Test autoplot legend parameter
test_that("autoplot legend parameter controls legend visibility", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 100,
    seed = 42,
    quiet = TRUE
  )

  # Test default behavior (legend = FALSE)
  plot_no_legend <- autoplot(result)
  expect_contains(class(plot_no_legend), "ggplot")

  # Check that legend is hidden by default
  plot_build <- ggplot2::ggplot_build(plot_no_legend)
  expect_equal(plot_no_legend$theme$legend.position, "none")

  # Test with legend = TRUE
  plot_with_legend <- autoplot(result, legend = TRUE)
  expect_contains(class(plot_with_legend), "ggplot")

  # Check that legend is shown when legend = TRUE
  expect_equal(plot_with_legend$theme$legend.position, "right")

  # Test visual regression for both cases
  vdiffr::expect_doppelganger("autoplot_no_legend", autoplot(result, legend = FALSE))
  vdiffr::expect_doppelganger("autoplot_with_legend", autoplot(result, legend = TRUE))
})

# Test autoplot legend parameter with hierarchical designs
test_that("autoplot legend parameter works with hierarchical designs", {
  # Hierarchical split-plot design
  df_split <- data.frame(
    row = rep(1:6, each = 4),
    col = rep(1:4, times = 6),
    block = rep(1:2, each = 12),
    wholeplot = rep(1:6, each = 4),
    wholeplot_treatment = rep(rep(LETTERS[1:3], each = 4), times = 2),
    subplot_treatment = rep(letters[1:4], 6)
  )

  result <- speed(df_split,
                  swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                  swap_within = list(wp = "block", sp = "wholeplot"),
                  iterations = list(wp = 50, sp = 50),
                  seed = 42, quiet = TRUE)

  # Test with wholeplot treatments
  plot_wp_no_legend <- autoplot(result, treatments = "wholeplot_treatment", legend = FALSE)
  plot_wp_with_legend <- autoplot(result, treatments = "wholeplot_treatment", legend = TRUE)

  expect_contains(class(plot_wp_no_legend), "ggplot")
  expect_contains(class(plot_wp_with_legend), "ggplot")

  # Check legend visibility
  expect_equal(plot_wp_no_legend$theme$legend.position, "none")
  expect_equal(plot_wp_with_legend$theme$legend.position, "right")

  # Test with subplot treatments
  plot_sp_no_legend <- autoplot(result, treatments = "subplot_treatment", legend = FALSE)
  plot_sp_with_legend <- autoplot(result, treatments = "subplot_treatment", legend = TRUE)

  expect_contains(class(plot_sp_no_legend), "ggplot")
  expect_contains(class(plot_sp_with_legend), "ggplot")

  # Check legend visibility
  expect_equal(plot_sp_no_legend$theme$legend.position, "none")
  expect_equal(plot_sp_with_legend$theme$legend.position, "right")

  # Test visual regression
  vdiffr::expect_doppelganger("autoplot_hierarchical_no_legend", plot_wp_no_legend)
  vdiffr::expect_doppelganger("autoplot_hierarchical_with_legend", plot_wp_with_legend)
})



# Test cases from Jules_example_cases.R

# Test: 2D blocking with rowBlocks and colBlocks
test_that("speed handles 2D blocking with row and column blocks", {
  # Create data frame in a single call
  dat_2d_blocking <- data.frame(
    row = rep(1:20, each = 20),
    col = rep(1:20, 20),
    treat = rep(paste("V", 1:40, sep = ""), 10),
    rowBlock = rep(1:10, each = 40),
    colBlock = rep(rep(1:10, times = 20), each = 2)
  )
  dat_2d_blocking <- dat_2d_blocking[order(dat_2d_blocking$col, dat_2d_blocking$row), ]

  # Store original options and set test options
  old_options <- options()
  on.exit(options(old_options))
  options(
    speed.swap_count = 5,
    speed.swap_all_blocks = TRUE,
    speed.adaptive_swaps = TRUE,
    speed.cooling_rate = 0.99,
    speed.random_initialisation = TRUE
  )

  result <- speed(dat_2d_blocking,
                  swap = "treat",
                  swap_within = "rowBlock",
                  spatial_factors = ~ colBlock,
                  iterations = 1000,
                  early_stop_iterations = 500,
                  seed = 123,
                  quiet = TRUE)

  expect_s3_class(result, "design")
  expect_equal(nrow(result$design_df), 400)
  expect_equal(ncol(result$design_df), 5)

  # Check that treatments are balanced within rowBlocks
  rowblock_treatment_counts <- table(result$design_df$rowBlock, result$design_df$treat)
  expect_true(all(rowSums(rowblock_treatment_counts) == 40))

  vdiffr::expect_doppelganger("speed_2d_blocking_rowblock",
                              autoplot(result, treatments = "treat", block = "rowBlock"))
  vdiffr::expect_doppelganger("speed_2d_blocking_colblock",
                              autoplot(result, treatments = "treat", block = "colBlock"))
})

# Test: RCBD with multiple treatment reps in rows and columns
test_that("speed handles RCBD with multiple treatment reps", {
  # Create data frame in a single call
  dat_rcbd <- data.frame(
    row = rep(1:25, each = 20),
    col = rep(1:20, 25),
    treat = rep(paste("V", 1:10, sep = ""), 50),
    block = rep(1:50, each = 10)
  )

  # Store original options and set test options
  old_options <- options()
  on.exit(options(old_options))
  options(speed.swap_count = 3, speed.adaptive_swaps = TRUE)

  result <- speed(dat_rcbd,
                  swap = "treat",
                  swap_within = "block",
                  iterations = 2000,
                  early_stop_iterations = 1000,
                  seed = 42,
                  quiet = TRUE)

  expect_s3_class(result, "design")
  expect_equal(nrow(result$design_df), 500)
  expect_equal(ncol(result$design_df), 4)

  # Check that each block has exactly 10 treatments
  block_sizes <- table(result$design_df$block)
  expect_true(all(block_sizes == 10))

  # Check that treatments are properly distributed within blocks
  block_treatment_counts <- table(result$design_df$block, result$design_df$treat)
  expect_true(all(rowSums(block_treatment_counts) == 10))

  vdiffr::expect_doppelganger("speed_rcbd_multi_reps",
                              autoplot(result, treatments = "treat"))
})

# Test: Partial replication design
test_that("speed handles partial replication designs", {
  set.seed(456)  # For reproducible random sampling

  # Create data frame in a single call for partial rep design
  treats <- sample(paste("V", 1:150, sep = ""), 150, replace = FALSE)
  trep <- sample(treats, 50, replace = FALSE)
  tunrep <- treats[!(treats %in% trep)]
  treat <- unlist(lapply(split(tunrep, rep(1:2, each = 50)), function(el, trep) c(el, trep), trep))

  dat_partial <- data.frame(
    row = rep(1:20, each = 10),
    col = rep(1:10, 20),
    treat = treat,
    block = rep(1:2, each = 100)
  )

  result <- speed(dat_partial,
                  swap = "treat",
                  swap_within = "block",
                  spatial_factors = ~ row + col,
                  iterations = 1000,
                  early_stop_iterations = 500,
                  seed = 42,
                  quiet = TRUE)

  expect_s3_class(result, "design")
  expect_equal(nrow(result$design_df), 200)
  expect_equal(ncol(result$design_df), 4)

  # Check that each block has 100 plots
  block_sizes <- table(result$design_df$block)
  expect_true(all(block_sizes == 100))

  # Check that replicated treatments appear in both blocks
  block1_treats <- unique(result$design_df$treat[result$design_df$block == 1])
  block2_treats <- unique(result$design_df$treat[result$design_df$block == 2])
  replicated_treats <- intersect(block1_treats, block2_treats)
  expect_equal(length(replicated_treats), 50)

  vdiffr::expect_doppelganger("speed_partial_rep",
                              autoplot(result, treatments = "treat"))
})

# Test: Large RCBD with 500 varieties
test_that("speed handles large RCBD with 500 treatments", {
  # Create data frame in a single call for large design
  dat_large <- data.frame(
    row = rep(1:50, times = 40),
    col = rep(1:40, each = 50),
    treat = rep(paste("V", 1:500, sep = ""), 4),
    block = rep(1:4, each = 500)
  )

  # Store original options and set test options
  old_options <- options()
  on.exit(options(old_options))
  options(speed.swap_count = 10)

  result <- speed(dat_large,
                  swap = "treat",
                  swap_within = "block",
                  spatial_factors = ~ row + col,
                  iterations = 1000,
                  early_stop_iterations = 400,
                  seed = 42,
                  quiet = TRUE)

  expect_s3_class(result, "design")
  expect_equal(nrow(result$design_df), 2000)
  expect_equal(ncol(result$design_df), 4)

  # Check that each block has exactly 500 treatments
  block_sizes <- table(result$design_df$block)
  expect_true(all(block_sizes == 500))

  # Check that each treatment appears exactly once per block
  block_treatment_counts <- table(result$design_df$block, result$design_df$treat)
  expect_true(all(block_treatment_counts %in% c(0, 1)))
  expect_true(all(colSums(block_treatment_counts) == 4))  # Each treatment in 4 blocks

  # Check row and column balance
  row_treatment_counts <- table(result$design_df$treat, result$design_df$row)
  col_treatment_counts <- table(result$design_df$treat, result$design_df$col)

  # Each treatment should appear in limited number of rows/columns
  row_appearances <- rowSums(row_treatment_counts > 0)
  col_appearances <- rowSums(col_treatment_counts > 0)
  expect_true(all(row_appearances <= 4))  # Max 4 rows per treatment
  expect_true(all(col_appearances <= 4))  # Max 4 columns per treatment

  vdiffr::expect_doppelganger("speed_large_rcbd",
                              autoplot(result, treatments = "treat", legend = FALSE))
})

test_that("speed runs a with variation of row and column columns", {
  # Sample data for testing
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
  vdiffr::expect_doppelganger("speed_small", autoplot(result))
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

# for nse
# test_that("speed runs within another function call", {
#   wrapper <- function() {
#     test_data <- data.frame(
#       row = rep(1:5, times = 4),
#       col = rep(1:4, each = 5),
#       treatment = rep(LETTERS[1:4], 5)
#     )
#
#     swap <- "treatment"
#     swap_within <- "1"
#
#     return(speed(
#       data = test_data,
#       swap = swap,
#       swap_within = swap_within,
#       spatial_factors = ~ row + col,
#       iterations = 100,
#       seed = 42,
#       quiet = TRUE
#     ))
#   }
#
#   result <- wrapper()
#   expect_s3_class(result, "design")
# })

# Test automatic seed generation in simple designs
test_that("speed generates seed automatically from .Random.seed[3] when seed=NULL for simple designs", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )

  # Run speed with seed=NULL (default)
  result <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 10,  # Very small number for quick test
    seed = NULL,  # Explicitly set to NULL
    quiet = TRUE
  )

  # Check that a seed was generated and stored
  expect_true(is.numeric(result$seed))
  expect_true(length(result$seed) == 1)
  expect_false(is.na(result$seed))

  # The seed should be within the range of possible .Random.seed[3] values
  # (This is a reasonable but not strict test since .Random.seed[3] can vary widely)
  expect_true(is.finite(result$seed))

  # Test that the generated seed produces reproducible results
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

  # Results should be identical when using the auto-generated seed
  expect_equal(result$design_df, result_with_generated_seed$design_df)
  expect_equal(result$seed, result_with_generated_seed$seed)
})

# Test automatic seed generation in hierarchical designs
test_that("speed generates seed automatically from .Random.seed[3] when seed=NULL for hierarchical designs", {
  # Hierarchical split-plot design
  df_split <- data.frame(
    row = rep(1:9, each = 3),
    col = rep(1:3, times = 9),
    wholeplot_treatment = rep(LETTERS[1:3], times = 9),
    subplot_treatment = rep(rep(letters[1:3], 3), each = 3),
    block = rep(1:3, each = 9)
  )

  # Run hierarchical speed with seed=NULL (default)
  result <- speed(df_split,
                  swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                  swap_within = list(wp = "block", sp = "wholeplot_treatment"),
                  spatial_factors = ~ row + col,
                  iterations = list(wp = 5, sp = 5),  # Very small numbers for quick test
                  seed = NULL,  # Explicitly set to NULL
                  quiet = TRUE)

  # Check that a seed was generated and stored
  expect_true(is.numeric(result$seed))
  expect_true(length(result$seed) == 1)
  expect_false(is.na(result$seed))

  # The seed should be within the range of possible .Random.seed[3] values
  expect_true(is.finite(result$seed))

  # Test that the generated seed produces reproducible results
  generated_seed <- result$seed

  result_with_generated_seed <- speed(df_split,
                                      swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                                      swap_within = list(wp = "block", sp = "wholeplot_treatment"),
                                      spatial_factors = ~ row + col,
                                      iterations = list(wp = 5, sp = 5),
                                      seed = generated_seed,
                                      quiet = TRUE)

  # Results should be identical when using the auto-generated seed
  expect_equal(result$design_df, result_with_generated_seed$design_df)
  expect_equal(result$seed, result_with_generated_seed$seed)

  # Check hierarchical structure is preserved
  expect_true(is.list(result$scores))
  expect_true(is.list(result$temperatures))
  expect_true(is.list(result$treatments))
})

# Test that different runs without seed produce different results (non-deterministic behavior)
test_that("speed produces different results when seed=NULL across different runs", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], each = 5)
  )

  # Run speed multiple times without setting seed
  result1 <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 50,
    seed = NULL,
    quiet = TRUE
  )

  # Advance the random number generator state
  set.seed(NULL)  # Reset to use current time
  dummy <- runif(10)  # Advance the state

  result2 <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 50,
    seed = NULL,
    quiet = TRUE
  )

  # Seeds should be different (with very high probability)
  expect_false(identical(result1$seed, result2$seed))

  # Results should be different (with very high probability due to different seeds)
  # Note: There's a tiny chance they could be the same by coincidence, but extremely unlikely
  expect_false(identical(result1$design_df$treatment, result2$design_df$treatment))
})

# Test progress output for simple designs
test_that("speed prints progress output when quiet=FALSE for simple designs", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )

  # Capture output with quiet=FALSE and enough iterations to trigger progress output
  expect_snapshot(
    result <- speed(
      data = test_data,
      swap = "treatment",
      swap_within = "1",
      spatial_factors = ~ row + col,
      iterations = 2000,  # Enough to trigger progress output at 1000
      seed = 42,
      quiet = FALSE
    )
  )

  # Check that progress messages are printed
  # expect_match(output, "Iteration: 1000")
  # expect_match(output, "Score:")
  # expect_match(output, "Best:")
  # expect_match(output, "Since Improvement:")

  # Check that the function still works correctly
  expect_s3_class(result, "design")
  expect_true(is.numeric(result$score))
})

# Test early stopping output for simple designs
test_that("speed prints early stopping message when quiet=FALSE for simple designs", {
  # Sample data that will likely converge quickly (already optimal)
  test_data <- data.frame(
    row = rep(1:3, times = 4),
    col = rep(1:4, each = 3),
    treatment = LETTERS[1:4]  # Already well-distributed
  )

  # Capture output with early stopping likely to occur
  expect_message(
    output <- capture_output(
      result <- speed(
        data = test_data,
        swap = "treatment",
        swap_within = "1",
        spatial_factors = ~ row + col,
        iterations = 1000,
        early_stop_iterations = 10,  # Low threshold for early stopping
        seed = 42,
        quiet = FALSE
      )
    ),
    "row and col are used as row and column, respectively"
  )

  # Check that early stopping message is printed
  expect_match(output, "Early stopping at iteration")

  # Verify that early stopping actually occurred
  expect_true(result$stopped_early)
  expect_lt(result$iterations_run, 1000)
})

# Test progress output for hierarchical designs
test_that("speed prints progress output when quiet=FALSE for hierarchical designs", {
  # Hierarchical split-plot design
  df_split <- data.frame(
    row = rep(1:6, each = 4),
    col = rep(1:4, times = 6),
    wholeplot_treatment = rep(LETTERS[1:3], each = 8),
    subplot_treatment = rep(letters[1:4], 6),
    block = rep(1:2, each = 12)
  )


  # Capture output with quiet=FALSE and enough iterations to trigger progress output
  expect_snapshot(
    result <- speed(df_split,
                    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                    swap_within = list(wp = "block", sp = "wholeplot_treatment"),
                    spatial_factors = ~ row + col,
                    iterations = list(wp = 1500, sp = 1500),  # Enough to trigger progress output
                    seed = 42,
                    quiet = FALSE)
  )

  # Check that hierarchical level messages are printed
  # expect_match(output, "Optimising level: wp")
  # expect_match(output, "Optimising level: sp")

  # Check that progress messages include level information
  # expect_match(output$stdout, "Level: wp")
  # expect_match(output$stdout, "Level: sp")
  # expect_match(output$stdout, "Iteration: 1000")
  # expect_match(output$stdout, "Score:")
  # expect_match(output$stdout, "Best:")
  # expect_match(output$stdout, "Since Improvement:")

  # Check that the function still works correctly
  expect_s3_class(result, "design")
  expect_true(is.list(result$scores))
  expect_true(is.list(result$treatments))
})

# Test early stopping output for hierarchical designs
test_that("speed prints early stopping messages when quiet=FALSE for hierarchical designs", {
  # Simple hierarchical design that will likely converge quickly
  df_split <- data.frame(
    row = rep(1:6, each = 4),
    col = rep(1:4, times = 6),
    wholeplot_treatment = rep(LETTERS[1:3], each = 8),
    subplot_treatment = rep(letters[1:4], 6),
    block = rep(1:2, each = 12)
  )

  # Capture output with early stopping likely to occur
  expect_message(
    output <- capture_output(
      result <- speed(df_split,
                      swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                      swap_within = list(wp = "1", sp = "wholeplot_treatment"),
                      spatial_factors = ~ row + col,
                      iterations = list(wp = 1000, sp = 1000),
                      early_stop_iterations = list(wp = 5, sp = 5),  # Low threshold
                      seed = 42,
                      quiet = FALSE)
    ),
    "row and col are used as row and column, respectively"
  )

  # Check that early stopping messages are printed with level information
  expect_match(output, "Early stopping at iteration .* for level wp")
  expect_match(output, "Early stopping at iteration .* for level sp")

  # Verify that early stopping actually occurred for at least one level
  expect_true(any(result$stopped_early))
})

# Test that quiet=TRUE suppresses all output
test_that("speed produces no output when quiet=TRUE", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )

  # Capture output with quiet=TRUE
  output <- capture_output(
    result <- speed(
      data = test_data,
      swap = "treatment",
      swap_within = "1",
      spatial_factors = ~ row + col,
      iterations = 2000,
      seed = 42,
      quiet = TRUE  # Should suppress all output
    )
  )

  # Check that no output is produced
  expect_equal(nchar(output), 0)
  expect_equal(nchar(output), 0)

  # Check that the function still works correctly
  expect_s3_class(result, "design")
  expect_true(is.numeric(result$score))
})

# Test that quiet=TRUE suppresses output in hierarchical designs
test_that("speed produces no output when quiet=TRUE for hierarchical designs", {
  # Hierarchical split-plot design
  df_split <- data.frame(
    row = rep(1:6, each = 4),
    col = rep(1:4, times = 6),
    wholeplot_treatment = rep(LETTERS[1:3], each = 8),
    subplot_treatment = rep(letters[1:4], 6),
    block = rep(1:2, each = 12)
  )

  # Capture output with quiet=TRUE
  output <- capture_output(
    result <- speed(df_split,
                    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                    swap_within = list(wp = "block", sp = "wholeplot_treatment"),
                    spatial_factors = ~ row + col,
                    iterations = list(wp = 1500, sp = 1500),
                    seed = 42,
                    quiet = TRUE)  # Should suppress all output
  )

  # Check that no output is produced
  expect_equal(nchar(output), 0)
  expect_equal(nchar(output), 0)

  # Check that the function still works correctly
  expect_s3_class(result, "design")
  expect_true(is.list(result$scores))
  expect_true(is.list(result$treatments))
})

# Test progress output frequency (every 1000 iterations)
test_that("speed prints progress output at correct intervals", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  # Capture output with enough iterations to trigger multiple progress outputs
  expect_message(
    output <- capture_output(
      result <- speed(
        data = test_data,
        swap = "treatment",
        swap_within = "1",
        spatial_factors = ~ row + col,
        iterations = 3500,  # Should trigger output at 1000, 2000, 3000
        seed = 42,
        quiet = FALSE
      )
    ),
    "row and col are used as row and column, respectively"
  )

  # Count occurrences of "Iteration:" to verify frequency
  iteration_matches <- regmatches(output, gregexpr("Iteration:", output))[[1]]

  # Should have progress output at iterations 1000, 2000, 3000 (3 total)
  # Plus potentially early stopping message, so at least 3
  expect_gte(length(iteration_matches), 2)

  # Check specific iteration numbers are present
  expect_match(output, "Iteration: 1000")
  expect_match(output, "Iteration: 2000")
})

# Test print.design method
test_that("print.design works for simple designs", {
  # Sample data for testing
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
    iterations = 100,
    seed = 42,
    quiet = TRUE
  )

  # Capture print output
  output <- capture_output(print(result))

  # Check that the printed output contains expected elements
  expect_match(output, "Optimised Experimental Design")
  expect_match(output, "Score:")
  expect_match(output, "Iterations Run:")
  expect_match(output, "Stopped Early:")
  expect_match(output, "Treatments:")
  expect_match(output, "Seed:")

  # Check specific values
  expect_match(output, paste("Score:", result$score))
  expect_match(output, paste("Iterations Run:", result$iterations_run))
  expect_match(output, paste("Stopped Early:", result$stopped_early))
  expect_match(output, paste("Seed:", result$seed))

  # Check treatments are displayed correctly for simple design
  expected_treatments <- paste(result$treatments, collapse = ", ")
  expect_match(output, paste("Treatments:", expected_treatments))

  # Verify invisible return
  expect_identical(print(result), result)
})

test_that("print.design works for hierarchical designs", {
  # Hierarchical split-plot design
  df_split <- data.frame(
    row = rep(1:6, each = 4),
    col = rep(1:4, times = 6),
    wholeplot_treatment = rep(LETTERS[1:3], each = 8),
    subplot_treatment = rep(letters[1:4], 6),
    block = rep(1:2, each = 12)
  )

  result <- speed(df_split,
                  swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                  swap_within = list(wp = "block", sp = "wholeplot_treatment"),
                  spatial_factors = ~ row + col,
                  iterations = list(wp = 50, sp = 50),
                  seed = 42,
                  quiet = TRUE)

  # Capture print output
  output <- capture_output(print.design(result))

  # Check that the printed output contains expected elements
  expect_match(output, "Optimised Experimental Design")
  expect_match(output, "Score:")
  expect_match(output, "Iterations Run:")
  expect_match(output, "Stopped Early:")
  expect_match(output, "Treatments:")
  expect_match(output, "Seed:")

  # Check hierarchical-specific formatting
  expect_match(output, "wp:")  # Level name should be shown
  expect_match(output, "sp:")  # Level name should be shown

  # Check that treatments for each level are displayed
  for (level_name in names(result$treatments)) {
    expected_treatments <- paste(result$treatments[[level_name]], collapse = ", ")
    expect_match(output, paste0(level_name, ": ", expected_treatments))
  }

  # Verify stopped_early is shown correctly for hierarchical (should show both levels)
  expect_match(output, "Stopped Early:")

  # Verify invisible return
  expect_identical(print(result), result)
})

test_that("print.design handles different stopped_early formats", {
  # Test with simple design where stopped_early is logical
  test_data <- data.frame(
    row = rep(1:3, times = 3),
    col = rep(1:3, each = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result_simple <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 50,
    seed = 42,
    quiet = TRUE
  )

  output_simple <- capture_output(print(result_simple))
  expect_match(output_simple, "Stopped Early: (TRUE|FALSE)")

  # Test with hierarchical design where stopped_early is named logical vector
  df_split <- data.frame(
    row = rep(1:4, each = 3),
    col = rep(1:3, times = 4),
    wholeplot_treatment = rep(LETTERS[1:2], each = 6),
    subplot_treatment = rep(letters[1:3], 4),
    block = rep(1:1, each = 12)
  )

  result_hierarchical <- speed(df_split,
                              swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                              swap_within = list(wp = "block", sp = "wholeplot_treatment"),
                              spatial_factors = ~ row + col,
                              iterations = list(wp = 30, sp = 30),
                              seed = 42,
                              quiet = TRUE)

  output_hierarchical <- capture_output(print(result_hierarchical))
  expect_match(output_hierarchical, "Stopped Early:")
})

test_that("print.design displays correct treatment counts and names", {
  # Test with different numbers of treatments
  test_data_few <- data.frame(
    row = rep(1:4, times = 2),
    col = rep(1:2, each = 4),
    treatment = rep(LETTERS[1:2], 4)
  )

  test_data_many <- data.frame(
    row = rep(1:5, times = 6),
    col = rep(1:6, each = 5),
    treatment = rep(LETTERS[1:6], 5)
  )

  result_few <- speed(
    data = test_data_few,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 50,
    seed = 42,
    quiet = TRUE
  )

  result_many <- speed(
    data = test_data_many,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 50,
    seed = 42,
    quiet = TRUE
  )

  # Test few treatments
  output_few <- capture_output(print(result_few))
  expect_match(output_few, "A, B")  # Should show both treatments

  # Test many treatments
  output_many <- capture_output(print(result_many))
  expect_match(output_many, "A, B, C, D, E, F")  # Should show all treatments
})

test_that("print.design works with extra arguments via ...", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:3, times = 3),
    col = rep(1:3, each = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 50,
    seed = 42,
    quiet = TRUE
  )

  # Should not error with extra arguments (though they're not used)
  expect_no_error({
    print(result, extra_param = "test")
  })

  # Output should still be correct
  output <- capture_output(print(result, unused_param = 123))
  expect_match(output, "Optimised Experimental Design")
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

