# What a design prints and plots: `autoplot()`, progress/quiet messaging
# and `print.design()`.

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
    autoplot(result)
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

  vdiffr::expect_doppelganger(
    "autoplot_factor_custom_names",
    autoplot(result, row = Row, column = Column, treatments = trt)
  )
})

test_that("speed reports non-numeric row/col labels clearly", {
  # Row/col labels like "R1"/"C1" cannot index a grid. This used to surface as
  # five coercion warnings followed by a cryptic "invalid 'nrow' value (too
  # large or NA)" from matrix(); build_design_matrix() now names the actual
  # problem once, with no warnings.
  test_data <- data.frame(
    row = factor(rep(paste0("R", 1:5), times = 4)),
    col = factor(rep(paste0("C", 1:4), each = 5)),
    treatment = rep(LETTERS[1:4], 5)
  )

  expect_no_warning(
    expect_error(
      speed(
        data = test_data,
        swap = "treatment",
        iterations = 100,
        seed = 42,
        quiet = TRUE
      ),
      "must be numeric, or coercible to numeric"
    )
  )
})

test_that("autoplot handles mixed factor and numeric columns in hierarchical designs", {
  # Hierarchical design with mixed factor/numeric columns
  df_split <- data.frame(
    row = factor(rep(1:12, times = 6)),
    col = rep(1:6, each = 12), # Keep col as numeric
    block = rep(1:6, each = 12),
    wholeplot_treatment = rep(
      rep(paste0("WP", LETTERS[1:3]), each = 4),
      times = 6
    ),
    subplot_treatment = rep(paste0("SP", letters[1:4]), times = 18)
  )

  result <- speed(
    df_split,
    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
    swap_within = list(wp = "block", sp = "wholeplot_treatment"),
    spatial_factors = ~ row + col,
    iterations = 100,
    seed = 42,
    swap_all = TRUE,
    quiet = TRUE
  )

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
  vdiffr::expect_doppelganger(
    "autoplot_no_legend",
    autoplot(result, legend = FALSE)
  )
  vdiffr::expect_doppelganger(
    "autoplot_with_legend",
    autoplot(result, legend = TRUE)
  )
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

  result <- speed(
    df_split,
    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
    swap_within = list(wp = "block", sp = "wholeplot"),
    iterations = list(wp = 50, sp = 50),
    swap_all = TRUE,
    seed = 42,
    quiet = TRUE
  )

  # Test with wholeplot treatments
  plot_wp_no_legend <- autoplot(
    result,
    treatments = "wholeplot_treatment",
    legend = FALSE
  )
  plot_wp_with_legend <- autoplot(
    result,
    treatments = "wholeplot_treatment",
    legend = TRUE
  )

  expect_contains(class(plot_wp_no_legend), "ggplot")
  expect_contains(class(plot_wp_with_legend), "ggplot")

  # Check legend visibility
  expect_equal(plot_wp_no_legend$theme$legend.position, "none")
  expect_equal(plot_wp_with_legend$theme$legend.position, "right")

  # Test with subplot treatments
  plot_sp_no_legend <- autoplot(
    result,
    treatments = "subplot_treatment",
    legend = FALSE
  )
  plot_sp_with_legend <- autoplot(
    result,
    treatments = "subplot_treatment",
    legend = TRUE
  )

  expect_contains(class(plot_sp_no_legend), "ggplot")
  expect_contains(class(plot_sp_with_legend), "ggplot")

  # Check legend visibility
  expect_equal(plot_sp_no_legend$theme$legend.position, "none")
  expect_equal(plot_sp_with_legend$theme$legend.position, "right")

  # Test visual regression
  vdiffr::expect_doppelganger(
    "autoplot_hierarchical_no_legend",
    plot_wp_no_legend
  )
  vdiffr::expect_doppelganger(
    "autoplot_hierarchical_with_legend",
    plot_wp_with_legend
  )
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
      iterations = 2000, # Enough to trigger progress output at 1000
      optimise_params = optim_params(stop_at_optimal = FALSE),
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
    treatment = LETTERS[1:4] # Already well-distributed
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
        early_stop_iterations = 10, # Low threshold for early stopping
        optimise_params = optim_params(stop_at_optimal = FALSE),
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
    result <- speed(
      df_split,
      swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
      swap_within = list(wp = "block", sp = "wholeplot_treatment"),
      spatial_factors = ~ row + col,
      iterations = list(wp = 1500, sp = 1500), # Enough to trigger progress output
      optimise_params = optim_params(stop_at_optimal = FALSE),
      seed = 42,
      quiet = FALSE
    )
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
      result <- speed(
        df_split,
        swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
        swap_within = list(wp = "1", sp = "wholeplot_treatment"),
        spatial_factors = ~ row + col,
        iterations = list(wp = 1000, sp = 1000),
        early_stop_iterations = list(wp = 5, sp = 5), # Low threshold
        seed = 42,
        quiet = FALSE
      )
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
      quiet = TRUE # Should suppress all output
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
    result <- speed(
      df_split,
      swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
      swap_within = list(wp = "block", sp = "wholeplot_treatment"),
      spatial_factors = ~ row + col,
      iterations = list(wp = 1500, sp = 1500),
      seed = 42,
      quiet = TRUE
    ) # Should suppress all output
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
        iterations = 3500, # Should trigger output at 1000, 2000, 3000
        optimise_params = optim_params(stop_at_optimal = FALSE),
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

  result <- speed(
    df_split,
    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
    swap_within = list(wp = "block", sp = "wholeplot_treatment"),
    spatial_factors = ~ row + col,
    iterations = list(wp = 50, sp = 50),
    seed = 42,
    quiet = TRUE
  )

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
  expect_match(output, "wp:") # Level name should be shown
  expect_match(output, "sp:") # Level name should be shown

  # Check that treatments for each level are displayed
  for (level_name in names(result$treatments)) {
    expected_treatments <- paste(
      result$treatments[[level_name]],
      collapse = ", "
    )
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

  result_hierarchical <- speed(
    df_split,
    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
    swap_within = list(wp = "block", sp = "wholeplot_treatment"),
    spatial_factors = ~ row + col,
    iterations = list(wp = 30, sp = 30),
    optimise_params = list(wp = optim_params(adj_weight = 0)),
    seed = 42,
    quiet = TRUE
  )

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
    optimise_params = optim_params(adj_weight = 0),
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
  expect_match(output_few, "A, B") # Should show both treatments

  # Test many treatments
  output_many <- capture_output(print(result_many))
  expect_match(output_many, "A, B, C, D, E, F") # Should show all treatments
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
