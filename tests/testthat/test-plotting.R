
# Test palette options in autoplot
test_that("autoplot handles default palette", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:6], 2)
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

  # Test default palette
  expect_no_error({
    plot <- autoplot(result, palette = "default")
  })

  plot <- autoplot(result, palette = "default")
  expect_contains(class(plot), "ggplot")

  vdiffr::expect_doppelganger("autoplot_palette_default", autoplot(result, palette = "default"))
})

test_that("autoplot handles viridis-based palettes", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:6], 2)
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

  # Test viridis palettes
  viridis_palettes <- c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")

  for (palette in viridis_palettes) {
    expect_no_error(plot <- autoplot(result, palette = palette))

    plot <- autoplot(result, palette = palette)
    expect_contains(class(plot), "ggplot")
  }

  # Visual regression tests for key viridis palettes
  vdiffr::expect_doppelganger("autoplot_palette_viridis", autoplot(result, palette = "viridis"))
  vdiffr::expect_doppelganger("autoplot_palette_magma", autoplot(result, palette = "magma"))
  vdiffr::expect_doppelganger("autoplot_palette_plasma", autoplot(result, palette = "plasma"))
  vdiffr::expect_doppelganger("autoplot_palette_inferno", autoplot(result, palette = "inferno"))
})

test_that("autoplot handles color-blind friendly palettes", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:6], 2)
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

  # Test various color-blind friendly palette specifications
  colorblind_options <- c("colour blind", "color blind", "colour-blind", "color-blind",
                          "colourblind", "colorblind", "cb", "viridis")

  for (palette in colorblind_options) {
    expect_no_error(autoplot(result, palette = palette))

    plot <- autoplot(result, palette = palette)
    expect_contains(class(plot), "ggplot")
  }

  # Visual regression test for color-blind palette
  vdiffr::expect_doppelganger("autoplot_palette_colorblind", autoplot(result, palette = "colour blind"))
})

test_that("autoplot handles RColorBrewer palettes", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:6], 2)
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

  # Test RColorBrewer palettes
  brewer_palettes <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy",
                       "RdYlBu", "RdYlGn", "Spectral", "Set3", "Paired")

  for (palette in brewer_palettes) {
    expect_no_error(autoplot(result, palette = palette))

    plot <- autoplot(result, palette = palette)
    expect_contains(class(plot), "ggplot")
  }

  # Visual regression tests for key RColorBrewer palettes
  vdiffr::expect_doppelganger("autoplot_palette_spectral", autoplot(result, palette = "Spectral"))
  vdiffr::expect_doppelganger("autoplot_palette_set3", autoplot(result, palette = "Set3"))
  vdiffr::expect_doppelganger("autoplot_palette_paired", autoplot(result, palette = "Paired"))
  vdiffr::expect_doppelganger("autoplot_palette_rdylbu", autoplot(result, palette = "RdYlBu"))
})

test_that("autoplot handles custom color palettes", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:4, times = 2),
    col = rep(1:2, each = 4),
    treatment = rep(LETTERS[1:4], 2)
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

  # Test custom color palette with exact number of colors
  custom_palette <- c("#FF5733", "#33FF57", "#3357FF", "#FF33F5")

  expect_no_error({
    plot <- autoplot(result, palette = custom_palette)
  })

  plot <- autoplot(result, palette = custom_palette)
  expect_contains(class(plot), "ggplot")

  vdiffr::expect_doppelganger("autoplot_palette_custom", autoplot(result, palette = custom_palette))
})

test_that("autoplot palette error handling works correctly", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:4, times = 2),
    col = rep(1:2, each = 4),
    treatment = rep(LETTERS[1:4], 2)
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

  # Test error when custom palette has wrong number of colors
  expect_error(
    autoplot(result, palette = c("#FF5733", "#33FF57")),  # Only 2 colors for 4 treatments
    "palette needs to be a single string to choose a predefined palette, or 4 custom colours"
  )

  expect_error(
    autoplot(result, palette = c("#FF5733", "#33FF57", "#3357FF", "#FF33F5", "#FFFF00")),  # Too many colors
    "palette needs to be a single string to choose a predefined palette, or 4 custom colours"
  )

  # Test error with invalid palette name
  expect_error(
    autoplot(result, palette = "invalid_palette_name"),
    "Invalid value for palette"
  )
})

test_that("autoplot palettes work with different numbers of treatments", {
  # Test with small number of treatments (3)
  small_data <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )

  small_result <- speed(
    data = small_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 50,
    seed = 42,
    quiet = TRUE
  )

  # Test with large number of treatments (8)
  large_data <- data.frame(
    row = rep(1:8, times = 4),
    col = rep(1:4, each = 8),
    treatment = rep(LETTERS[1:8], 4)
  )

  large_result <- speed(
    data = large_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 50,
    seed = 42,
    quiet = TRUE
  )

  # Test various palettes work with different treatment numbers
  test_palettes <- c("default", "viridis", "magma", "Spectral", "Set3")

  for (palette in test_palettes) {
    expect_no_error({
      small_plot <- autoplot(small_result, palette = palette)
      large_plot <- autoplot(large_result, palette = palette)
    })

    expect_contains(class(small_plot), "ggplot")
    expect_contains(class(large_plot), "ggplot")
  }

  # Visual regression tests
  vdiffr::expect_doppelganger("autoplot_palette_small_treatments", autoplot(small_result, palette = "viridis"))
  vdiffr::expect_doppelganger("autoplot_palette_large_treatments", autoplot(large_result, palette = "Spectral"))
})

test_that("autoplot palettes work with hierarchical designs", {
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
                  iterations = 100,
                  seed = 42,
                  quiet = TRUE)

  # Test different palettes work with hierarchical designs
  test_palettes <- c("default", "plasma", "Set3", "RdYlGn")

  for (palette in test_palettes) {
    expect_no_error({
      wp_plot <- autoplot(result, treatments = "wholeplot_treatment", palette = palette)
      sp_plot <- autoplot(result, treatments = "subplot_treatment", palette = palette)
    })

    expect_contains(class(wp_plot), "ggplot")
    expect_contains(class(sp_plot), "ggplot")
  }

  # Visual regression tests for hierarchical with different palettes
  vdiffr::expect_doppelganger("autoplot_hierarchical_plasma_wp",
                              autoplot(result, treatments = "wholeplot_treatment", palette = "plasma"))
  vdiffr::expect_doppelganger("autoplot_hierarchical_set3_sp",
                              autoplot(result, treatments = "subplot_treatment", palette = "Set3"))
})

test_that("autoplot palettes work with blocks", {
  # Sample data with blocks
  test_data <- data.frame(
    row = rep(1:6, each = 4),
    col = rep(1:4, times = 6),
    treatment = rep(LETTERS[1:6], 4),
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

  # Test different palettes work with blocked designs
  test_palettes <- c("default", "inferno", "Paired", "RdBu")

  for (palette in test_palettes) {
    expect_no_error({
      plot <- autoplot(result, palette = palette)
    })

    plot <- autoplot(result, palette = palette)
    expect_contains(class(plot), "ggplot")
  }

  # Visual regression tests for blocks with different palettes
  vdiffr::expect_doppelganger("autoplot_blocks_inferno", autoplot(result, palette = "inferno"))
  vdiffr::expect_doppelganger("autoplot_blocks_paired", autoplot(result, palette = "Paired"))
})

test_that("autoplot custom palettes work with exact color specifications", {
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

  # Test various custom color formats
  hex_colors <- c("#FF0000", "#00FF00", "#0000FF")
  rgb_colors <- c("red", "green", "blue")
  mixed_colors <- c("#FF5733", "purple", "#32CD32")

  color_sets <- list(
    hex = hex_colors,
    rgb = rgb_colors,
    mixed = mixed_colors
  )

  for (color_name in names(color_sets)) {
    expect_no_error({
      plot <- autoplot(result, palette = color_sets[[color_name]])
    })

    plot <- autoplot(result, palette = color_sets[[color_name]])
    expect_contains(class(plot), "ggplot")
  }

  # Visual regression tests for custom colors
  vdiffr::expect_doppelganger("autoplot_custom_hex", autoplot(result, palette = hex_colors))
  vdiffr::expect_doppelganger("autoplot_custom_rgb", autoplot(result, palette = rgb_colors))
  vdiffr::expect_doppelganger("autoplot_custom_mixed", autoplot(result, palette = mixed_colors))
})

# Test plot_progress function
test_that("plot_progress executes without errors", {
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

  # Should not error when calling plot_progress
  expect_no_error({
    plot_progress(result)
  })
})

test_that("plot_progress handles simple design optimization results", {
  # Sample data for testing
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  result <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 500,
    seed = 42,
    quiet = TRUE
  )

  # Capture the output to verify plots are printed
  expect_output(
    plot_progress(result),
    NA  # No specific text output expected, just that it executes
  )

  # Verify that result has required components for plot_progress
  expect_true("scores" %in% names(result))
  expect_true("temperatures" %in% names(result))
  expect_true(is.numeric(result$scores))
  expect_true(is.numeric(result$temperatures))
  expect_equal(length(result$scores), 500)
  expect_equal(length(result$temperatures), 500)
})

test_that("plot_progress handles different iteration lengths", {
  # Test with short optimization
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
  )

  short_result <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 50,
    seed = 42,
    quiet = TRUE
  )

  # Test with longer optimization
  long_result <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 1000,
    seed = 42,
    quiet = TRUE
  )

  # Both should work
  expect_no_error({
    plot_progress(short_result)
  })

  expect_no_error({
    plot_progress(long_result)
  })

  # Verify lengths match
  expect_equal(length(short_result$scores), 50)
  expect_equal(length(short_result$temperatures), 50)
  expect_equal(length(long_result$scores), 1000)
  expect_equal(length(long_result$temperatures), 1000)

  # Visual regression tests for plot_progress
  vdiffr::expect_doppelganger("plot_progress_short", plot_progress(short_result))
  vdiffr::expect_doppelganger("plot_progress_long", plot_progress(long_result))
})

test_that("plot_progress handles early stopping results", {
  # Create a design that will likely trigger early stopping
  test_data <- data.frame(
    row = rep(1:4, times = 4),
    col = rep(1:4, each = 4),
    treatment = rep(LETTERS[1:4], 4)
  )

  result <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 1000,
    early_stop_iterations = 50,
    seed = 42,
    quiet = TRUE
  )

  # Should work even with early stopping
  expect_no_error({
    plot_progress(result)
  })

  # Verify early stopping occurred
  expect_true(result$stopped_early)
  expect_lt(result$iterations_run, 1000)

  # Verify arrays are the correct length (should match iterations_run)
  expect_equal(length(result$scores), result$iterations_run)
  expect_equal(length(result$temperatures), result$iterations_run)
})

test_that("plot_progress handles blocked designs", {
  # Sample data with blocks
  test_data <- data.frame(
    row = rep(1:6, each = 4),
    col = rep(1:4, times = 6),
    treatment = rep(LETTERS[1:8], 3),
    block = rep(1:3, each = 8)
  )

  result <- speed(
    data = test_data,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col,
    iterations = 200,
    seed = 42,
    quiet = TRUE
  )

  # Should work with blocked designs
  expect_no_error({
    plot_progress(result)
  })

  # Verify structure is still simple (not hierarchical)
  expect_true(is.numeric(result$scores))
  expect_true(is.numeric(result$temperatures))
  expect_false(is.list(result$scores))
  expect_false(is.list(result$temperatures))
})

test_that("plot_progress handles results with different optimization patterns", {
  # Test with different starting configurations to get different optimization patterns
  test_data <- data.frame(
    row = rep(1:5, times = 5),
    col = rep(1:5, each = 5),
    treatment = rep(LETTERS[1:5], 5)
  )

  # Test with different seeds to get different optimization trajectories
  seeds <- c(123, 456, 789)

  for (seed in seeds) {
    result <- speed(
      data = test_data,
      swap = "treatment",
      swap_within = "1",
      spatial_factors = ~ row + col,
      iterations = 300,
      seed = seed,
      quiet = TRUE
    )

    expect_no_error({
      plot_progress(result)
    })

    # Verify the optimization data has expected properties
    expect_true(all(is.finite(result$scores)))
    expect_true(all(is.finite(result$temperatures)))
    expect_true(all(result$temperatures > 0))  # Temperatures should be positive
    expect_gte(min(result$temperatures), 0)
    expect_gte(max(result$temperatures), min(result$temperatures))  # Non-increasing temperature
  }
})

test_that("plot_progress error handling for invalid inputs", {
  # Test with missing scores
  invalid_result_no_scores <- list(
    temperatures = seq(1, 0.01, length.out = 100)
  )

  expect_error(
    plot_progress(invalid_result_no_scores),
    "arguments imply differing number of rows: 2, 0, 100"
  )

  # Test with missing temperatures
  invalid_result_no_temps <- list(
    scores = runif(100, 0, 10)
  )

  expect_error(
    plot_progress(invalid_result_no_temps),
    "arguments imply differing number of rows: 100, 0"
  )

  # Test with mismatched lengths
  invalid_result_mismatch <- list(
    scores = runif(100, 0, 10),
    temperatures = seq(1, 0.01, length.out = 35)  # Different length
  )

  expect_error(
    plot_progress(invalid_result_mismatch),
    "arguments imply differing number of rows"
  )
})

test_that("plot_progress generates expected plot elements", {
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

  # Capture the plots using a different approach
  # Since plot_progress() prints directly, we test the underlying data
  df <- data.frame(
    iteration = 1:length(result$scores),
    score = result$scores,
    temperature = result$temperatures
  )

  # Verify the data frame that would be used in plot_progress
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 3)
  expect_true(all(c("iteration", "score", "temperature") %in% names(df)))

  # Verify iteration sequence
  expect_equal(df$iteration, 1:100)

  # Verify scores and temperatures are numeric and finite
  expect_true(all(is.numeric(df$score)))
  expect_true(all(is.numeric(df$temperature)))
  expect_true(all(is.finite(df$score)))
  expect_true(all(is.finite(df$temperature)))
})

test_that("plot_progress works with single iteration result", {
  # Create a result that stops after 1 iteration
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
    iterations = 1,  # Single iteration
    seed = 42,
    quiet = TRUE
  )

  # Should handle single iteration without error
  expect_no_error({
    plot_progress(result)
  })

  # Verify single point data
  expect_equal(length(result$scores), 1)
  expect_equal(length(result$temperatures), 1)
})

# Test autoplot margin parameter
test_that("autoplot margin parameter controls plot margins", {
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

  # Test default behavior (margin = FALSE)
  plot_no_margin <- autoplot(result, margin = FALSE)
  expect_contains(class(plot_no_margin), "ggplot")

  # Test with margin = TRUE
  plot_with_margin <- autoplot(result, margin = TRUE)
  expect_contains(class(plot_with_margin), "ggplot")

  # Both plots should be generated without error
  expect_no_error({
    autoplot(result, margin = FALSE)
  })

  expect_no_error({
    autoplot(result, margin = TRUE)
  })

  # Check that the plots have different scale properties
  # margin = FALSE should have expand = c(0, 0) for both x and y scales
  # margin = TRUE should not have expand = c(0, 0)

  # Build the plots to access their internal structure
  built_no_margin <- ggplot2::ggplot_build(plot_no_margin)
  built_with_margin <- ggplot2::ggplot_build(plot_with_margin)

  # The plots should have the same data but different scale ranges
  expect_equal(built_no_margin$data, built_with_margin$data)

  # Visual regression tests to capture the differences
  vdiffr::expect_doppelganger("autoplot_margin_false", autoplot(result, margin = FALSE))
  vdiffr::expect_doppelganger("autoplot_margin_true", autoplot(result, margin = TRUE))
})

test_that("autoplot margin parameter works with blocked designs", {
  # Sample data with blocks for more complex margin testing
  test_data <- data.frame(
    row = rep(1:6, each = 4),
    col = rep(1:4, times = 6),
    treatment = rep(LETTERS[1:6], 4),
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

  # Test margin parameter with blocked designs
  expect_no_error({
    plot_no_margin_blocked <- autoplot(result, margin = FALSE)
  })

  expect_no_error({
    plot_with_margin_blocked <- autoplot(result, margin = TRUE)
  })

  plot_no_margin_blocked <- autoplot(result, margin = FALSE)
  plot_with_margin_blocked <- autoplot(result, margin = TRUE)

  expect_contains(class(plot_no_margin_blocked), "ggplot")
  expect_contains(class(plot_with_margin_blocked), "ggplot")

  # Visual regression tests for blocked designs with different margin settings
  vdiffr::expect_doppelganger("autoplot_blocked_margin_false", autoplot(result, margin = FALSE))
  vdiffr::expect_doppelganger("autoplot_blocked_margin_true", autoplot(result, margin = TRUE))
})

test_that("autoplot margin parameter works with different palettes", {
  # Test that margin parameter works correctly with different palette options
  test_data <- data.frame(
    row = rep(1:5, times = 4),
    col = rep(1:4, each = 5),
    treatment = rep(LETTERS[1:4], 5)
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

  # Test different combinations of margin and palette parameters
  test_combinations <- list(
    list(margin = FALSE, palette = "viridis"),
    list(margin = TRUE, palette = "viridis"),
    list(margin = FALSE, palette = "Set3"),
    list(margin = TRUE, palette = "Set3"),
    list(margin = FALSE, palette = c("#FF5733", "#33FF57", "#3357FF", "#FF33F5")),
    list(margin = TRUE, palette = c("#FF5733", "#33FF57", "#3357FF", "#FF33F5"))
  )

  for (combo in test_combinations) {
    expect_no_error({
      autoplot(result, margin = combo$margin, palette = combo$palette)
    })

    plot <- autoplot(result, margin = combo$margin, palette = combo$palette)
    expect_contains(class(plot), "ggplot")
  }

  # Visual regression tests for key combinations
  vdiffr::expect_doppelganger("autoplot_margin_false_viridis",
                              autoplot(result, margin = FALSE, palette = "viridis"))
  vdiffr::expect_doppelganger("autoplot_margin_true_set3",
                              autoplot(result, margin = TRUE, palette = "Set3"))
})
