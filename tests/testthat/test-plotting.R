
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
