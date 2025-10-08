test_that("create_buffers adds edge buffers correctly", {
  # Create a simple 3x3 design
  design <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result <- create_buffers(design, type = "edge", blocks = FALSE)

  # Check that buffers were added
  expect_true("buffer" %in% result$treatment)

  # Original design should be shifted by 1
  non_buffers <- result[result$treatment != "buffer", ]
  expect_equal(min(non_buffers$row), 2)
  expect_equal(max(non_buffers$row), 4)
  expect_equal(min(non_buffers$col), 2)
  expect_equal(max(non_buffers$col), 4)

  # Check number of buffer plots
  buffers <- result[result$treatment == "buffer", ]
  # Edge buffers: top row (5), bottom row (5), left column (3), right column (3) = 16
  expect_equal(nrow(buffers), 16)

  # Check that all edge positions have buffers
  expect_true(all(result[result$row == 1, "treatment"] == "buffer"))
  expect_true(all(result[result$row == 5, "treatment"] == "buffer"))
  expect_true(all(result[result$col == 1, "treatment"] == "buffer"))
  expect_true(all(result[result$col == 5, "treatment"] == "buffer"))
})

test_that("create_buffers adds edge buffers with alternative syntax", {
  design <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, times = 2),
    treatment = rep(c("A", "B"), 2)
  )

  # Test "edges" syntax
  result1 <- create_buffers(design, type = "edges", blocks = FALSE)
  expect_true("buffer" %in% result1$treatment)

  # Test "e" syntax
  result2 <- create_buffers(design, type = "e", blocks = FALSE)
  expect_true("buffer" %in% result2$treatment)

  # Test "EDGE" (case insensitive)
  result3 <- create_buffers(design, type = "EDGE", blocks = FALSE)
  expect_true("buffer" %in% result3$treatment)

  # All should produce the same result
  expect_equal(nrow(result1), nrow(result2))
  expect_equal(nrow(result2), nrow(result3))
})

test_that("create_buffers adds row buffers correctly", {
  design <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result <- create_buffers(design, type = "row", blocks = FALSE)

  # Check that buffers were added
  expect_true("buffer" %in% result$treatment)

  # Original design rows should be at even positions
  non_buffers <- result[result$treatment != "buffer", ]
  expect_true(all(non_buffers$row %% 2 == 0))

  # Buffers should be at odd positions
  buffers <- result[result$treatment == "buffer", ]
  expect_true(all(buffers$row %% 2 == 1))

  # Should have 4 buffer rows (before row 1, between rows, after row 3) * 3 cols = 12
  expect_equal(nrow(buffers), 12)
})

test_that("create_buffers adds column buffers correctly", {
  design <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result <- create_buffers(design, type = "column", blocks = FALSE)

  # Check that buffers were added
  expect_true("buffer" %in% result$treatment)

  # Original design cols should be at even positions
  non_buffers <- result[result$treatment != "buffer", ]
  expect_true(all(non_buffers$col %% 2 == 0))

  # Buffers should be at odd positions
  buffers <- result[result$treatment == "buffer", ]
  expect_true(all(buffers$col %% 2 == 1))

  # Should have 4 buffer columns * 3 rows = 12
  expect_equal(nrow(buffers), 12)
})

test_that("create_buffers throws error for block buffers", {
  design <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, times = 2),
    treatment = rep(c("A", "B"), 2)
  )

  expect_error(
    create_buffers(design, type = "block", blocks = FALSE),
    "Block buffers are not yet supported"
  )
})

test_that("create_buffers throws error for invalid buffer type", {
  design <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, times = 2),
    treatment = rep(c("A", "B"), 2)
  )

  expect_error(
    create_buffers(design, type = "invalid", blocks = FALSE),
    "Invalid buffer option: invalid"
  )
})

# Integration tests with speed() function
test_that("create_buffers works with speed optimized designs", {
  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  # Run speed optimization with few iterations
  result <- speed(df, swap = "treatment", iterations = 100, quiet = TRUE, seed = 42)

  # Add edge buffers to optimized design
  buffered_design <- add_buffers(result, type = "edge")

  # Check that buffers were added
  expect_true(inherits(buffered_design, "design"))
  expect_true("buffer" %in% buffered_design$design$treatment)
  expect_gt(nrow(buffered_design$design), nrow(result$design_df))

  # Check that original treatments are preserved
  original_treatments <- sort(unique(result$design_df$treatment))
  buffered_treatments <- sort(unique(buffered_design$design$treatment[buffered_design$design$treatment != "buffer"]))
  expect_equal(original_treatments, buffered_treatments)
})

test_that("add_buffers preserves optimization results", {
  df <- data.frame(
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4),
    treatment = rep(LETTERS[1:4], 2)
  )

  # Run speed optimization
  result <- speed(df, swap = "treatment", iterations = 50, quiet = TRUE, seed = 123)

  # Store original score and seed
  original_score <- result$score
  original_seed <- result$seed

  # Add buffers
  buffered_result <- add_buffers(result, type = "row")

  # Check that metadata is preserved
  expect_equal(buffered_result$score, original_score)
  expect_equal(buffered_result$seed, original_seed)
  expect_equal(buffered_result$iterations_run, result$iterations_run)
})

test_that("autoplot renders speed optimized design with edge buffers", {
  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  # Run speed optimization
  result <- speed(df, swap = "treatment", iterations = 100, quiet = TRUE, seed = 42)

  # Add edge buffers
  buffered_result <- add_buffers(result, type = "edge")

  # Create plot
  p <- autoplot(buffered_result)

  # Verify it's a ggplot object
  expect_contains(class(p), "ggplot")

  # Visual regression test
  vdiffr::expect_doppelganger("speed-optimized-edge-buffers", p)
})

test_that("autoplot renders speed optimized design with row buffers", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result <- speed(df, swap = "treatment", iterations = 100, quiet = TRUE, seed = 42)
  buffered_result <- add_buffers(result, type = "row")

  p <- autoplot(buffered_result)
  expect_s3_class(p, "ggplot")

  vdiffr::expect_doppelganger("speed-optimized-row-buffers", p)
})

test_that("autoplot renders speed optimized design with column buffers", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result <- speed(df, swap = "treatment", iterations = 100, quiet = TRUE, seed = 42)
  buffered_result <- add_buffers(result, type = "column")

  p <- autoplot(buffered_result)
  expect_s3_class(p, "ggplot")

  vdiffr::expect_doppelganger("speed-optimized-column-buffers", p)
})

test_that("autoplot renders blocked design with buffers", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(
    row = rep(1:6, each = 2),
    col = rep(1:2, times = 6),
    treatment = rep(LETTERS[1:4], 3),
    block = rep(1:3, each = 4)
  )

  result <- speed(df, swap = "treatment", swap_within = "block",
                  iterations = 100, quiet = TRUE, seed = 42)

  buffered_result <- add_buffers(result, type = "edge")

  p <- autoplot(buffered_result)
  expect_s3_class(p, "ggplot")

  vdiffr::expect_doppelganger("speed-optimized-blocked-buffers", p)
})

test_that("buffers work with different palette options", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result <- speed(df, swap = "treatment", iterations = 50, quiet = TRUE, seed = 42)
  buffered_result <- add_buffers(result, type = "edge")

  # Test with colour blind palette
  p_cb <- autoplot(buffered_result, palette = "colour blind")
  expect_s3_class(p_cb, "ggplot")

  vdiffr::expect_doppelganger("buffers-colourblind-palette", p_cb)

  # Test with plasma palette
  p_plasma <- autoplot(buffered_result, palette = "plasma")
  expect_s3_class(p_plasma, "ggplot")

  vdiffr::expect_doppelganger("buffers-plasma-palette", p_plasma)
})

test_that("buffers render correctly with margin parameter", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result <- speed(df, swap = "treatment", iterations = 50, quiet = TRUE, seed = 42)
  buffered_result <- add_buffers(result, type = "edge")

  # Without margin
  p_no_margin <- autoplot(buffered_result, margin = FALSE)
  expect_s3_class(p_no_margin, "ggplot")

  # With margin
  p_with_margin <- autoplot(buffered_result, margin = TRUE)
  expect_s3_class(p_with_margin, "ggplot")

  vdiffr::expect_doppelganger("buffers-no-margin", p_no_margin)
  vdiffr::expect_doppelganger("buffers-with-margin", p_with_margin)
})

test_that("create_buffers maintains treatment positions correctly", {
  df <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, times = 2),
    treatment = c("A", "B", "C", "D")
  )

  result <- speed(df, swap = "treatment", iterations = 50, quiet = TRUE, seed = 42)

  # Add edge buffers
  result_buffered <- add_buffers(result, type = "edge")

  # Extract non-buffer treatments
  non_buffers <- result_buffered$design[result_buffered$design$treatment != "buffer", ]

  # Check that we have the same number of treatments
  expect_equal(nrow(non_buffers), nrow(result$design_df))

  # Check that all original treatments are present
  expect_setequal(non_buffers$treatment, result$design_df$treatment)
})

test_that("multiple buffer types can be visualized", {
  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result <- speed(df, swap = "treatment", iterations = 50, quiet = TRUE, seed = 42)

  # Test different buffer types
  buffer_types <- c("edge", "row", "column", "double row", "double column")

  for (type in buffer_types) {
    buffered <- add_buffers(result, type = type)

    expect_no_error({
      p <- autoplot(buffered)
    })

    expect_s3_class(p, "ggplot")
    expect_true("buffer" %in% buffered$design$treatment)
  }
})

test_that("buffers work with legend display", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result <- speed(df, swap = "treatment", iterations = 50, quiet = TRUE, seed = 42)
  buffered_result <- add_buffers(result, type = "edge")

  p_legend <- autoplot(buffered_result, legend = TRUE)
  expect_s3_class(p_legend, "ggplot")

  vdiffr::expect_doppelganger("buffers-with-legend-display", p_legend)
})

test_that("buffers work with rotation and size parameters", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result <- speed(df, swap = "treatment", iterations = 50, quiet = TRUE, seed = 42)
  buffered_result <- add_buffers(result, type = "edge")

  # Test with rotation
  p_rotated <- autoplot(buffered_result, rotation = 45, size = 3)
  expect_s3_class(p_rotated, "ggplot")

  vdiffr::expect_doppelganger("buffers-rotated-text", p_rotated)
})

test_that("hierarchical designs can have buffers added", {
  df_split <- data.frame(
    row = rep(1:6, each = 4),
    col = rep(1:4, times = 6),
    block = rep(1:2, each = 12),
    wholeplot = rep(1:6, each = 4),
    wholeplot_treatment = rep(rep(LETTERS[1:3], each = 2), times = 4),
    subplot_treatment = rep(letters[1:4], 6)
  )

  result <- speed(df_split,
                  swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                  swap_within = list(wp = "block", sp = "wholeplot"),
                  iterations = 50,
                  quiet = TRUE,
                  seed = 42)

  # Add buffers to hierarchical design
  buffered_result <- add_buffers(result, type = "edge")

  expect_true(inherits(buffered_result, "design"))
  expect_true("buffer" %in% buffered_result$design$subplot_treatment)

  # Test plotting
  expect_no_error({
    p_wp <- autoplot(buffered_result, treatments = "wholeplot_treatment")
    p_sp <- autoplot(buffered_result, treatments = "subplot_treatment", block = "wholeplot")
  })
})

test_that("buffers are displayed as white in plots", {
  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(c("A", "B", "C"), 3)
  )

  result <- speed(df, swap = "treatment", iterations = 50, quiet = TRUE, seed = 42)
  buffered_result <- add_buffers(result, type = "edge")

  p <- autoplot(buffered_result)

  # Extract plot data to verify buffer color
  plot_build <- ggplot2::ggplot_build(p)
  fill_colors <- unique(plot_build$data[[1]]$fill)

  # Buffers should be white
  expect_true("white" %in% fill_colors || "#FFFFFF" %in% fill_colors)
})

test_that("buffer count is correct for different design sizes", {
  # 2x2 design
  df_2x2 <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, times = 2),
    treatment = rep(c("A", "B"), 2)
  )

  result_2x2 <- create_buffers(df_2x2, type = "edge", blocks = FALSE)
  buffers_2x2 <- sum(result_2x2$treatment == "buffer")
  # Edge: (2+2)*2 + 2*2 = 12
  expect_equal(buffers_2x2, 12)

  # 3x3 design
  df_3x3 <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result_3x3 <- create_buffers(df_3x3, type = "edge", blocks = FALSE)
  buffers_3x3 <- sum(result_3x3$treatment == "buffer")
  # Edge: (3+2)*2 + 3*2 = 16
  expect_equal(buffers_3x3, 16)

  # 4x5 design
  df_4x5 <- data.frame(
    row = rep(1:4, each = 5),
    col = rep(1:5, times = 4),
    treatment = rep(LETTERS[1:5], 4)
  )

  result_4x5 <- create_buffers(df_4x5, type = "edge", blocks = FALSE)
  buffers_4x5 <- sum(result_4x5$treatment == "buffer")
  # Edge: (5+2)*2 + 4*2 = 22
  expect_equal(buffers_4x5, 22)
})

test_that("buffers work correctly with early stopping", {
  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  result <- speed(df, swap = "treatment",
                  iterations = 10000,
                  early_stop_iterations = 50,
                  quiet = TRUE,
                  seed = 42)

  # Add buffers to early-stopped design
  buffered_result <- add_buffers(result, type = "edge")

  expect_true(inherits(buffered_result, "design"))
  expect_true("buffer" %in% buffered_result$design$treatment)
  expect_equal(buffered_result$stopped_early, result$stopped_early)
})

test_that("add_buffers handles design objects that are lists", {
  # Create a design object that's a list
  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  design_obj <- list(
    design_df = df,
    initial_design = df
  )
  class(design_obj) <- c("design", "list")

  # Should handle the list structure correctly
  result <- add_buffers(design_obj, type = "edge")

  expect_true(inherits(result, "design"))
  expect_true("buffer" %in% result$design$treatment)
})

test_that("add_buffers works with different buffer types", {
  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(LETTERS[1:3], 3)
  )

  design_obj <- list(
    design_df = df,
    initial_design = df
  )
  class(design_obj) <- "design"

  # Test different buffer types
  result_edge <- add_buffers(design_obj, type = "edge")
  result_row <- add_buffers(design_obj, type = "row")
  result_col <- add_buffers(design_obj, type = "column")

  expect_true("buffer" %in% result_edge$design$treatment)
  expect_true("buffer" %in% result_row$design$treatment)
  expect_true("buffer" %in% result_col$design$treatment)

  # Different buffer types should produce different numbers of buffers
  expect_false(nrow(result_edge$design) == nrow(result_row$design))
})

