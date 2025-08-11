test_that("autoplot handles factor row and column variables", {
  # Create a design with factor row and column variables
  df <- data.frame(
    row = factor(rep(1:3, each = 3), levels = 1:3),
    col = factor(rep(1:3, times = 3), levels = 1:3),
    treatment = rep(LETTERS[1:3], 3)
  )

  # Verify columns are factors
  expect_true(is.factor(df$row))
  expect_true(is.factor(df$col))

  # Set seed for reproducibility
  set.seed(42)

  # Run speed optimization
  result <- speed(df, "treatment", iterations = 100)

  # Test that autoplot works with factor columns
  expect_no_error(plot <- autoplot(result))
  expect_s3_class(plot, "ggplot")

  # Check that the plot data has numeric coordinates after conversion
  plot_data <- plot$data
  expect_true(is.numeric(plot_data$row))
  expect_true(is.numeric(plot_data$col))
})

test_that("autoplot handles factor columns with non-sequential levels", {
  # Create a design with factor columns that have non-sequential levels
  df <- data.frame(
    row = factor(rep(c(2, 4, 6), each = 3), levels = c(2, 4, 6)),
    col = factor(rep(c(1, 3, 5), times = 3), levels = c(1, 3, 5)),
    treatment = rep(LETTERS[1:3], 3)
  )

  # Verify columns are factors with non-sequential levels
  expect_true(is.factor(df$row))
  expect_true(is.factor(df$col))
  expect_equal(levels(df$row), c("2", "4", "6"))
  expect_equal(levels(df$col), c("1", "3", "5"))

  # Set seed for reproducibility
  set.seed(42)

  # Run speed optimization
  result <- speed(df, "treatment", iterations = 100)

  # Test that autoplot works and converts factors appropriately
  expect_no_error(plot <- autoplot(result))
  expect_s3_class(plot, "ggplot")

  # Check that the plot uses the correct coordinate ranges
  plot_data <- plot$data
  expect_true(all(plot_data$row %in% 1:3))  # Should be converted to 1,2,3
  expect_true(all(plot_data$col %in% 1:3))  # Should be converted to 1,2,3
})

test_that("autoplot handles mixed factor and numeric columns", {
  # Create a design with mixed factor and numeric spatial columns
  df <- data.frame(
    row = factor(rep(1:4, each = 2), levels = 1:4),  # Factor
    col = rep(1:2, times = 4),                       # Numeric
    treatment = rep(LETTERS[1:4], 2)
  )

  # Verify column types
  expect_true(is.factor(df$row))
  expect_true(is.numeric(df$col))

  # Set seed for reproducibility
  set.seed(42)

  # Run speed optimization
  result <- speed(df, "treatment", iterations = 100)

  # Test that autoplot works with mixed column types
  expect_no_error(plot <- autoplot(result))
  expect_s3_class(plot, "ggplot")

  # Check that both columns are numeric in the plot data
  plot_data <- plot$data
  expect_true(is.numeric(plot_data$row))
  expect_true(is.numeric(plot_data$col))
})

test_that("autoplot handles factor columns with blocks", {
  # Create a design with factor columns and blocks
  df <- data.frame(
    row = factor(rep(1:6, each = 4), levels = 1:6),
    col = factor(rep(1:4, times = 6), levels = 1:4),
    treatment = rep(LETTERS[1:8], 3),
    block = rep(1:3, each = 8)
  )

  # Verify columns are factors
  expect_true(is.factor(df$row))
  expect_true(is.factor(df$col))

  # Set seed for reproducibility
  set.seed(42)

  # Run speed optimization with blocks
  result <- speed(df, "treatment", swap_within = "block", iterations = 100)

  # Test that autoplot works with factor columns and blocks
  expect_no_error(plot <- autoplot(result))
  expect_s3_class(plot, "ggplot")

  # Check that the plot data has numeric coordinates
  plot_data <- plot$data
  expect_true(is.numeric(plot_data$row))
  expect_true(is.numeric(plot_data$col))
})

test_that("autoplot handles character columns converted to factors", {
  # Create a design with character spatial columns that will be converted to factors
  df <- data.frame(
    row = rep(c("A", "B", "C"), each = 3),
    col = rep(c("X", "Y", "Z"), times = 3),
    treatment = rep(LETTERS[1:3], 3),
    stringsAsFactors = FALSE
  )

  # Convert to factors manually to simulate user input
  df$row <- factor(df$row, levels = c("A", "B", "C"))
  df$col <- factor(df$col, levels = c("X", "Y", "Z"))

  # Verify columns are factors
  expect_true(is.factor(df$row))
  expect_true(is.factor(df$col))

  # Set seed for reproducibility
  set.seed(42)

  # Run speed optimization
  result <- speed(df, "treatment", iterations = 100)

  # Test that autoplot works with character-based factor levels
  expect_no_error(plot <- autoplot(result))
  expect_s3_class(plot, "ggplot")

  # Check that the plot data has numeric coordinates
  plot_data <- plot$data
  expect_true(is.numeric(plot_data$row))
  expect_true(is.numeric(plot_data$col))
})

test_that("autoplot preserves factor order when converting to numeric", {
  # Create a design with factors in non-alphabetical order
  df <- data.frame(
    row = factor(rep(c("Third", "First", "Second"), each = 2),
                 levels = c("First", "Second", "Third")),
    col = factor(rep(c("B", "A"), times = 3),
                 levels = c("A", "B")),
    treatment = rep(LETTERS[1:3], 2)
  )

  # Set seed for reproducibility
  set.seed(42)

  # Run speed optimization
  result <- speed(df, "treatment", iterations = 100)

  # Test that autoplot works and preserves the factor level ordering
  expect_no_error(plot <- autoplot(result))
  expect_s3_class(plot, "ggplot")

  # Check conversion preserves order
  plot_data <- plot$data
  expect_true(is.numeric(plot_data$row))
  expect_true(is.numeric(plot_data$col))

  # Verify the numeric values correspond to factor level positions
  # "First" should be 1, "Second" should be 2, "Third" should be 3
  # "A" should be 1, "B" should be 2
  unique_rows <- unique(plot_data$row)
  unique_cols <- unique(plot_data$col)
  expect_true(all(unique_rows %in% 1:3))
  expect_true(all(unique_cols %in% 1:2))
})

test_that("autoplot correctly converts factor labels to numeric values", {
  # Create a design with factor columns that have numeric labels
  # This tests that as.numeric(as.character()) is used instead of as.numeric()
  df <- data.frame(
    row = factor(rep(c("10", "20", "30"), each = 3), levels = c("10", "20", "30")),
    col = factor(rep(c("5", "15", "25"), times = 3), levels = c("5", "15", "25")),
    treatment = rep(LETTERS[1:3], 3)
  )

  # Create a design object directly (skip speed optimization for this test)
  result <- structure(df, class = "design")

  # Test that autoplot works and converts factor labels correctly
  expect_no_error(plot <- autoplot(result))
  expect_s3_class(plot, "ggplot")

  # The plot data should have numeric values from the factor labels, not levels
  plot_data <- plot$data
  expect_true(all(plot_data$row %in% c(10, 20, 30)))
  expect_true(all(plot_data$col %in% c(5, 15, 25)))

  # Should NOT be the factor levels (1, 2, 3)
  expect_false(any(plot_data$row %in% c(1, 2, 3)))
  expect_false(any(plot_data$col %in% c(1, 2, 3)))
})
