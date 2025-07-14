test_that("initialise_design_df works", {
  items <- c(1, 2, 2, 1, 3, 3, 1, 3, 3)
  nrows <- 3
  ncols <- 3

  design_df <- initialise_design_df(items, nrows, ncols)

  expect_setequal(names(design_df), c("row", "col", "treatment"))
  expect_equal(nrow(design_df), nrows * ncols)
  expect_equal(design_df$treatment, items)
  expect_equal(sort(design_df$row), sort(rep(1:nrows, ncols)))
  expect_equal(sort(design_df$col), sort(rep(1:ncols, nrows)))
})

test_that("initialise_design_df works with blocking", {
  items <- rep(1:8, 4)
  nrows <- 8
  ncols <- 4
  nrows_block <- 2
  ncols_block <- 2

  n_items <- length(items)

  design_df <- initialise_design_df(items, nrows, ncols, nrows_block, ncols_block)

  expect_setequal(names(design_df), c("row", "col", "treatment", "row_block", "col_block", "block"))
  expect_equal(nrow(design_df), n_items)
  expect_equal(design_df$treatment, items)
  expect_equal(sort(design_df$row), sort(rep(1:nrows, ncols)))
  expect_equal(sort(design_df$col), sort(rep(1:ncols, nrows)))

  rows_block <- nrows / nrows_block
  cols_block <- ncols / ncols_block
  n_blocks <- rows_block * cols_block
  expect_equal(sort(design_df$row_block), sort(rep(1:rows_block, n_items / rows_block)))
  expect_equal(sort(design_df$col_block), sort(rep(1:cols_block, n_items / cols_block)))
  expect_equal(sort(design_df$block), sort(rep(1:n_blocks, n_items / n_blocks)))
})

test_that("initialise_design_df converts single numeric to treatment labels", {
  # Test the specific line: if (length(items) == 1 && is.numeric(items))
  result <- initialise_design_df(items = 3, nrows = 3, ncols = 3)
  
  # Check that treatments are converted to "T1", "T2", "T3"
  expect_equal(result$treatment, rep(c("T1", "T2", "T3"), times = 3))
  
  # Test with different numbers
  result2 <- initialise_design_df(items = 2, nrows = 2, ncols = 2)
  expected_treatments2 <- c("T1", "T2", "T1", "T2")
  expect_equal(result2$treatment, expected_treatments2)
  
  # Test with single item
  result3 <- initialise_design_df(items = 1, nrows = 2, ncols = 2)
  expected_treatments3 <- c("T1", "T1", "T1", "T1")
  expect_equal(result3$treatment, expected_treatments3)
  
  # Verify the condition doesn't trigger when items is a vector
  result4 <- initialise_design_df(items = c(1, 2, 3), nrows = 3, ncols = 1)
  expect_equal(result4$treatment, c(1, 2, 3))
  
  # Verify the condition doesn't trigger when items is non-numeric
  result5 <- initialise_design_df(items = c("A", "B", "C"), nrows = 3, ncols = 1)
  expect_equal(result5$treatment, c("A", "B", "C"))
})
