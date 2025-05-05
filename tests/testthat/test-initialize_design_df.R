test_that("initialize_design_df works for simple design", {
  nrows <- 5
  ncols <- 5
  treatments <- rep(1:5, 5)
  design_df <- initialize_design_df(treatments, nrows, ncols)

  expect_equal(nrow(design_df), nrows * ncols)
  expect_equal(sort(design_df$row), sort(factor(rep(1:nrows, ncols))))
  expect_equal(sort(design_df$col), sort(factor(rep(1:ncols, nrows))))
  expect_equal(design_df$treatment, factor(treatments))
  expect_true(all(!c("row_block", "col_block", "block") %in% names(design_df)))
  expect_equal(sum(design_df[is.null(design_df), ]), 0)
})

test_that("initialize_design_df works for blocked design", {
  nrows <- 20
  ncols <- 6
  nrows_block <- 20
  ncols_block <- 2
  treatments <- rep(1:40, 3)
  design_df <- initialize_design_df(treatments, nrows, ncols, nrows_block, ncols_block)

  expect_equal(nrow(design_df), nrows * ncols)
  expect_equal(sort(design_df$row), sort(factor(rep(1:nrows, ncols))))
  expect_equal(sort(design_df$col), sort(factor(rep(1:ncols, nrows))))
  expect_equal(sort(design_df$row_block), sort(factor(rep(1:(nrows / nrows_block), ncols * nrows_block))))
  expect_equal(sort(design_df$col_block), sort(factor(rep(1:(ncols / ncols_block), nrows * ncols_block))))
  expect_equal(design_df$treatment, factor(treatments))
  expect_equal(sum(design_df[is.null(design_df), ]), 0)
})

test_that("initialize_design_df throws error for invalid inputs", {
  expect_error(initialize_design_df(1, 4, 4, 2, 2))
  expect_error(initialize_design_df(1:16, 0, 4, 2, 2))
  expect_error(initialize_design_df(1:16, 4, 0, 2, 2))
  expect_error(initialize_design_df(1:16, 4, 4, 2))
  expect_error(initialize_design_df(1:16, 4, 4, NULL, 2))
  expect_error(initialize_design_df(1:16, 4, 4, 0, 2))
  expect_error(initialize_design_df(1:16, 4, 4, 2, 0))
})
