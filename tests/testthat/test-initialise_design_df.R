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

test_that("an alias initialize_design_df works", {
  items <- c(1, 2, 2, 1, 3, 3, 1, 3, 3)
  nrows <- 3
  ncols <- 3

  design_df <- initialize_design_df(items, nrows, ncols)

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
  ncols_block <- 4

  n_items <- length(items)

  design_df <- initialise_design_df(items, nrows, ncols, nrows_block, ncols_block)

  expect_setequal(names(design_df), c("row", "col", "treatment", "row_block", "col_block", "block"))
  expect_equal(nrow(design_df), n_items)
  expect_equal(sort(design_df$row), sort(rep(1:nrows, ncols)))
  expect_equal(sort(design_df$col), sort(rep(1:ncols, nrows)))

  # check each block
  for (block in unique(design_df$block)) {
    expect_setequal(design_df$treatment[design_df$block == block], 1:8)
  }

  rows_block <- nrows / nrows_block
  cols_block <- ncols / ncols_block
  n_blocks <- rows_block * cols_block
  expect_equal(sort(design_df$row_block), sort(rep(1:rows_block, n_items / rows_block)))
  expect_equal(sort(design_df$col_block), sort(rep(1:cols_block, n_items / cols_block)))
  expect_equal(sort(design_df$block), sort(rep(1:n_blocks, n_items / n_blocks)))
})

test_that("initialise_design_df works with blocking with different items", {
  items <- rep(1:8, 4)
  nrows <- 8
  ncols <- 4
  nrows_block <- 2
  ncols_block <- 2

  n_items <- length(items)

  design_df <- initialise_design_df(items, nrows, ncols, nrows_block, ncols_block)

  expect_setequal(names(design_df), c("row", "col", "treatment", "row_block", "col_block", "block"))
  expect_equal(nrow(design_df), n_items)
  expect_equal(sort(design_df$row), sort(rep(1:nrows, ncols)))
  expect_equal(sort(design_df$col), sort(rep(1:ncols, nrows)))

  # check each block
  for (block in unique(design_df$block)) {
    expect_equal(length(unique(design_df$treatment[design_df$block == block])), 4)
  }

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

test_that("initialise_design_df works for multi sites", {
  items <- c(rep(1:10, 6), rep(11:20, 8))
  df_site1 <- expand.grid(row = 1:10, col = 1:3)
  df_site1$site <- "a"
  df_site2 <- expand.grid(row = 1:10, col = 1:5)
  df_site2$site <- "b"
  df_site3 <- expand.grid(row = 1:10, col = 1:6)
  df_site3$site <- "c"
  df <- rbind(df_site1, df_site2, df_site3)
  df$treatment <- items

  design_df <- initialise_design_df(
    items = items,
    designs = list(
      a = list(nrows = 10, ncols = 3),
      b = list(nrows = 10, ncols = 5),
      c = list(nrows = 10, ncols = 6)
    )
  )

  expect_setequal(names(design_df), c("row", "col", "treatment", "site"))
  expect_equal(sort(design_df$row), sort(df$row))
  expect_equal(sort(design_df$col), sort(df$col))
  expect_equal(sort(design_df$treatment), sort(items))
  expect_setequal(design_df$site, c("a", "b", "c"))
})

test_that("initialise_design_df works for multi sites w/o names", {
  items <- c(rep(1:10, 6), rep(11:20, 8))
  df_site1 <- expand.grid(row = 1:10, col = 1:3)
  df_site1$site <- "1"
  df_site2 <- expand.grid(row = 1:10, col = 1:5)
  df_site2$site <- "2"
  df_site3 <- expand.grid(row = 1:10, col = 1:6)
  df_site3$site <- "c"
  df <- rbind(df_site1, df_site2, df_site3)
  df$treatment <- items

  design_df <- initialise_design_df(
    items = items,
    designs = list(
      list(nrows = 10, ncols = 3),
      list(nrows = 10, ncols = 5),
      c = list(nrows = 10, ncols = 6)
    )
  )

  expect_setequal(names(design_df), c("row", "col", "treatment", "site"))
  expect_equal(sort(design_df$row), sort(df$row))
  expect_equal(sort(design_df$col), sort(df$col))
  expect_equal(sort(design_df$treatment), sort(items))
  expect_equal(length(unique(design_df$site)), 3)
})

test_that("initialise_design_df works for multi sites with separate treatments", {
  items <- c(rep(1:10, 6), rep(11:20, 8))
  df_site1 <- expand.grid(row = 1:10, col = 1:3)
  df_site1$treatment <- items[1:30]
  df_site1$site <- "a"
  df_site2 <- expand.grid(row = 1:10, col = 1:5)
  df_site2$treatment <- items[31:80]
  df_site2$site <- "b"
  df_site3 <- expand.grid(row = 1:10, col = 1:6)
  df_site3$treatment <- items[81:140]
  df_site3$site <- "c"
  df <- rbind(df_site1, df_site2, df_site3)

  design_df <- initialise_design_df(
    items = items,
    designs = list(
      a = list(items = items[1:30], nrows = 10, ncols = 3),
      b = list(items = items[31:80], nrows = 10, ncols = 5),
      c = list(items = items[81:140], nrows = 10, ncols = 6)
    )
  )

  expect_setequal(names(design_df), c("row", "col", "treatment", "site"))
  expect_equal(sort(design_df$row), sort(df$row))
  expect_equal(sort(design_df$col), sort(df$col))
  expect_setequal(design_df$site, c("a", "b", "c"))
  for (site in c("a", "b", "c")) {
    expect_equal(sort(design_df$treatment[design_df$site == site]), sort(df$treatment[df$site == site]))
  }
})

test_that("initialise_design_df works for multi sites with partial blocking", {
  items <- c(rep(1:10, 6), rep(11:20, 8))
  df_site1 <- expand.grid(row = 1:10, col = 1:3)
  df_site1$site <- "1"
  df_site2 <- expand.grid(row = 1:10, col = 1:5)
  df_site2$site <- "2"
  df_site3 <- expand.grid(row = 1:10, col = 1:6)
  df_site3$site <- "c"
  df <- rbind(df_site1, df_site2, df_site3)
  df$treatment <- items

  design_df <- initialise_design_df(
    items = items,
    designs = list(
      list(nrows = 10, ncols = 3, block_nrows = 2, block_ncols = 3),
      list(nrows = 10, ncols = 5),
      c = list(nrows = 10, ncols = 6, block_nrows = 5, block_ncols = 3)
    )
  )

  expect_setequal(names(design_df), c("row", "col", "treatment", "site", "row_block", "col_block", "block"))
  expect_equal(sort(design_df$row), sort(df$row))
  expect_equal(sort(design_df$col), sort(df$col))
  expect_equal(sort(design_df$treatment), sort(items))
  expect_equal(length(unique(design_df$site)), 3)

  design_df_site1 <- design_df[design_df$site == "1", ]
  expect_equal(sort(design_df_site1$row_block), sort(rep(1:5, 6)))
  expect_equal(design_df_site1$col_block, rep(1, 30))
  expect_equal(sort(design_df_site1$block), sort(rep(1:5, 6)))

  design_df_site2 <- design_df[design_df$site == "2", ]
  expect_true(all(is.na(design_df_site2$row_block)))
  expect_true(all(is.na(design_df_site2$col_block)))
  expect_true(all(is.na(design_df_site2$block)))

  design_df_sitec <- design_df[design_df$site == "c", ]
  expect_equal(sort(design_df_sitec$row_block), sort(rep(1:2, 30)))
  expect_equal(sort(design_df_sitec$col_block), sort(rep(1:2, 30)))
  expect_equal(sort(design_df_sitec$block), sort(rep(1:4, 15)))
})
