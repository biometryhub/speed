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

test_that("initialize_design_df throws error for invalid inputs", {
  expect_error(initialise_design_df(1:16, 0, 4, 2, 2))
  expect_error(initialise_design_df(1:16, 4, 0, 2, 2))
  expect_error(initialise_design_df(1:16, 4, 4, 2))
  expect_error(initialise_design_df(1:16, 4, 4, NULL, 2))
  expect_error(initialise_design_df(1:16, 4, 4, 0, 2))
  expect_error(initialise_design_df(1:16, 4, 4, 2, 0))

  expect_error(initialise_design_df(1:16, designs = list(
    list(nrows = 4, ncols = 4),
    list(nrows = 4, ncols = 4, nn = 1)
  )), "`nn` is an invalid argument")

  expect_error(initialise_design_df(1:16, designs = list(
    list(nrows = 4, ncols = 4),
    list(nrows = 4)
  )), "`nrows` and `ncols` must be provided for each design")

  expect_error(initialise_design_df(designs = list(
    list(items = 1:4, nrows = 4, ncols = 4),
    list(nrows = 4, ncols = 4)
  )), "`items` must be provided for all designs")

  expect_error(initialise_design_df(designs = list(
    list(nrows = 4, ncols = 4),
    list(nrows = 4, ncols = 4)
  )), "`items` must be provided for all designs or `items` must be provided to `initialise_design_df`")
})

test_that("initialise_design_df builds a split-plot via splits", {
  skip_if_not_installed("vdiffr")

  df <- suppressWarnings(initialise_design_df(
    nrows = 12,
    ncols = 4,
    block_nrows = 12,
    block_ncols = 1,
    splits = list(
      wholeplot = list(nrows = 4, ncols = 1, items = LETTERS[1:3]),
      subplot = list(nrows = 1, ncols = 1, items = letters[1:4])
    )
  ))

  expect_setequal(names(df), c(
    "row", "col", "row_block", "col_block", "block", "wholeplot",
    "wholeplot_treatment", "subplot", "subplot_treatment"
  ))
  expect_equal(nrow(df), 48)

  # 4 blocks (replicates), each holding 3 wholeplots, each holding 4 subplots.
  expect_equal(length(unique(df$block)), 4)
  expect_equal(length(unique(df$wholeplot)), 12)
  expect_equal(length(unique(df$subplot)), 48)

  # Every wholeplot has exactly one wholeplot treatment.
  per_wholeplot <- tapply(df$wholeplot_treatment, df$wholeplot, function(x) length(unique(x)))
  expect_true(all(per_wholeplot == 1))

  # Every block sees a full wholeplot-treatment set.
  per_block <- tapply(df$wholeplot_treatment, df$block, function(x) sort(unique(x)))
  for (set in per_block) expect_equal(set, LETTERS[1:3])

  # Every wholeplot sees a full subplot-treatment set.
  per_wholeplot_sp <- tapply(df$subplot_treatment, df$wholeplot, function(x) sort(unique(x)))
  for (set in per_wholeplot_sp) expect_equal(set, letters[1:4])

  class(df) <- c("design", class(df))
  vdiffr::expect_doppelganger(
    "initialise-splitplot-wholeplot",
    autoplot(df, treatments = "wholeplot_treatment")
  )
  vdiffr::expect_doppelganger(
    "initialise-splitplot-subplot",
    autoplot(df, treatments = "subplot_treatment", block = "wholeplot")
  )
})

test_that("initialise_design_df supports split-split-plot via nested splits", {
  skip_if_not_installed("vdiffr")

  df <- suppressWarnings(initialise_design_df(
    nrows = 8,
    ncols = 4,
    block_nrows = 8,
    block_ncols = 2,
    splits = list(
      wp = list(nrows = 4, ncols = 2, items = LETTERS[1:2]),
      sp = list(nrows = 2, ncols = 2, items = letters[1:2]),
      ssp = list(nrows = 1, ncols = 1, items = c("x", "y", "z", "w"))
    )
  ))

  expect_true(all(c("wp", "wp_treatment", "sp", "sp_treatment", "ssp", "ssp_treatment") %in% names(df)))
  # 2 blocks, each holding 2 wholeplots, each holding 2 subplots, each holding 4 sub-subplots.
  expect_equal(length(unique(df$block)), 2)
  expect_equal(length(unique(df$wp)), 4)
  expect_equal(length(unique(df$sp)), 8)
  expect_equal(length(unique(df$ssp)), 32)

  # Each parent unit gets a full inner treatment set.
  for (parent in unique(df$wp)) {
    expect_setequal(df$sp_treatment[df$wp == parent], letters[1:2])
  }
  for (parent in unique(df$sp)) {
    expect_setequal(df$ssp_treatment[df$sp == parent], c("x", "y", "z", "w"))
  }

  class(df) <- c("design", class(df))
  vdiffr::expect_doppelganger("initialise-splitsplit-wp", autoplot(df, treatments = "wp_treatment"))
  vdiffr::expect_doppelganger(
    "initialise-splitsplit-sp",
    autoplot(df, treatments = "sp_treatment", block = "wp")
  )
  vdiffr::expect_doppelganger(
    "initialise-splitsplit-ssp",
    autoplot(df, treatments = "ssp_treatment", block = "sp")
  )
})

test_that("initialise_design_df expands a numeric scalar `items` in splits to T1..TN", {
  df <- suppressWarnings(initialise_design_df(
    nrows = 4, ncols = 2,
    splits = list(plot = list(nrows = 2, ncols = 1, items = 4))
  ))

  expect_setequal(unique(df$plot_treatment), paste0("T", 1:4))
  expect_equal(length(unique(df$plot)), 4)
})

test_that("initialise_design_df applies splits without an outer block", {
  df <- suppressWarnings(initialise_design_df(
    nrows = 6, ncols = 4,
    splits = list(plot = list(nrows = 2, ncols = 2, items = LETTERS[1:6]))
  ))

  expect_setequal(names(df), c("row", "col", "plot", "plot_treatment"))
  # No block columns when block_nrows/block_ncols is NULL.
  expect_false(any(c("block", "row_block", "col_block") %in% names(df)))
  # 6 cells per child (2x2), 24 cells total -> 6 children, each treatment used once.
  expect_equal(length(unique(df$plot)), 6)
  expect_setequal(unique(df$plot_treatment), LETTERS[1:6])
  # Every plot has a single treatment (one-to-one mapping).
  per_plot <- tapply(df$plot_treatment, df$plot, function(x) length(unique(x)))
  expect_true(all(per_plot == 1))
})

test_that("initialise_design_df warns that `splits` is deprecated and suggests a runnable call", {
  expect_warning(
    initialise_design_df(
      nrows = 12, ncols = 4,
      block_nrows = 3, block_ncols = 4,
      splits = list(
        wholeplot = list(nrows = 1, ncols = 4, items = LETTERS[1:3]),
        subplot = list(nrows = 1, ncols = 1, items = letters[1:4])
      )
    ),
    "deprecated"
  )

  # The suggested `initialise_split_design_df()` call must itself be runnable.
  suggestion <- suggest_split_design_df(
    splits = list(
      wholeplot = list(nrows = 1, ncols = 4, items = LETTERS[1:3]),
      subplot = list(nrows = 1, ncols = 1, items = letters[1:4])
    ),
    nrows = 12, ncols = 4, block_nrows = 3, block_ncols = 4
  )
  expect_no_error(eval(parse(text = suggestion)))
})

test_that("initialise_design_df splits validate parent dimensions", {
  expect_error(initialise_design_df(
    nrows = 6,
    ncols = 2,
    block_nrows = 6,
    block_ncols = 2,
    splits = list(wp = list(nrows = 4, ncols = 2))
  ))
  expect_error(initialise_design_df(
    nrows = 6,
    ncols = 2,
    splits = list(wp = list(nrows = 3, ncols = 2, foo = 1))
  ), "`foo` is an invalid argument")
  expect_error(initialise_design_df(nrows = 4, ncols = 4), "`items` must be provided when `splits` is `NULL`")

  # `items` length must equal n_children or divide it; 5 items into 6 children -> error.
  # This case passes validation, so it reaches (and emits) the deprecation warning
  # before `apply_splits` throws; suppress it like the other deprecated-path tests.
  expect_error(
    suppressWarnings(initialise_design_df(
      nrows = 6, ncols = 2,
      splits = list(wp = list(nrows = 2, ncols = 1, items = LETTERS[1:5]))
    )),
    "`items` for split `wp` must have length 6 \\(or divide it\\); got 5"
  )

  # Each split entry must provide both `nrows` and `ncols`.
  expect_error(
    initialise_design_df(
      nrows = 6, ncols = 2,
      splits = list(wp = list(nrows = 2))
    ),
    "`nrows` and `ncols` must be provided for split `wp`"
  )
})
