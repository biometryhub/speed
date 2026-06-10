test_that("initialise_split_design_df builds the split-plot from the docs", {
  df <- initialise_split_design_df(
    splits = list(
      subplot = list(items = letters[1:4]),
      wholeplot = list(items = LETTERS[1:3], nrows = 1, ncols = 4),
      block = list(nrows = 3, ncols = 4)
    ),
    rep_dim = c(2, 2)
  )

  expect_setequal(names(df), c(
    "row", "col", "block", "wholeplot", "wholeplot_treatment", "subplot", "subplot_treatment"
  ))

  # 3x4 block tiled 2x2 -> 6x8 field of 48 cells
  expect_equal(nrow(df), 48)
  expect_equal(max(df$row), 6)
  expect_equal(max(df$col), 8)
  expect_equal(sort(df$row), sort(rep(1:6, 8)))
  expect_equal(sort(df$col), sort(rep(1:8, 6)))

  # 4 blocks, each holding 3 wholeplots, each holding 4 subplots
  expect_equal(length(unique(df$block)), 4)
  expect_equal(length(unique(df$wholeplot)), 12)
  expect_equal(length(unique(df$subplot)), 48)

  # Layout matches the illustration: wholeplot treatment depends only on the
  # row within a block (A/B/C down the rows), subplot only on the column.
  expect_equal(df$wholeplot_treatment, LETTERS[((df$row - 1) %% 3) + 1])
  expect_equal(df$subplot_treatment, letters[((df$col - 1) %% 4) + 1])

  # Each wholeplot carries a single treatment; each block sees a full set
  per_wholeplot <- tapply(df$wholeplot_treatment, df$wholeplot, function(x) length(unique(x)))
  expect_true(all(per_wholeplot == 1))
  for (set in tapply(df$wholeplot_treatment, df$block, function(x) sort(unique(x)))) {
    expect_equal(set, LETTERS[1:3])
  }
  # Each wholeplot sees a full subplot-treatment set
  for (set in tapply(df$subplot_treatment, df$wholeplot, function(x) sort(unique(x)))) {
    expect_equal(set, letters[1:4])
  }
})

test_that("initialise_split_design_df numbers blocks column-major", {
  df <- initialise_split_design_df(
    splits = list(
      subplot = list(items = letters[1:4]),
      wholeplot = list(items = LETTERS[1:3], nrows = 1, ncols = 4),
      block = list(nrows = 3, ncols = 4)
    ),
    rep_dim = c(2, 2)
  )

  # block 1 top-left, 2 bottom-left, 3 top-right, 4 bottom-right
  expect_equal(unique(df$block[df$row <= 3 & df$col <= 4]), 1)
  expect_equal(unique(df$block[df$row >= 4 & df$col <= 4]), 2)
  expect_equal(unique(df$block[df$row <= 3 & df$col >= 5]), 3)
  expect_equal(unique(df$block[df$row >= 4 & df$col >= 5]), 4)
})

test_that("initialise_split_design_df defaults rep_dim to a single replicate", {
  df <- initialise_split_design_df(
    splits = list(
      subplot = list(items = letters[1:4]),
      wholeplot = list(items = LETTERS[1:3], nrows = 1, ncols = 4),
      block = list(nrows = 3, ncols = 4)
    )
  )

  expect_equal(nrow(df), 12)
  expect_equal(length(unique(df$block)), 1)
  expect_equal(max(df$row), 3)
  expect_equal(max(df$col), 4)
})

test_that("initialise_split_design_df supports more than two split levels", {
  df <- initialise_split_design_df(
    splits = list(
      subsubplot = list(items = 1:2),
      subplot = list(items = letters[1:4], nrows = 1, ncols = 2),
      wholeplot = list(items = LETTERS[1:3], nrows = 1, ncols = 8),
      block = list(nrows = 3, ncols = 8)
    ),
    rep_dim = c(2, 1)
  )

  expect_true(all(c(
    "block", "wholeplot", "wholeplot_treatment", "subplot", "subplot_treatment",
    "subsubplot", "subsubplot_treatment"
  ) %in% names(df)))

  # 3x8 block tiled 2x1 -> 6x8
  expect_equal(nrow(df), 48)
  expect_equal(length(unique(df$block)), 2)
  expect_equal(length(unique(df$wholeplot)), 6) # 3 per block
  expect_equal(length(unique(df$subplot)), 24) # 4 per wholeplot
  expect_equal(length(unique(df$subsubplot)), 48) # 2 per subplot

  # Each parent gets a full inner treatment set
  for (parent in unique(df$wholeplot)) {
    expect_setequal(df$subplot_treatment[df$wholeplot == parent], letters[1:4])
  }
  for (parent in unique(df$subplot)) {
    expect_setequal(df$subsubplot_treatment[df$subplot == parent], 1:2)
  }
})

test_that("initialise_split_design_df expands a numeric scalar items to T1..TN", {
  df <- initialise_split_design_df(
    splits = list(
      subplot = list(items = 4),
      block = list(nrows = 1, ncols = 4)
    )
  )

  expect_setequal(unique(df$subplot_treatment), paste0("T", 1:4))
  expect_equal(length(unique(df$subplot)), 4)
})

test_that("initialise_split_design_df recycles a divisor-length items per parent", {
  df <- initialise_split_design_df(
    splits = list(
      subplot = list(items = letters[1:2]),
      block = list(nrows = 1, ncols = 4)
    )
  )

  # 4 subplots per block, 2 items -> a, b, a, b
  expect_equal(df$subplot_treatment, c("a", "b", "a", "b"))
})

test_that("initialise_split_design_df omits the treatment column when items is absent", {
  df <- initialise_split_design_df(
    splits = list(
      subplot = list(),
      block = list(nrows = 2, ncols = 2)
    )
  )

  expect_false("subplot_treatment" %in% names(df))
  expect_true("subplot" %in% names(df))
  expect_equal(length(unique(df$subplot)), 4)
})

test_that("initialise_split_design_df validates its inputs", {
  # fewer than two levels
  expect_error(
    initialise_split_design_df(list(block = list(nrows = 3, ncols = 4))),
    "at least two levels"
  )

  # rep_dim must be length 2
  expect_error(
    initialise_split_design_df(
      list(sp = list(items = letters[1:4]), block = list(nrows = 3, ncols = 4)),
      rep_dim = c(2, 2, 1)
    ),
    "length-2 vector"
  )
  expect_error(
    initialise_split_design_df(
      list(sp = list(items = letters[1:4]), block = list(nrows = 3, ncols = 4)),
      rep_dim = c(0, 2)
    )
  )

  # unknown split argument
  expect_error(
    initialise_split_design_df(
      list(sp = list(nrow = 1), block = list(nrows = 3, ncols = 4))
    ),
    "`nrow` is an invalid argument"
  )

  # non-whole dimension
  expect_error(
    initialise_split_design_df(
      list(sp = list(nrows = 1.5, ncols = 1), block = list(nrows = 3, ncols = 4))
    ),
    "must be a positive whole number"
  )

  # level does not tile evenly into its parent
  expect_error(
    initialise_split_design_df(
      list(sp = list(nrows = 2, ncols = 1), block = list(nrows = 3, ncols = 4))
    ),
    "does not tile evenly"
  )

  # items length does not divide units per parent
  expect_error(
    initialise_split_design_df(
      list(sp = list(items = letters[1:5]), block = list(nrows = 3, ncols = 4))
    ),
    "does not divide the 12 units per parent"
  )
})
