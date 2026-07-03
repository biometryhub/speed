test_that("initialise_split_design_df builds the split-plot from the docs", {
  df <- initialise_split_design_df(
    list(
      block = list(nrows = 3, ncols = 4),
      wholeplot = list(items = LETTERS[1:3], nrows = 1, ncols = 4),
      subplot = list(items = letters[1:4])
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

  # Each wholeplot carries a single treatment; each block sees a full set
  per_wholeplot <- tapply(df$wholeplot_treatment, df$wholeplot, function(x) length(unique(x)))
  expect_true(all(per_wholeplot == 1))
  for (set in tapply(df$wholeplot_treatment, df$block, function(x) sort(unique(x)))) {
    expect_setequal(set, LETTERS[1:3])
  }
  # Each wholeplot sees a full subplot-treatment set
  for (set in tapply(df$subplot_treatment, df$wholeplot, function(x) sort(unique(x)))) {
    expect_setequal(set, letters[1:4])
  }
})

test_that("initialise_split_design_df units cover the right dimensions", {
  df <- initialise_split_design_df(
    list(
      block = list(nrows = 4, ncols = 4),
      wholeplot = list(nrows = 2, ncols = 2),
      subplot = list(items = letters[1:4])
    ),
    rep_dim = c(1, 2)
  )

  # Each unit at a level must occupy a contiguous nrows x ncols rectangle
  # (the fill order within that rectangle is unconstrained).
  expect_unit_dims <- function(level, nrows, ncols) {
    for (id in unique(df[[level]])) {
      cells <- df[df[[level]] == id, ]
      rows <- sort(unique(cells$row))
      cols <- sort(unique(cells$col))
      expect_equal(nrow(cells), nrows * ncols)
      expect_equal(rows, seq(min(rows), length.out = nrows))
      expect_equal(cols, seq(min(cols), length.out = ncols))
    }
  }

  expect_unit_dims("block", 4, 4)
  expect_unit_dims("wholeplot", 2, 2)
  expect_unit_dims("subplot", 1, 1)
})

test_that("initialise_split_design_df defaults rep_dim to a single replicate", {
  df <- initialise_split_design_df(list(
    block = list(nrows = 3, ncols = 4),
    wholeplot = list(items = LETTERS[1:3], nrows = 1, ncols = 4),
    subplot = list(items = letters[1:4])
  ))

  expect_equal(nrow(df), 12)
  expect_equal(length(unique(df$block)), 1)
  expect_equal(max(df$row), 3)
  expect_equal(max(df$col), 4)
})

test_that("initialise_split_design_df supports more than two split levels", {
  df <- initialise_split_design_df(
    list(
      block = list(nrows = 3, ncols = 8),
      wholeplot = list(items = LETTERS[1:3], nrows = 1, ncols = 8),
      subplot = list(items = letters[1:4], nrows = 1, ncols = 2),
      subsubplot = list(items = 1:2)
    ),
    rep_dim = c(2, 1)
  )

  expect_true(all(c(
    "block", "wholeplot", "wholeplot_treatment", "subplot", "subplot_treatment",
    "subsubplot", "subsubplot_treatment"
  ) %in% names(df)))

  # 2x1 reps of 3x8 and everything
  expect_equal(nrow(df), 48)
  expect_equal(length(unique(df$block)), 2)
  expect_equal(length(unique(df$wholeplot)), 6)
  expect_equal(length(unique(df$subplot)), 24)
  expect_equal(length(unique(df$subsubplot)), 48)

  # Each parent gets a full inner treatment set
  for (parent in unique(df$wholeplot)) {
    expect_setequal(df$subplot_treatment[df$wholeplot == parent], letters[1:4])
  }
  for (parent in unique(df$subplot)) {
    expect_setequal(df$subsubplot_treatment[df$subplot == parent], 1:2)
  }
})

test_that("initialise_split_design_df expands a numeric scalar items to T1..TN", {
  df <- initialise_split_design_df(list(
    block = list(nrows = 1, ncols = 4),
    subplot = list(items = 4)
  ))

  expect_setequal(unique(df$subplot_treatment), paste0("T", 1:4))
  expect_equal(length(unique(df$subplot)), 4)
})

test_that("initialise_split_design_df recycles a divisor-length items per parent", {
  df <- initialise_split_design_df(list(
    block = list(nrows = 1, ncols = 4),
    subplot = list(items = letters[1:2])
  ))

  expect_equal(sort(df$subplot_treatment), c("a", "a", "b", "b"))
})

test_that("initialise_split_design_df omits the treatment column when items is absent", {
  df <- initialise_split_design_df(list(
    block = list(nrows = 2, ncols = 2),
    subplot = list()
  ))

  expect_false("subplot_treatment" %in% names(df))
  expect_true("subplot" %in% names(df))
  expect_equal(length(unique(df$subplot)), 4)
})

test_that("initialise_split_design_df validates its inputs", {
  block <- list(nrows = 3, ncols = 4)
  with_default_block <- function(sp, ...) {
    initialise_split_design_df(list(block = block, sp = sp), ...)
  }

  expect_error(initialise_split_design_df(list(block = block)), "at least two levels")

  # incorrect rep_dim
  expect_error(
    with_default_block(list(items = letters[1:4]), rep_dim = c(2, 2, 1)),
    "length-2 vector"
  )
  expect_error(with_default_block(list(items = letters[1:4]), rep_dim = c(0, 2)))

  # unknown split argument
  expect_error(with_default_block(list(nrow = 1)), "`nrow` is an invalid argument")

  # non-whole dimension
  expect_error(with_default_block(list(nrows = 1.5, ncols = 1)), "must be a positive whole number")

  # level does not tile evenly into its parent
  expect_error(with_default_block(list(nrows = 2, ncols = 1)), "does not tile evenly")

  # items length does not divide units per parent
  expect_error(with_default_block(list(items = letters[1:5])), "does not divide the 12 units per parent")

  # only the innermost level may omit dimensions
  expect_error(
    initialise_split_design_df(list(
      block = list(nrows = 2, ncols = 4),
      mid = list(items = letters[1:2]),
      sub = list()
    )),
    "must be provided for split `mid`"
  )
})
