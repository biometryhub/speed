# The published designs below are written out visually, one grid row per source
# line, so they can be read against the paper. `initialise_design_df()` assigns
# `items` *down columns* - `expand.grid(row, col)` varies `row` fastest - so a
# row-major literal has to be transposed before it is handed over, or the design
# stored at those coordinates is not the one written here.
by_row <- function(items, nrows, ncols) {
  initialise_design_df(
    as.vector(matrix(items, nrow = nrows, ncol = ncols, byrow = TRUE)),
    nrows,
    ncols
  )
}

test_that("calculate_efficiency_factor provides the same results as the paper", {
  # fmt: skip
  df_design1 <- by_row(c(
    7, 5, 6, 9, 4, 1, 3, 2, 8,
    5, 6, 3, 1, 7, 8, 2, 4, 9,
    8, 9, 5, 6, 3, 4, 1, 7, 2,
    1, 8, 2, 7, 6, 3, 9, 5, 4
  ), 4, 9)

  expect_equal(
    round(calculate_efficiency_factor(df_design1, "treatment"), 3),
    0.834
  )

  # fmt: skip
  df_design2 <- by_row(c(
    8, 5, 7, 2, 4, 1, 6, 9, 3,
    1, 9, 8, 6, 3, 2, 4, 7, 5,
    7, 6, 4, 9, 5, 8, 3, 2, 1,
    4, 2, 3, 1, 7, 9, 5, 8, 6
  ), 4, 9)

  expect_equal(
    round(calculate_efficiency_factor(df_design2, "treatment"), 3),
    0.783
  )

  # fmt: skip
  df_design3 <- by_row(c(
    9, 8, 1, 4, 3, 7, 5, 2, 6,
    7, 5, 6, 2, 9, 1, 3, 8, 4,
    2, 4, 3, 5, 6, 8, 9, 7, 1,
    1, 9, 8, 7, 4, 3, 2, 6, 5
  ), 4, 9)

  expect_equal(
    round(calculate_efficiency_factor(df_design3, "treatment"), 3),
    0.827
  )

  # fmt: skip
  df_design4 <- by_row(c(
    47, 16, 43, 42, 37, 35,  1, 59, 24, 19,  4, 18, 40, 28, 51, 29, 54, 57,
    12, 25,  6, 57, 47, 32, 39, 17, 31, 50, 15,  5, 55, 51,  9, 54, 41,  3,
    23, 18, 45, 36, 49,  7,  8, 60, 41, 29,  3, 58, 26, 52,  2, 15, 28, 27,
    56, 52, 42, 31, 53, 17, 27, 48,  7, 13, 10, 21, 19, 12, 33, 30, 34, 16,
    43, 22, 13, 20, 32, 58, 48,  9, 46,  8, 37, 40, 56, 14,  4, 11, 38, 44
  ), 5, 18)

  expect_equal(
    round(calculate_efficiency_factor(df_design4, "treatment"), 3),
    0.529
  )
})

test_that("calculate_efficiency_factor provides better result for an optimised design", {
  # A poor but *estimable* comparator. The obvious "unoptimised" layout - each
  # treatment filling one grid row - is not merely inefficient, it confounds
  # treatment with row, so it has no efficiency factor at all and is refused
  # (see the estimability tests below). Comparing against it would have been
  # comparing against a number that does not exist.
  # fmt: skip
  df_design_initial <- by_row(c(
    1, 2, 6, 3,
    4, 3, 5, 5,
    1, 4, 2, 6
  ), 3, 4)

  # fmt: skip
  df_design_optimised <- by_row(c(
    1, 2, 4, 3,
    5, 1, 6, 2,
    3, 6, 5, 4
  ), 3, 4)

  expect_lt(
    abs(1 - calculate_efficiency_factor(df_design_optimised, "treatment")),
    abs(1 - calculate_efficiency_factor(df_design_initial, "treatment"))
  )
  # Both are genuine values in [0, 1], so the comparison above is meaningful.
  expect_lte(calculate_efficiency_factor(df_design_optimised, "treatment"), 1)
  expect_gt(calculate_efficiency_factor(df_design_initial, "treatment"), 0)
})

test_that("calculate_efficiency_factor provides same result for mathematically identical designs", {
  # fmt: skip
  df_design1 <- by_row(c(
    1, 2, 4, 3,
    5, 1, 6, 2,
    3, 6, 5, 4
  ), 3, 4)

  # fmt: skip
  df_design2 <- by_row(c(
    "a", "b", "d", "c",
    "e", "a", "f", "b",
    "c", "f", "e", "d"
  ), 3, 4)

  # fmt: skip
  df_design3 <- by_row(c(
    "b", "a", "c", "d",
    "e", "b", "f", "a",
    "d", "f", "e", "c"
  ), 3, 4)

  expect_equal(
    calculate_efficiency_factor(df_design1, "treatment"),
    calculate_efficiency_factor(df_design2, "treatment")
  )

  expect_equal(
    calculate_efficiency_factor(df_design3, "treatment"),
    calculate_efficiency_factor(df_design2, "treatment")
  )
})

test_that("calculate_efficiency_factor refuses a design whose contrasts are not estimable", {
  # C occupies row 3 entirely, so the C-vs-A contrast cannot be separated from
  # the row 3 effect. Confirmed independently: fitting
  # `y ~ factor(row) + factor(col) + treatment` with lm() aliases one treatment
  # coefficient. There is no efficiency factor for such a design - before this
  # check the formula returned a finite, plausible-looking value anyway.
  df_confounded_row <- data.frame(
    row = c(1, 1, 2, 2, 3, 3),
    col = c(1, 2, 1, 2, 1, 2),
    treatment = c("A", "B", "A", "B", "C", "C")
  )

  expect_error(
    calculate_efficiency_factor(df_confounded_row, "treatment"),
    class = "speed_efficiency_rank"
  )
  expect_error(
    calculate_efficiency_factor(df_confounded_row, "treatment"),
    "not all treatment contrasts are estimable",
    ignore.case = TRUE
  )
})

test_that("calculate_efficiency_factor keeps designs with no residual df to spare", {
  # The boundary the gate must not overshoot: zero residual degrees of freedom
  # is still estimable, so these must return a value rather than be refused.
  # fmt: skip
  df_df0 <- by_row(c(
    1, 2, 3,
    3, 1, 2
  ), 2, 3)

  result <- calculate_efficiency_factor(df_df0, "treatment")
  expect_true(is.finite(result))
  expect_gt(result, 0)
  expect_lte(result, 1)
})

test_that("calculate_efficiency_factor works with minimal design dimensions", {
  # Test edge case with very small design that might have numerical issues
  df_design_minimal <- data.frame(
    row = c(1, 1, 2, 2),
    col = c(1, 2, 1, 2),
    treatment = c("A", "B", "B", "A")
  )

  expect_no_error({
    result <- calculate_efficiency_factor(df_design_minimal, "treatment")
  })

  result <- calculate_efficiency_factor(df_design_minimal, "treatment")
  expect_type(result, "double")
  expect_true(is.finite(result))
  expect_gt(result, 0)
})

test_that("calculate_efficiency_factor refuses a single-column design with blocked treatments", {
  # One plot per row means every row effect is a plot effect, so nothing is left
  # to estimate a treatment difference with. Refusing is the only honest answer;
  # the formula previously returned a finite value here.
  df_design_confounded <- data.frame(
    row = rep(1:8, each = 1),
    col = rep(1, times = 8),
    treatment = c("A", "A", "A", "A", "B", "B", "B", "B") # Single column, treatments in blocks
  )

  expect_error(
    calculate_efficiency_factor(df_design_confounded, "treatment"),
    class = "speed_efficiency_rank"
  )
})

test_that("calculate_efficiency_factor refuses unreplicated and degenerate designs", {
  # Every route to an impossible value, pinned together. Each was measured
  # returning > 1 before the rank check existed.
  unreplicated <- initialise_design_df(as.character(1:12), 4, 3)
  single_row <- data.frame(
    row = rep(1, 6),
    col = 1:6,
    treatment = rep(c("A", "B", "C"), 2)
  )
  # Column-major storage, so this is one treatment per grid row - `each = 4`
  # would give a diagonal pattern, which is estimable.
  aliased_with_row <- initialise_design_df(rep(c("a", "b", "c"), 4), 3, 4)

  for (d in list(unreplicated, single_row, aliased_with_row)) {
    expect_error(
      calculate_efficiency_factor(d, "treatment"),
      class = "speed_efficiency_rank"
    )
  }
})

test_that("calculate_efficiency_factor handles designs with dependencies between treatments", {
  # Create a larger design with subtle dependencies that increase condition number
  df_design_dependent <- data.frame(
    row = rep(1:6, each = 2),
    col = rep(1:2, times = 6),
    treatment = c(
      # Create patterns that introduce dependencies but not perfect singularity
      "A",
      "B",
      "B",
      "A",
      "C",
      "A",
      "B",
      "C",
      "A",
      "C",
      "B",
      "C"
    )
  )

  # This design should have dependencies without being perfectly singular
  expect_no_error({
    result <- calculate_efficiency_factor(df_design_dependent, "treatment")
  })

  result <- calculate_efficiency_factor(df_design_dependent, "treatment")
  expect_type(result, "double")
  expect_true(is.finite(result))
  expect_gt(result, 0)
})

test_that("calculate_efficiency_factor honours custom row/col column names", {
  # fmt: skip
  df <- by_row(c(
    "a", "b", "d", "c",
    "e", "a", "f", "b",
    "c", "f", "e", "d"
  ), 3, 4)
  base <- calculate_efficiency_factor(df, "treatment")

  # Same design, grid columns renamed - must give the same efficiency.
  df2 <- df
  names(df2)[names(df2) == "row"] <- "Row"
  names(df2)[names(df2) == "col"] <- "Column"
  custom <- calculate_efficiency_factor(
    df2,
    "treatment",
    row_column = "Row",
    col_column = "Column"
  )
  expect_equal(custom, base)
})
