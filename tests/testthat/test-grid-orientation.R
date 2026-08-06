# Regression tests for the grid-orientation fix. Every expected value here is
# derived from the (row, col) coordinates by hand, never by reshaping the
# treatment column -- reshaping is the bug these tests exist to catch.

# --- calculate_adjacency_score() composability -------------------------------

test_that("adjacency score is the same for either input ordering", {
  # Same physical 2x3 layout described row-major and column-major.
  row_major <- data.frame(
    row = c(1, 1, 1, 2, 2, 2),
    col = c(1, 2, 3, 1, 2, 3),
    trt = c("a", "c", "b", "b", "a", "c")
  )
  col_major <- data.frame(
    row = c(1, 2, 1, 2, 1, 2),
    col = c(1, 1, 2, 2, 3, 3),
    trt = c("a", "b", "c", "a", "b", "c")
  )

  expect_equal(
    calculate_adjacency_score(row_major, "trt"),
    calculate_adjacency_score(col_major, "trt")
  )
})

test_that("calculate_adjacency_score() composes with initialise_design_df()", {
  # initialise_design_df() emits column-major data. A byrow = TRUE fill scored
  # this layout as 6 when the correct answer is 0.
  #   col 1  col 2  col 3
  # row 1: a      c      b
  # row 2: b      a      c
  df <- initialise_design_df(
    items = rep(c("a", "b", "c"), 2),
    nrows = 2,
    ncols = 3
  )
  expect_equal(calculate_adjacency_score(df, "treatment"), 0)
})

test_that("adjacency score counts like-treatment edges on a known layout", {
  # row 1: A A A
  # row 2: B B B
  # row 3: A A A
  # Horizontal like-pairs: 2 per row x 3 rows = 6. No vertical like-pairs.
  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = c("A", "A", "A", "B", "B", "B", "A", "A", "A")
  )
  expect_equal(calculate_adjacency_score(df, "treatment"), 6)
})

# --- objective_function_piepho() ---------------------------------------------

test_that("piepho score does not depend on the input row ordering", {
  # This is the property the whole change buys: the score describes the layout,
  # not the order the rows happen to be in.
  col_major <- initialise_design_df(
    items = rep(c("a", "b", "c"), 4),
    nrows = 2,
    ncols = 6
  )
  row_major <- col_major[order(col_major$row, col_major$col), ]
  rownames(row_major) <- NULL
  pm <- create_pair_mapping(col_major$treatment)

  from_col_major <- objective_function_piepho(
    col_major,
    "treatment",
    c("row", "col"),
    pair_mapping = pm
  )
  from_row_major <- objective_function_piepho(
    row_major,
    "treatment",
    c("row", "col"),
    pair_mapping = pm
  )

  expect_equal(from_row_major$score, from_col_major$score)
  expect_equal(from_row_major$components, from_col_major$components)
})

test_that("piepho components match hand-derived values on a non-square grid", {
  # 2x6 layout from initialise_design_df(rep(c("a","b","c"), 4), 2, 6):
  #   col 1  2  3  4  5  6
  # row 1:  a  c  b  a  c  b
  # row 2:  b  a  c  b  a  c
  # Every treatment appears 4 times, twice per row and never twice in a column,
  # so the balance score is the row term only: 3 treatments x var(c(2,2)) = 0
  # across columns, and rowVars over the 2 rows gives 2 in total.
  df <- initialise_design_df(
    items = rep(c("a", "b", "c"), 4),
    nrows = 2,
    ncols = 6
  )
  df <- df[order(df$row, df$col), ]
  pm <- create_pair_mapping(df$treatment)

  res <- objective_function_piepho(
    df,
    "treatment",
    c("row", "col"),
    pair_mapping = pm
  )

  # No like-treatment neighbours anywhere in this layout.
  expect_equal(res$components[["adjacency"]], 0)
  expect_equal(
    res$components[["adjacency"]],
    calculate_adjacency_score(df, "treatment")
  )
  # Balance must be computed on the real treatment column.
  expect_equal(
    res$components[["balance"]],
    calculate_balance_score(df, "treatment", c("row", "col"))
  )
})

test_that("piepho does not overwrite the treatment column it was given", {
  # The objective used to write a column-major flatten of the grid back into
  # `design`, which permuted the treatments against their coordinates before
  # the balance and adjacency components were computed.
  df <- initialise_design_df(
    items = rep(LETTERS[1:4], 6),
    nrows = 4,
    ncols = 6
  )
  df <- df[order(df$row, df$col), ]
  pm <- create_pair_mapping(df$treatment)

  res <- objective_function_piepho(
    df,
    "treatment",
    c("row", "col"),
    pair_mapping = pm
  )

  expect_equal(
    res$components[["balance"]],
    calculate_balance_score(df, "treatment", c("row", "col"))
  )
  expect_equal(
    res$components[["adjacency"]],
    calculate_adjacency_score(df, "treatment")
  )
})

# --- sparse grids ------------------------------------------------------------

test_that("neighbour balance skips empty cells, with and without a mapping", {
  # Rows 1, 2, 4, 5 exist; row 3 is a gap. Pairs, by coordinate:
  # horizontal A-B, C-C, C-C, A-B; vertical A-C, B-C (rows 1-2), A-C, B-C
  # (rows 4-5). Nothing crosses the gap.
  df <- data.frame(
    row = rep(c(1, 2, 4, 5), each = 2),
    col = rep(1:2, times = 4),
    trt = c("A", "B", "C", "C", "C", "C", "A", "B")
  )
  m <- build_design_matrix(df, "trt")

  with_mapping <- calculate_nb(m, create_pair_mapping(df$trt))
  without_mapping <- calculate_nb(m)

  expect_equal(as.integer(with_mapping$nb[["C,C"]]), 2L)
  expect_equal(as.integer(without_mapping$nb[["C,C"]]), 2L)
  # No pair may involve an empty cell.
  expect_false(any(grepl("NA", names(with_mapping$nb), fixed = TRUE)))
  expect_false(any(grepl("NA", names(without_mapping$nb), fixed = TRUE)))
})

test_that("piepho runs on a design with missing plots", {
  df <- data.frame(
    row = rep(c(1, 2, 4, 5), each = 2),
    col = rep(1:2, times = 4),
    treatment = c("A", "B", "C", "D", "D", "C", "A", "B")
  )
  # Both the default (NULL) and supplied pair_mapping paths must work.
  expect_no_error(
    objective_function_piepho(df, "treatment", c("row", "col"))
  )
  expect_no_error(
    objective_function_piepho(
      df,
      "treatment",
      c("row", "col"),
      pair_mapping = create_pair_mapping(df$treatment)
    )
  )
})

test_that("adjacency score ignores gaps rather than closing them", {
  # C-C pairs exist within rows 2 and 4 but not between them: the gap at row 3
  # must not be collapsed.
  df <- data.frame(
    row = rep(c(1, 2, 4, 5), each = 2),
    col = rep(1:2, times = 4),
    trt = c("A", "B", "C", "C", "C", "C", "A", "B")
  )
  expect_equal(calculate_adjacency_score(df, "trt"), 2)
})

# --- ring_weights recycling --------------------------------------------------

test_that("a single ring_weight applies to every ring_dist", {
  # ring_weights defaults to the scalar 1, so the documented default was
  # unusable with a multi-ring ring_dists: adjacency_score_vec() asserted the
  # two were the same length.
  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    trt = c("A", "B", "C", "B", "C", "A", "C", "A", "B")
  )

  expect_no_error(
    scalar_weight <- calculate_adjacency_score(df, "trt", ring_dists = c(1, 2))
  )
  expect_equal(
    scalar_weight,
    calculate_adjacency_score(
      df,
      "trt",
      ring_dists = c(1, 2),
      ring_weights = c(1, 1)
    )
  )
})

test_that("a genuine ring_weights length mismatch is still an error", {
  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    trt = c("A", "B", "C", "B", "C", "A", "C", "A", "B")
  )
  expect_error(
    calculate_adjacency_score(
      df,
      "trt",
      ring_dists = c(1, 2, 3),
      ring_weights = c(1, 2)
    )
  )
})

# --- calculate_efficiency_factor() -------------------------------------------

test_that("efficiency factor does not depend on the input row ordering", {
  # It used to walk a plot counter through nested row/col loops, so it assumed
  # a complete grid in row-major order and silently scored a different layout
  # for anything else - including initialise_design_df()'s column-major output.
  col_major <- initialise_design_df(
    items = rep(LETTERS[1:3], 4),
    nrows = 4,
    ncols = 3
  )
  row_major <- col_major[order(col_major$row, col_major$col), ]
  rownames(row_major) <- NULL

  expect_equal(
    calculate_efficiency_factor(col_major, treatment),
    calculate_efficiency_factor(row_major, treatment)
  )
  # Row-major was already correct, so its value must be unchanged. Column-major
  # returned 1.5 - impossible for an efficiency factor, and the symptom that
  # made this visible.
  expect_equal(calculate_efficiency_factor(row_major, treatment), 0.9375)
})

test_that("efficiency factor is unchanged by shuffling a real design", {
  d <- speed(
    initialise_design_df(items = rep(LETTERS[1:6], 4), nrows = 3, ncols = 8),
    swap = "treatment",
    iterations = 500,
    seed = 11,
    quiet = TRUE
  )
  ordered <- calculate_efficiency_factor(d$design_df, treatment)
  shuffled <- calculate_efficiency_factor(
    d$design_df[rev(seq_len(nrow(d$design_df))), ],
    treatment
  )

  expect_equal(ordered, shuffled)
  expect_lte(ordered, 1)
})
