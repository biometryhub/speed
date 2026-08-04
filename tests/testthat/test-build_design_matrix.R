# build_design_matrix() places treatments by their (row, col) coordinates rather
# than assuming a data frame ordering. These tests pin that it is correct for
# *both* orderings the package produces: speed() sorts row-major, while
# initialise_design_df() emits column-major.

test_that("row-major and column-major frames give the same grid", {
  # Same physical 2x3 layout, described two different ways.
  #   col 1  col 2  col 3
  # row 1: a      c      b
  # row 2: b      a      c
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

  expected <- matrix(
    c("a", "b", "c", "a", "b", "c"),
    nrow = 2,
    ncol = 3
  )
  expect_equal(build_design_matrix(row_major, "trt"), expected)
  expect_equal(build_design_matrix(col_major, "trt"), expected)
})

test_that("shuffled row order does not change the grid", {
  df <- data.frame(
    row = rep(1:3, each = 4),
    col = rep(1:4, times = 3),
    trt = LETTERS[1:12]
  )
  expect_equal(
    build_design_matrix(df[sample.int(12), ], "trt"),
    build_design_matrix(df, "trt")
  )
})

test_that("non-square grids keep their orientation", {
  df <- data.frame(
    row = c(1, 1, 2, 2, 3, 3),
    col = c(1, 2, 1, 2, 1, 2),
    trt = c("a", "b", "c", "d", "e", "f")
  )
  m <- build_design_matrix(df, "trt")

  expect_equal(dim(m), c(3L, 2L))
  expect_equal(m[3, 1], "e")
  expect_equal(m[1, 2], "b")
})

test_that("factor coordinates with lexical level order are handled", {
  # as.factor() on characters orders levels 1, 10, 11, 2, ... A fill that
  # relied on order() following the levels would build a permuted grid.
  df <- data.frame(
    row = factor(as.character(1:11)),
    col = factor(rep("1", 11)),
    trt = LETTERS[1:11]
  )
  m <- build_design_matrix(df, "trt")

  expect_equal(dim(m), c(11L, 1L))
  expect_equal(as.vector(m), LETTERS[1:11])
})

test_that("gaps in the coordinates become NA cells, not a collapsed grid", {
  # Rows 1, 2, 4, 5 exist; row 3 does not (e.g. a road). Plots in rows 2 and 4
  # must not become neighbours.
  df <- data.frame(
    row = rep(c(1, 2, 4, 5), each = 2),
    col = rep(1:2, times = 4),
    trt = c("A", "B", "C", "C", "C", "C", "A", "B")
  )
  m <- build_design_matrix(df, "trt")

  expect_equal(dim(m), c(5L, 2L))
  expect_true(all(is.na(m[3, ])))
  expect_equal(m[2, ], c("C", "C"))
  expect_equal(m[4, ], c("C", "C"))
})

test_that("coordinates that do not start at 1 are preserved, not shifted", {
  # add_buffers() offsets the real design's coordinates; dropping the buffer
  # rows leaves them starting above 1.
  df <- data.frame(
    row = c(2, 2, 3, 3),
    col = c(2, 3, 2, 3),
    trt = c("A", "B", "B", "A")
  )
  m <- build_design_matrix(df, "trt")

  expect_equal(dim(m), c(3L, 3L))
  expect_true(all(is.na(m[1, ])))
  expect_true(all(is.na(m[, 1])))
  expect_equal(m[2, 2], "A")
})

test_that("NA treatments are placed as NA", {
  df <- data.frame(
    row = c(1, 1, 2, 2),
    col = c(1, 2, 1, 2),
    trt = c("A", NA, "B", "A")
  )
  expect_true(is.na(build_design_matrix(df, "trt")[1, 2]))
})

test_that("non-numeric coordinates give an informative error", {
  df <- data.frame(
    row = c("R1", "R1"),
    col = c("C1", "C2"),
    trt = c("A", "B")
  )
  expect_error(
    build_design_matrix(df, "trt"),
    "must be numeric, or coercible to numeric"
  )
})

test_that("non-positive or fractional coordinates give an informative error", {
  zero_based <- data.frame(
    row = c(0, 0, 1, 1),
    col = c(1, 2, 1, 2),
    trt = c("A", "B", "B", "A")
  )
  expect_error(
    build_design_matrix(zero_based, "trt"),
    "positive whole numbers"
  )

  fractional <- data.frame(
    row = c(1, 1, 1.5, 1.5),
    col = c(1, 2, 1, 2),
    trt = c("A", "B", "B", "A")
  )
  expect_error(
    build_design_matrix(fractional, "trt"),
    "positive whole numbers"
  )
})

test_that("duplicate coordinates error rather than silently overwriting", {
  # Multi-site designs reuse row/col per site. Placing them on one grid would
  # keep only the last site written.
  met <- data.frame(
    site = rep(c("s1", "s2"), each = 4),
    row = rep(c(1, 1, 2, 2), times = 2),
    col = rep(c(1, 2, 1, 2), times = 2),
    trt = c("A", "B", "B", "A", "A", "A", "B", "B")
  )
  expect_error(
    build_design_matrix(met, "trt"),
    "Duplicate \\(row, col\\) coordinates"
  )
})

test_that("column names are reported in errors", {
  df <- data.frame(
    range = c("R1", "R1"),
    bed = c("C1", "C2"),
    variety = c("A", "B")
  )
  expect_error(
    build_design_matrix(
      df,
      "variety",
      row_column = "range",
      col_column = "bed"
    ),
    "`range` and `bed`"
  )
})
