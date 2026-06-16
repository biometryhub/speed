test_that("position incidence returns correct row and col counts", {
  # 2x3 grid:
  #   col 1  col 2  col 3
  # row 1: A      A      B
  # row 2: A      B      B
  df <- data.frame(
    row = c(1, 1, 1, 2, 2, 2),
    col = c(1, 2, 3, 1, 2, 3),
    trt = c("A", "A", "B", "A", "B", "B")
  )
  res <- calculate_position_incidence(df, swap = "trt")

  # row counts
  expect_equal(res$row["A", "1"], 2L)
  expect_equal(res$row["A", "2"], 1L)
  expect_equal(res$row["B", "1"], 1L)
  expect_equal(res$row["B", "2"], 2L)

  # col counts
  expect_equal(res$col["A", "1"], 2L)
  expect_equal(res$col["A", "2"], 1L)
  expect_equal(res$col["A", "3"], 0L)
  expect_equal(res$col["B", "1"], 0L)
  expect_equal(res$col["B", "2"], 1L)
  expect_equal(res$col["B", "3"], 2L)
})

test_that("returns list with row and col matrices", {
  df <- data.frame(
    row = c(1, 1, 2, 2),
    col = c(1, 2, 1, 2),
    trt = c("A", "B", "A", "B")
  )
  res <- calculate_position_incidence(df, swap = "trt")

  expect_type(res, "list")
  expect_named(res, c("row", "col"))
  expect_true(is.matrix(res$row))
  expect_true(is.matrix(res$col))
})

test_that("treatments as rows, positions as columns", {
  df <- data.frame(
    row = c(1, 1, 2, 2),
    col = c(1, 2, 1, 2),
    trt = c("A", "B", "A", "B")
  )
  res <- calculate_position_incidence(df, swap = "trt")

  expect_equal(rownames(res$row), c("A", "B"))
  expect_equal(colnames(res$row), c("1", "2"))
  expect_equal(rownames(res$col), c("A", "B"))
  expect_equal(colnames(res$col), c("1", "2"))
})

test_that("balanced design has equal counts everywhere", {
  # Each treatment appears once per row and once per column (Latin square)
  df <- data.frame(
    row = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    col = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    trt = c("A", "B", "C", "B", "C", "A", "C", "A", "B")
  )
  res <- calculate_position_incidence(df, swap = "trt")

  expect_true(all(res$row == 1L))
  expect_true(all(res$col == 1L))
})

test_that("NA cells excluded consistently", {
  df <- data.frame(
    row = c(1, 1, 2, 2),
    col = c(1, 2, 1, 2),
    trt = c("A", NA, "B", "A")
  )
  res <- calculate_position_incidence(df, swap = "trt")

  # NA treatment is dropped; row/col counts reflect only non-NA cells
  expect_equal(res$row["A", "1"], 1L)
  expect_equal(res$row["A", "2"], 1L)
  expect_equal(res$row["B", "1"], 0L)
  expect_equal(res$row["B", "2"], 1L)
})

test_that("design object accepted as input", {
  df <- data.frame(
    row = c(1, 1, 2, 2),
    col = c(1, 2, 1, 2),
    trt = c("A", "B", "A", "B")
  )
  fake_design <- structure(list(design_df = df), class = "design")
  res_df <- calculate_position_incidence(df, swap = "trt")
  res_obj <- calculate_position_incidence(fake_design, swap = "trt")

  expect_equal(res_df, res_obj)
})

test_that("missing column gives informative error", {
  df <- data.frame(row = 1:3, trt = letters[1:3])
  expect_error(
    calculate_position_incidence(df, swap = "trt"),
    "col"
  )
})
