test_that("3x3 Latin square: symmetric matrix, zero diagonal, all pairs = 4", {
  # Grid (column-major from initialise_design_df):
  #   col 1  col 2  col 3
  # row 1: A      B      C
  # row 2: B      C      A
  # row 3: C      A      B
  df <- data.frame(
    row = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    col = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    trt = c("A", "B", "C", "B", "C", "A", "C", "A", "B")
  )
  M <- calculate_pair_incidence(df, swap = "trt")

  expect_true(isSymmetric(M))
  expect_equal(diag(M), c(A = 0L, B = 0L, C = 0L))
  expect_true(all(M[upper.tri(M)] == 4L))
})

test_that("as_list rows match matrix rows", {
  df <- data.frame(
    row = c(1, 1, 2, 2),
    col = c(1, 2, 1, 2),
    trt = c("A", "B", "B", "A")
  )
  M <- calculate_pair_incidence(df, swap = "trt")
  lst <- calculate_pair_incidence(df, swap = "trt", as_list = TRUE)

  expect_equal(lst[["A"]], M["A", ])
  expect_equal(lst[["B"]], M["B", ])
})

test_that("self-adjacency recorded on diagonal", {
  # 1x2 grid with two identical treatments: one self-adjacent edge
  df <- data.frame(row = c(1, 1), col = c(1, 2), trt = c("A", "A"))
  M <- calculate_pair_incidence(df, swap = "trt")

  expect_equal(M["A", "A"], 1L)
})

test_that("checkerboard 2x2: zero diagonal, four A-B edges", {
  df <- data.frame(
    row = c(1, 1, 2, 2),
    col = c(1, 2, 1, 2),
    trt = c("A", "B", "B", "A")
  )
  M <- calculate_pair_incidence(df, swap = "trt")

  expect_equal(diag(M), c(A = 0L, B = 0L))
  expect_equal(M["A", "B"], 4L)
})

test_that("single row: only horizontal edges counted", {
  df <- data.frame(row = c(1, 1, 1), col = c(1, 2, 3), trt = c("A", "B", "A"))
  M <- calculate_pair_incidence(df, swap = "trt")

  # Horizontal edges: (A,B), (B,A) -> A-B = 2; no A-A edges
  expect_equal(M["A", "B"], 2L)
  expect_equal(M["A", "A"], 0L)
})

test_that("single column: only vertical edges counted", {
  df <- data.frame(row = c(1, 2, 3), col = c(1, 1, 1), trt = c("A", "B", "A"))
  M <- calculate_pair_incidence(df, swap = "trt")

  # Vertical edges: (A,B), (B,A) -> A-B = 2
  expect_equal(M["A", "B"], 2L)
  expect_equal(M["A", "A"], 0L)
})

test_that("NA cells are skipped", {
  # 2x2 grid with one NA cell
  df <- data.frame(
    row = c(1, 1, 2, 2),
    col = c(1, 2, 1, 2),
    trt = c("A", NA, "B", "A")
  )
  M <- calculate_pair_incidence(df, swap = "trt")

  # Edges involving the NA cell are dropped.
  # Remaining edges: horiz row2 (B,A); vert col1 (A,B); vert col2 (NA,A) dropped
  # So A-B = 2
  expect_equal(M["A", "B"], 2L)
  expect_true(!any(is.na(M)))
})

test_that("design object accepted as input", {
  df <- data.frame(
    row = c(1, 1, 2, 2),
    col = c(1, 2, 1, 2),
    trt = c("A", "B", "B", "A")
  )
  fake_design <- structure(list(design_df = df), class = "design")
  M_df <- calculate_pair_incidence(df, swap = "trt")
  M_obj <- calculate_pair_incidence(fake_design, swap = "trt")

  expect_equal(M_df, M_obj)
})

test_that("treatments never adjacent still appear as zero row/col", {
  # 1x3 grid: A at ends, B in middle — A never adjacent to itself
  df <- data.frame(row = c(1, 1, 1), col = c(1, 2, 3), trt = c("A", "B", "A"))
  M <- calculate_pair_incidence(df, swap = "trt")

  expect_true(all(c("A", "B") %in% rownames(M)))
  expect_equal(M["A", "A"], 0L)
})

test_that("missing column gives informative error", {
  df <- data.frame(row = 1:3, trt = letters[1:3])
  expect_error(
    calculate_pair_incidence(df, swap = "trt"),
    "col"
  )
})
