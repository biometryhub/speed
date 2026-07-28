# fmt: skip
design_matrix <- matrix(c(
  1, 1, 1,
  2, 2, 3,
  2, 3, 1
), ncol = 3)

# Counting both directions, the adjacencies of design_matrix are
#   (1,2) x2   (1,3) x3   (2,3) x3   and (1,1) x2, (2,2) x2 as self-pairs.
# Pair (3,3) never occurs, so it must still appear with a count of 0.
expected_nb <- c("1,2" = 2L, "1,3" = 3L, "2,3" = 3L)

test_that("calculate_nb works with pair mapping", {
  pair_mapping <- create_pair_mapping(c(design_matrix))

  nb <- calculate_nb(design_matrix, pair_mapping)
  expect_equal(nb$nb, expected_nb)
  expect_equal(nb$max_nb, 3L)
  expect_setequal(nb$max_pairs, c("1,3", "2,3"))
  expect_equal(nb$var, var(expected_nb))
  expect_equal(nb$s2, sum(expected_nb * (expected_nb - 1) / 2))
})

test_that("calculate_nb works without pair mapping", {
  nb <- calculate_nb(design_matrix)
  expect_equal(nb$nb, expected_nb)
  expect_equal(nb$max_nb, 3L)
  expect_setequal(nb$max_pairs, c("1,3", "2,3"))
  expect_equal(nb$var, var(expected_nb))
})

test_that("calculate_nb excludes self-pairs and reports them separately", {
  nb <- calculate_nb(design_matrix)

  # (1,1) x2 and (2,2) x2 are adjacencies of an item with itself
  expect_equal(nb$self_adjacencies, 4L)
  expect_false(any(grepl("^(.),\\1$", names(nb$nb))))
})

test_that("calculate_nb counts pairs that are never adjacent", {
  # Every treatment pair is tabulated, whether or not it occurs
  block <- rbind(c(1, 1, 2, 2), c(1, 1, 2, 2), c(3, 3, 4, 4), c(3, 3, 4, 4))

  nb <- calculate_nb(block)
  expect_length(nb$nb, 6L)
  expect_true(any(nb$nb == 0))
  expect_equal(sum(nb$nb == 0), 2L) # (1,4) and (2,3) never adjoin
  expect_gt(nb$var, 0)
})

test_that("calculate_nb honours the directions argument", {
  # Rows are constant, so every row adjacency is a self-pair and no distinct
  # pair is adjacent along a row at all
  rows_constant <- matrix(rep(1:3, each = 3), nrow = 3, byrow = TRUE)

  by_row <- calculate_nb(rows_constant, directions = "row")
  expect_true(all(by_row$nb == 0))
  expect_equal(by_row$self_adjacencies, 6L)

  by_col <- calculate_nb(rows_constant, directions = "col")
  expect_equal(by_col$self_adjacencies, 0L)
  expect_equal(by_col$nb[["1,2"]], 3L)
  expect_equal(by_col$nb[["1,3"]], 0L)

  both <- calculate_nb(rows_constant, directions = c("row", "col"))
  expect_equal(
    sum(both$nb) + both$self_adjacencies,
    sum(by_row$nb) +
      by_row$self_adjacencies +
      sum(by_col$nb) +
      by_col$self_adjacencies
  )
  expect_error(calculate_nb(rows_constant, directions = "diagonal"))
})

test_that("calculate_nb defaults to the shape-based direction rule", {
  wide <- matrix(c(1, 2, 3, 4, 3, 1, 4, 2), nrow = 2)
  tall <- t(wide)

  expect_equal(calculate_nb(wide)$nb, calculate_nb(wide, directions = "row")$nb)
  expect_equal(calculate_nb(tall)$nb, calculate_nb(tall, directions = "col")$nb)

  # A wide layout and its transpose describe the same neighbourhoods, so the
  # rule must give them the same neighbour balance
  expect_equal(calculate_nb(wide)$nb, calculate_nb(tall)$nb)

  # Square layouts count both directions
  square <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_equal(
    calculate_nb(square)$nb,
    calculate_nb(square, directions = c("row", "col"))$nb
  )
})

test_that("calculate_nb scores a two-item design as balanced", {
  # Only one distinct pair exists, and var() of a length-1 vector is NA, which
  # would propagate an NA score into the acceptance test in speed()
  nb <- calculate_nb(matrix(c("a", "b", "b", "a"), nrow = 2))

  expect_length(nb$nb, 1)
  expect_equal(nb$var, 0)
  expect_true(is.finite(nb$var))
})

test_that("calculate_nb rejects a pair_mapping that misses items", {
  expect_error(
    calculate_nb(design_matrix, create_pair_mapping(c(1, 2))),
    "does not cover every item"
  )
})
