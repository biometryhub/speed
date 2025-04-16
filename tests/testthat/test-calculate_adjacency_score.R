test_that("calculate_adjacency_score works as expected", {
  # 1 2
  # 1 2
  design <- matrix(c(1, 1, 2, 2), nrow = 2, ncol = 2)
  expect_equal(calculate_adjacency_score(design), 2)

  # 1 1
  # 2 3
  design <- matrix(c(1, 2, 1, 3), nrow = 2, ncol = 2)
  expect_equal(calculate_adjacency_score(design), 1)

  # 1 1 1
  # 2 3 3
  # 2 3 3
  design <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
  expect_equal(calculate_adjacency_score(design), 7)

  # 1 3 2
  # 2 1 3
  # 3 2 1
  design <- matrix(c(1, 2, 3, 3, 1, 2, 2, 3, 1), nrow = 3, ncol = 3)
  expect_equal(calculate_adjacency_score(design), 0)
})
