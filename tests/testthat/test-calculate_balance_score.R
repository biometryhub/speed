test_that("calculate_balance_score works as expected", {
  # TODO: manually calculate the actual variance
  # 1 1 1
  # 2 3 3
  # 2 3 3
  design <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
  expect_gt(calculate_balance_score(design, c(1, 2, 3)), 0)

  # 1 3 2
  # 2 1 3
  # 3 2 1
  design <- matrix(c(1, 2, 3, 3, 1, 2, 2, 3, 1), nrow = 3, ncol = 3)
  expect_equal(calculate_balance_score(design, c(1, 2, 3)), 0)
})
