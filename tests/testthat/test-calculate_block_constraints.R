test_that("check_block_constraints works as expected", {
  # no blocks
  # 1 1 1
  # 2 3 3
  # 2 3 3
  design <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
  expect_equal(check_block_constraints(design, NULL, c(1, 2, 3)), TRUE)

  # 1 1 1
  # 2 3 3
  # 2 3 3
  design <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
  blocks <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3), nrow = 3, ncol = 3)
  expect_equal(check_block_constraints(design, blocks, c(1, 2, 3)), FALSE)

  # 1 3 2
  # 2 1 3
  # 3 2 1
  design <- matrix(c(1, 2, 3, 3, 1, 2, 2, 3, 1), nrow = 3, ncol = 3)
  blocks <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3), nrow = 3, ncol = 3)
  expect_equal(check_block_constraints(design, blocks, c(1, 2, 3)), TRUE)
})
