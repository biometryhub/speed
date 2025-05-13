test_that("calculate_adjacency_score works as expected", {
  # 1 2
  # 1 2
  # fmt: skip
  design <- data.frame(row = rep(1:3, times = 3),
                       col = rep(1:3, each = 3),
                       swap = rep(1:3, each = 3))
  expect_equal(calculate_adjacency_score(design, "swap"), 6)

  # 1 1
  # 2 3
  design$swap <- c(1, 2, 1, 1, 3, 2, 1, 2, 3)
  expect_equal(calculate_adjacency_score(design, "swap"), 2)

  # 1 1 1
  # 2 3 3
  # 2 3 3
  design$swap <- c(1, 2, 2, 1, 3, 3, 1, 3, 3)
  expect_equal(calculate_adjacency_score(design, "swap"), 7)

  # 1 3 2
  # 2 1 3
  # 3 2 1
  design$swap <- c(1, 2, 3, 3, 1, 2, 2, 3, 1)
  expect_equal(calculate_adjacency_score(design, "swap"), 0)
})
