test_that("calculate_balance_score works as expected", {
  # TODO: manually calculate the actual variance
  # 1 1 1
  # 2 3 3
  # 2 3 3
  df <- data.frame(
    row = rep(1:3, 3),
    col = rep(1:3, each = 3),
    treatment = c(1, 2, 2, 1, 3, 3, 1, 3, 3)
  )
  expect_gt(calculate_balance_score(df, "treatment", c("row", "col")), 0)

  # 1 3 2
  # 2 1 3
  # 3 2 1
  df <- data.frame(
    row = rep(1:3, 3),
    col = rep(1:3, each = 3),
    treatment = c(1, 2, 3, 3, 1, 2, 2, 3, 1)
  )
  expect_equal(calculate_balance_score(df, "treatment", c("row", "col")), 0)
})
