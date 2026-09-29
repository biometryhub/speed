test_that("cor_ar1 produces a valid correlation matrix", {
  correlation <- cor_ar1(5, 0.7)

  expect_equal(dim(correlation), c(5, 5))
  expect_true(isSymmetric(correlation))
  expect_true(all(diag(correlation) == 1))
  expect_true(all(eigen(correlation, only.values = TRUE)$values > 0))
  expect_equal(correlation[1, 2], 0.7)
  expect_equal(correlation[1, 3], 0.7^2)
  expect_equal(correlation[1, 5], 0.7^4)
})

test_that("cor_ar1 with zero correlation gives identity", {
  expect_equal(cor_ar1(4, 0), diag(4))
})
