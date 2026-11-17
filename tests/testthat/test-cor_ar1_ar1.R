test_that("cor_ar1_ar1 produces a valid separable correlation matrix", {
  correlation <- cor_ar1_ar1(3, 5, 0.6, 0.4)

  expect_equal(dim(correlation), c(15, 15))
  expect_true(isSymmetric(correlation))
  expect_true(all(diag(correlation) == 1))
  expect_true(all(eigen(correlation, only.values = TRUE)$values > 0))
})

test_that("cor_ar1_ar1 is the Kronecker product of marginal correlations", {
  expected <- kronecker(cor_ar1(3, 0.6), cor_ar1(5, 0.4))

  expect_equal(cor_ar1_ar1(3, 5, 0.6, 0.4), expected)
})
