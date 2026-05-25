# Tests for ar1_vinv() and ar1_ar1_vinv() helper functions

# --------------------------------------------------------------------------
# ar1_vinv
# --------------------------------------------------------------------------

test_that("ar1_vinv returns a 1x1 identity matrix for n=1", {
  result <- ar1_vinv(1, 0.5)
  expect_equal(result, matrix(1, 1, 1))
})

test_that("ar1_vinv(3, 0.5) matches known analytic form", {
  # For rho = 0.5, 1-rho^2 = 0.75
  # corner = 1/0.75 = 4/3
  # interior = (1+0.25)/0.75 = 5/3
  # off-diag = -0.5/0.75 = -2/3
  expected <- matrix(c(
     4/3, -2/3,    0,
    -2/3,  5/3, -2/3,
       0, -2/3,  4/3
  ), nrow = 3, byrow = TRUE)

  result <- ar1_vinv(3, 0.5)
  expect_equal(result, expected, tolerance = 1e-12)
})

test_that("ar1_vinv is symmetric", {
  result <- ar1_vinv(5, 0.3)
  expect_equal(result, t(result))
})

test_that("ar1_vinv is the inverse of the AR1 correlation matrix", {
  rho <- 0.6
  n   <- 4
  # Build AR1 correlation matrix
  Sigma <- outer(seq_len(n), seq_len(n), function(i, j) rho^abs(i - j))
  Vinv  <- ar1_vinv(n, rho)

  # Vinv %*% Sigma should be identity (to numerical tolerance)
  expect_equal(Vinv %*% Sigma, diag(n), tolerance = 1e-10)
})

test_that("ar1_vinv is positive definite for valid rho", {
  result <- ar1_vinv(6, 0.4)
  eigenvalues <- eigen(result, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigenvalues > 0))
})

test_that("ar1_vinv errors on |rho| >= 1", {
  expect_error(ar1_vinv(3, 1),   "rho")
  expect_error(ar1_vinv(3, -1),  "rho")
  expect_error(ar1_vinv(3, 1.1), "rho")
})

test_that("ar1_vinv errors on non-positive n", {
  expect_error(ar1_vinv(0, 0.5), "positive integer")
  expect_error(ar1_vinv(-1, 0.3), "positive integer")
})

test_that("ar1_vinv(2, rho) has correct form", {
  rho <- 0.7
  denom <- 1 - rho^2
  expected <- matrix(c(1/denom, -rho/denom, -rho/denom, 1/denom), 2, 2)
  expect_equal(ar1_vinv(2, rho), expected, tolerance = 1e-12)
})

# --------------------------------------------------------------------------
# ar1_ar1_vinv
# --------------------------------------------------------------------------

test_that("ar1_ar1_vinv returns a matrix of correct dimensions", {
  layout_df <- data.frame(
    row = rep(1:3, each = 4),
    col = rep(1:4, times = 3)
  )
  result <- ar1_ar1_vinv(layout_df, rho_r = 0.4, rho_c = 0.3)
  expect_equal(dim(result), c(12L, 12L))
})

test_that("ar1_ar1_vinv equals kronecker(ar1_vinv(nrows,rho_r), ar1_vinv(ncols,rho_c))", {
  layout_df <- data.frame(
    row = rep(1:3, each = 4),
    col = rep(1:4, times = 3)
  )
  rho_r <- 0.5
  rho_c <- 0.3

  result   <- ar1_ar1_vinv(layout_df, rho_r, rho_c)
  expected <- kronecker(ar1_vinv(3, rho_r), ar1_vinv(4, rho_c))

  expect_equal(result, expected, tolerance = 1e-12)
})

test_that("ar1_ar1_vinv is symmetric", {
  layout_df <- data.frame(row = rep(1:4, each = 3), col = rep(1:3, times = 4))
  result <- ar1_ar1_vinv(layout_df, rho_r = 0.3, rho_c = 0.6)
  expect_equal(result, t(result))
})

test_that("ar1_ar1_vinv is positive definite", {
  layout_df <- data.frame(row = rep(1:3, each = 3), col = rep(1:3, times = 3))
  result <- ar1_ar1_vinv(layout_df, rho_r = 0.4, rho_c = 0.4)
  eigenvalues <- eigen(result, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigenvalues > 0))
})

test_that("ar1_ar1_vinv with rho_r=0 and rho_c=0 gives identity", {
  layout_df <- data.frame(row = rep(1:3, each = 3), col = rep(1:3, times = 3))
  result <- ar1_ar1_vinv(layout_df, rho_r = 0, rho_c = 0)
  expect_equal(result, diag(9), tolerance = 1e-12)
})

test_that("ar1_ar1_vinv is inverse of full AR1xAR1 covariance", {
  rho_r <- 0.4
  rho_c <- 0.5
  nr <- 3
  nc <- 3
  layout_df <- data.frame(row = rep(seq_len(nr), each = nc),
                           col = rep(seq_len(nc), times = nr))

  # Build full covariance: V = Sigma_r ⊗ Sigma_c
  Sigma_r <- outer(seq_len(nr), seq_len(nr), function(i, j) rho_r^abs(i - j))
  Sigma_c <- outer(seq_len(nc), seq_len(nc), function(i, j) rho_c^abs(i - j))
  V       <- kronecker(Sigma_r, Sigma_c)

  Vinv    <- ar1_ar1_vinv(layout_df, rho_r, rho_c)
  expect_equal(Vinv %*% V, diag(nr * nc), tolerance = 1e-10)
})
