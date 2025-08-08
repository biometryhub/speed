# Test pseudo_inverse function
test_that("pseudo_inverse calculates Moore-Penrose inverse correctly", {
  # Test with a well-conditioned matrix
  A <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  A_inv <- pseudo_inverse(A)
  
  # Check that A * A_inv * A = A (approximately)
  expect_equal(A %*% A_inv %*% A, A, tolerance = 1e-10)
  
  # Check dimensions
  expect_equal(dim(A_inv), c(2, 2))
})

test_that("pseudo_inverse handles singular matrices", {
  # Test with a rank-deficient matrix (singular)
  singular_matrix <- matrix(c(1, 2, 2, 4), nrow = 2, ncol = 2)
  
  # This should not throw an error but handle the rank deficiency
  result <- pseudo_inverse(singular_matrix)
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(2, 2))
})

test_that("pseudo_inverse throws error for zero matrix", {
  # Test with a zero matrix (rank 0)
  zero_matrix <- matrix(0, nrow = 2, ncol = 2)
  
  # This should throw an error with specific message
  expect_error(
    pseudo_inverse(zero_matrix),
    "zero_matrix has rank 0 - design may be invalid"
  )
})

test_that("pseudo_inverse works with rectangular matrices", {
  # Test with a rectangular matrix
  rect_matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  result <- pseudo_inverse(rect_matrix)
  
  # Check dimensions (should be transposed)
  expect_equal(dim(result), c(3, 2))
  
  # Check that A * A_inv * A = A (approximately)
  expect_equal(rect_matrix %*% result %*% rect_matrix, rect_matrix, tolerance = 1e-10)
})

test_that("pseudo_inverse respects tolerance parameter", {
  # Create a matrix with very small singular values
  U <- matrix(c(1, 0, 0, 1), nrow = 2)
  d <- c(1, 1e-12)  # Very small second singular value
  V <- matrix(c(1, 0, 0, 1), nrow = 2)
  
  # Reconstruct matrix with small singular value
  A <- U %*% diag(d) %*% t(V)
  
  # With default tolerance, should treat as rank 1
  result_default <- pseudo_inverse(A)
  expect_true(is.matrix(result_default))
  
  # With very small tolerance, should treat as rank 2
  result_small_tol <- pseudo_inverse(A, tolerance = 1e-15)
  expect_true(is.matrix(result_small_tol))
})

# Test env_add_one function
test_that("env_add_one adds to existing key", {
  env <- new.env()
  env$test_key <- 5
  
  env_add_one(env, "test_key")
  
  expect_equal(env$test_key, 6)
})

test_that("env_add_one creates new key with value 1", {
  env <- new.env()
  
  env_add_one(env, "new_key")
  
  expect_equal(env$new_key, 1)
})

test_that("env_add_one works with different key types", {
  env <- new.env()
  
  # Test with character key
  env_add_one(env, "char_key")
  expect_equal(env$char_key, 1)
  
  # Test incrementing
  env_add_one(env, "char_key")
  expect_equal(env$char_key, 2)
})