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

# test_that("pseudo_inverse handles singular matrices", {
#   # Test with a rank-deficient matrix (singular)
#   singular_matrix <- matrix(c(1, 2, 2, 4), nrow = 2, ncol = 2)
#
#   # This should not throw an error but handle the rank deficiency
#   result <- pseudo_inverse(singular_matrix)
#   expect_true(is.matrix(result))
#   expect_equal(dim(result), c(2, 2))
# })

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
  d <- c(1, 1e-12) # Very small second singular value
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

test_that("infer_row_col can infer row and column", {
  expect_message(
    inferred <- infer_row_col(data.frame(
      row = rep(1:4, each = 5),
      col = rep(1:4, times = 5)
    )),
    "row and col are used as row and column, respectively"
  )

  expect_equal(inferred$inferred, TRUE)
  expect_equal(inferred$col, "col")
  expect_equal(inferred$row, "row")

  inferred <- infer_row_col(
    data.frame(
      Row = rep(1:4, each = 5),
      column = rep(1:4, times = 5)
    ),
    quiet = TRUE
  )

  expect_equal(inferred$inferred, TRUE)
  expect_equal(inferred$col, "column")
  expect_equal(inferred$row, "Row")

  inferred <- infer_row_col(
    data.frame(
      Rows = rep(1:4, each = 5),
      range = rep(1:4, times = 5)
    ),
    quiet = TRUE
  )

  expect_equal(inferred$inferred, TRUE)
  expect_equal(inferred$col, "range")
  expect_equal(inferred$row, "Rows")

  inferred <- infer_row_col(
    data.frame(
      lane = rep(1:4, each = 5),
      position = rep(1:4, times = 5)
    ),
    grid_factors = list(dim1 = "lane", dim2 = "position"),
    quiet = TRUE
  )

  expect_equal(inferred$inferred, TRUE)
  expect_equal(inferred$row, "lane")
  expect_equal(inferred$col, "position")
})

test_that("infer_row_col raise warning if cannot infer", {
  expect_warning(
    inferred <- infer_row_col(data.frame(row = rep(1:4, each = 5))),
    paste0(
      "Cannot infer column in the design data frame. speed.adj_weight is set to 0 for this call. If this is",
      " not intended, provide `grid_factors` argument."
    )
  )

  expect_equal(inferred$inferred, FALSE)

  expect_warning(
    inferred <- infer_row_col(data.frame(col = rep(1:4, each = 5))),
    paste0(
      "Cannot infer row in the design data frame. speed.adj_weight is set to 0 for this call. If this is not",
      " intended, provide `grid_factors` argument."
    )
  )

  expect_equal(inferred$inferred, FALSE)
})

test_that("to_factor converts data frame data to factors", {
  test_data <- data.frame(
    factor_col = as.factor(1:5),
    character_col = LETTERS[1:5],
    integer_col = 1:5,
    numeric_col = as.numeric(1:5),
    logical_col = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )
  factored <- to_factor(test_data)

  expect_setequal(names(factored), c("df", "input_types"))
  expect_setequal(names(factored$df), names(test_data))
  expect_setequal(names(factored$input_types), names(test_data))
  expect_setequal(unique(sapply(factored$df, class)), "factor")
  expect_equal(factored$df$factor_col, test_data$factor_col)
  expect_equal(factored$df$character_col, as.factor(test_data$character_col))
  expect_equal(factored$df$integer_col, as.factor(test_data$integer_col))
  expect_equal(factored$df$numeric_col, as.factor(test_data$numeric_col))
  expect_equal(factored$df$logical_col, as.factor(test_data$logical_col))
})

test_that("to_types converts data frame data to input types", {
  test_data <- data.frame(
    factor_col = as.factor(1:5),
    character_col = LETTERS[1:5],
    integer_col = 1:5,
    numeric_col = as.numeric(1:5),
    logical_col = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )
  factored <- to_factor(test_data)

  typed_data <- to_types(factored$df, factored$input_types)

  expect_setequal(names(typed_data), names(test_data))
  expect_equal(typed_data$factor_col, test_data$factor_col)
  expect_equal(typed_data$character_col, test_data$character_col)
  expect_equal(typed_data$integer_col, test_data$integer_col)
  expect_equal(typed_data$numeric_col, test_data$numeric_col)
  expect_equal(typed_data$logical_col, test_data$logical_col)
})

# test_that("parse_swap_formula parses with defaults", {
#   swap <- ~ single(treatment)
#   parsed <- parse_swap_formula(swap)
#
#   expect_equal(parsed, list(
#     "single treatment within whole design" = list("single", "treatment", c("row", "col"), "1")
#   ))
# })

# test_that("parse_swap_formula parses with multiple terms", {
#   swap <- ~ single(treatment) + all(sub_treatment, a_row + a_col, block) + single(z, a + b + c, d)
#   parsed <- parse_swap_formula(swap)
#
#   expect_equal(parsed, list(
#     "single treatment within whole design" = list("single", "treatment", c("row", "col"), "1"),
#     "all sub_treatment within block" = list("all", "sub_treatment", c("a_row", "a_col"), "block"),
#     "single z within d" = list("single", "z", c("a", "b", "c"), "d")
#   ))
# })

test_that("create_speed_input creates an input from a named list", {
  speed_input <- create_speed_input(
    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
    swap_within = list(wp = "block", sp = "wholeplot"),
    spatial_factors = ~ row + col,
    grid_factors = list(dim1 = "row", dim2 = "col"),
    iterations = 10000,
    early_stop_iterations = list(wp = 1000, sp = 10000),
    obj_function = objective_function,
    swap_all = TRUE
  )

  expect_equal(speed_input, list(
    wp = list(
      swap = "wholeplot_treatment",
      swap_within = "block",
      spatial_factors = ~ row + col,
      grid_factors = list(dim1 = "row", dim2 = "col"),
      iterations = 10000,
      early_stop_iterations = 1000,
      obj_function = objective_function,
      swap_all = TRUE
    ),
    sp = list(
      swap = "subplot_treatment",
      swap_within = "wholeplot",
      spatial_factors = ~ row + col,
      grid_factors = list(dim1 = "row", dim2 = "col"),
      iterations = 10000,
      early_stop_iterations = 10000,
      obj_function = objective_function,
      swap_all = TRUE
    )
  ))
})

test_that("create_speed_input creates an input from a string", {
  speed_input <- create_speed_input(
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col,
    grid_factors = list(dim1 = "row", dim2 = "col"),
    iterations = 10000,
    early_stop_iterations = 1000,
    obj_function = objective_function,
    swap_all = FALSE
  )

  expect_equal(speed_input, list(
    "single treatment within block" = list(
      swap = "treatment",
      swap_within = "block",
      spatial_factors = ~ row + col,
      grid_factors = list(dim1 = "row", dim2 = "col"),
      iterations = 10000,
      early_stop_iterations = 1000,
      obj_function = objective_function,
      swap_all = FALSE
    )
  ))
})
