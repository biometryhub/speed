test_that("compute_L_projection validates covariance inputs", {
  df <- data.frame(
    row = 1:4,
    col = 1:4,
    block = c(1, 1, 2, 2),
    treatment = c("A", "B", "A", "B")
  )

  expect_error(compute_L_projection(df, 1:4), "matrix")
  expect_error(compute_L_projection(df, diag(3)), "dimension")
  expect_error(compute_L_projection(df, matrix(1:16, 4, 4)), "symmetric")
})

test_that("identity covariance matches the direct projection", {
  df <- data.frame(
    row       = rep(1:3, each = 4),
    col       = rep(1:4, times = 3),
    block     = rep(1:3, each = 4),
    treatment = rep(paste0("T", 1:4), 3)
  )

  spatial <- compute_L_projection(df, diag(12), block_column = "block")
  direct <- .build_L_from_df(df, "block", 12)

  expect_equal(unname(spatial), unname(direct), tolerance = 1e-10)
})

test_that("compute_L_projection supports nuisance formulas", {
  df <- data.frame(
    row       = rep(1:3, each = 4),
    col       = rep(1:4, times = 3),
    treatment = rep(paste0("T", 1:4), 3)
  )

  spatial <- compute_L_projection(
    df,
    diag(12),
    nuisance_formula = ~ row + col
  )
  direct <- .build_L_from_df(
    df,
    "block",
    12,
    nuisance_formula = ~ row + col
  )

  expect_equal(unname(spatial), unname(direct), tolerance = 1e-10)
})

test_that("compute_L_projection returns a projection in grid order", {
  df <- initialise_design_df(
    items = 6, nrows = 4, ncols = 6,
    block_nrows = 1, block_ncols = 6
  )
  Sigma <- cor_ar1_ar1(4, 6, rho_row = 0.6, rho_col = 0.3)
  X2 <- kronecker(diag(4), rep(1, 6))
  Sigma_inv <- solve(Sigma)
  expected <- Sigma_inv - Sigma_inv %*% X2 %*%
    solve(crossprod(X2, Sigma_inv %*% X2)) %*% t(X2) %*% Sigma_inv

  for (layout in list(df, df[rev(seq_len(nrow(df))), ])) {
    result <- compute_L_projection(layout, Sigma)
    expect_equal(unname(result), unname(expected), tolerance = 1e-10)
    expect_equal(unname(result %*% X2), matrix(0, 24, 4), tolerance = 1e-10)
  }
})

test_that("compute_L_projection requires at least two blocks by default", {
  df <- data.frame(row = rep(1:2, each = 2), col = rep(1:2, 2))
  expect_error(compute_L_projection(df, diag(4)), "at least two blocks")
  df$block <- factor(rep(1, 4), levels = 1:2)
  expect_error(compute_L_projection(df, diag(4)), "at least two blocks")
})
