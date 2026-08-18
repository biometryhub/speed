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
