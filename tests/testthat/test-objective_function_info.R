# tests/testthat/test-objective_function_info.R

# ---- objective_function_info

# all RCBDs are equally optimal
test_that("all valid RCBD randomisations produce the same score under identity covariance", {
  set.seed(1)
  n_treatments <- 4
  n_blocks <- 3

  df <- data.frame(
    row       = rep(1:n_blocks, each = n_treatments),
    col       = rep(1:n_treatments, times = n_blocks),
    block     = rep(1:n_blocks, each = n_treatments),
    treatment = NA_character_
  )

  scores <- numeric(20)
  for (i in seq_len(20)) {
    for (b in 1:n_blocks) {
      idx <- df$block == b
      df$treatment[idx] <- paste0("T", sample(n_treatments))
    }
    result <- objective_function_info(
      df, swap = "treatment", spatial_cols = c("row", "col"),
      criterion = "A"
    )
    scores[i] <- result$score
  }

  expect_true(max(scores) - min(scores) < 1e-10)
})

# when there IS spatial correlation, two different arrangements of the same
# treatments must give different scores. df1 should be worse, since treatments
# are always in the same column position, but in df2 they're "randomised".
test_that("spatial designs differentiate between arrangements", {
  n_treatments <- 4
  n_blocks <- 3

  df <- data.frame(
    row   = rep(1:n_blocks, each = n_treatments),
    col   = rep(1:n_treatments, times = n_blocks),
    block = rep(1:n_blocks, each = n_treatments)
  )

  Sigma <- cor_ar1_ar1(n_blocks, n_treatments, rho_row = 0.7, rho_col = 0.5)
  L <- compute_L_projection(df, Sigma, block_col = "block")

  # Same treatment ordering in every block
  df1 <- df
  df1$treatment <- rep(paste0("T", 1:n_treatments), n_blocks)

  # Alternated ordering
  df2 <- df
  df2$treatment <- c(
    paste0("T", c(1, 2, 3, 4)),
    paste0("T", c(3, 4, 1, 2)),
    paste0("T", c(4, 3, 2, 1))
  )

  s1 <- objective_function_info(
    df1, "treatment", c("row", "col"),
    criterion = "A", L_matrix = L
  )$score

  s2 <- objective_function_info(
    df2, "treatment", c("row", "col"),
    criterion = "A", L_matrix = L
  )$score

  expect_false(abs(s1 - s2) < 1e-10)
})

# just make sure this works lel
test_that("D-optimality criterion works", {
  df <- data.frame(
    row       = rep(1:3, each = 4),
    col       = rep(1:4, times = 3),
    block     = rep(1:3, each = 4),
    treatment = rep(paste0("T", 1:4), 3)
  )

  result <- objective_function_info(
    df, "treatment", c("row", "col"),
    criterion = "D"
  )

  expect_true(is.finite(result$score))
  expect_equal(result$criterion, "D")
  expect_equal(length(result$eigenvalues), 3)  # v - 1
})

# if treatments never exist in the same block together, you cant compare them,
# so the design is fully broken
test_that("disconnected designs have massive penalty", {
  df <- data.frame(
    row       = 1:6,
    col       = c(1, 2, 1, 2, 1, 2),
    block     = c(1, 1, 2, 2, 3, 3),
    treatment = c("A", "A", "B", "B", "C", "C")
  )

  result <- objective_function_info(
    df, "treatment", c("row", "col"),
    criterion = "A"
  )

  expect_gte(result$score, 1e9)
  expect_true(is.finite(result$score))
})


# ---- compute_L_projection

# input validation test
test_that("compute_L_projection validates inputs", {
  df <- data.frame(
    row = 1:4, col = 1:4, block = c(1, 1, 2, 2),
    treatment = c("A", "B", "A", "B")
  )

  # Wrong dimensions
  expect_error(compute_L_projection(df, diag(3)))

  # Not symmetric
  expect_error(compute_L_projection(df, matrix(1:16, 4, 4)))

  # Missing block column
  expect_error(compute_L_projection(df, diag(4), block_col = "nonexistent"))
})

# identity sigma should be the default L projection. i.e. spatial optimisation
# with identity correlation is the same as non-spatial
test_that("L with identity Sigma matches .build_L_from_df", {
  df <- data.frame(
    row       = rep(1:3, each = 4),
    col       = rep(1:4, times = 3),
    block     = rep(1:3, each = 4),
    treatment = rep(paste0("T", 1:4), 3)
  )

  L_spatial <- compute_L_projection(df, diag(12), block_col = "block")
  L_direct  <- .build_L_from_df(df, "block", 12)

  expect_true(max(abs(L_spatial - L_direct)) < 1e-10)
})


# ---- calc_info_matrix

# balanced RCBD should have rank = v - 1
test_that("info matrix for RCBD has correct properties", {
  df <- data.frame(
    row       = rep(1:3, each = 5),
    col       = rep(1:5, times = 3),
    block     = rep(1:3, each = 5),
    treatment = rep(paste0("T", 1:5), 3)
  )

  info <- calc_info_matrix(df)

  # v = 5, so rank = v - 1 = 4
  expect_equal(info$rank, 4)

  # For balanced RCBD all non-zero eigenvalues = b = 3
  expect_true(all(abs(info$eigenvalues - 3) < 1e-10))

  # 5x5 matrix
  expect_equal(dim(info$info_matrix), c(5, 5))

  # Named
  expect_equal(rownames(info$info_matrix), paste0("T", 1:5))
  expect_equal(colnames(info$info_matrix), paste0("T", 1:5))
})


# spatial should always be different to non-spatial
test_that("spatial info matrix differs from non-spatial", {
  df <- data.frame(
    row       = rep(1:3, each = 4),
    col       = rep(1:4, times = 3),
    block     = rep(1:3, each = 4),
    treatment = rep(paste0("T", 1:4), 3)
  )

  Sigma <- cor_ar1_ar1(3, 4, 0.5, 0.3)
  L <- compute_L_projection(df, Sigma)

  info_plain   <- calc_info_matrix(df)
  info_spatial <- calc_info_matrix(df, L_matrix = L)

  expect_false(
    all(abs(info_plain$eigenvalues - info_spatial$eigenvalues) < 1e-10)
  )
})

# disconnected design should give invalid criteria
test_that("disconnected design gives Inf for A and D values", {
  df <- data.frame(
    row       = 1:6,
    col       = c(1, 2, 1, 2, 1, 2),
    block     = c(1, 1, 2, 2, 3, 3),
    treatment = c("A", "A", "B", "B", "C", "C")
  )

  info <- calc_info_matrix(df)

  expect_equal(info$A_value, Inf)
  expect_equal(info$D_value, Inf)
})


# ---- calc_incidence_matrix

# complete design should have all incidence matrix entries = 1
# should also encode replication (rows) and block size (cols)
test_that("incidence matrix has correct dimensions and values for RCBD", {
  df <- data.frame(
    row       = rep(1:3, each = 4),
    col       = rep(1:4, times = 3),
    block     = rep(1:3, each = 4),
    treatment = rep(paste0("T", 1:4), 3)
  )

  N <- calc_incidence_matrix(df)

  expect_equal(dim(N), c(4, 3))        # v x b
  expect_true(all(N == 1))             # complete blocks
  expect_true(all(rowSums(N) == 3))    # replication = b
  expect_true(all(colSums(N) == 4))    # block size = v
})

# ensure incidence works fine for imbalanced designs
test_that("incidence matrix handles unequal replication", {
  df <- data.frame(
    row       = 1:5,
    col       = c(1, 2, 1, 2, 1),
    block     = c(1, 1, 2, 2, 2),
    treatment = c("A", "B", "A", "B", "A")
  )

  N <- calc_incidence_matrix(df)

  expect_equal(dim(N), c(2, 2))
  expect_equal(sum(N["A", ]), 3)   # A appears 3 times
  expect_equal(sum(N["B", ]), 2)   # B appears 2 times
})

# ---- calc_concurrence_matrix

# balanced RCBD with N all = 1, N Nt should have entries equal to block number
test_that("concurrence matrix is correct for RCBD", {
  df <- data.frame(
    row       = rep(1:3, each = 4),
    col       = rep(1:4, times = 3),
    block     = rep(1:3, each = 4),
    treatment = rep(paste0("T", 1:4), 3)
  )

  C <- calc_concurrence_matrix(df)

  expect_equal(dim(C), c(4, 4))
  expect_true(all(C == 3))
})


# ---- calculate_efficiency_factors

# balanced RCBD has all efficiency factors = 1
test_that("efficiency factors are 1 for complete blocks", {
  df <- data.frame(
    row       = rep(1:4, each = 5),
    col       = rep(1:5, times = 4),
    block     = rep(1:4, each = 5),
    treatment = rep(paste0("T", 1:5), 4)
  )

  info <- calc_info_matrix(df)
  treatments <- factor(df$treatment)
  r_bar <- mean(table(treatments))
  eff <- info$eigenvalues / r_bar

  expect_true(all(abs(eff - 1) < 1e-10))
})


# ---- correlation constructors

# make sure ar1 builder works fine
test_that("cor_ar1 produces valid correlation matrix", {
  R <- cor_ar1(5, 0.7)

  expect_equal(dim(R), c(5, 5))
  expect_true(isSymmetric(R))
  expect_true(all(diag(R) == 1))
  expect_true(all(eigen(R, only.values = TRUE)$values > 0))

  # Check specific entries
  expect_equal(R[1, 2], 0.7)
  expect_equal(R[1, 3], 0.7^2)
  expect_equal(R[1, 5], 0.7^4)
})

# yes
test_that("cor_ar1 with rho = 0 gives identity", {
  expect_equal(cor_ar1(4, 0), diag(4))
})

# make sure ar1 x ar1 builder works fine
test_that("cor_ar1_ar1 has correct dimensions and is valid", {
  Sigma <- cor_ar1_ar1(3, 5, 0.6, 0.4)

  expect_equal(dim(Sigma), c(15, 15))
  expect_true(isSymmetric(Sigma))
  expect_true(all(diag(Sigma) == 1))
  expect_true(all(eigen(Sigma, only.values = TRUE)$values > 0))
})


# ---- Test with speed function

# Actually test by passing to speed
test_that("speed() optimises an RCBD with the info objective under AR1xAR1", {
  skip_if_not_installed("speed")

  df <- initialise_design_df(
    items = 6, nrows = 4, ncols = 6,
    block_nrows = 1, block_ncols = 6
  )

  Sigma <- cor_ar1_ar1(4, 6, rho_row = 0.6, rho_col = 0.3)
  L <- compute_L_projection(df, Sigma, block_column = "block")

  result <- speed(
    df,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ block + row + col,
    obj_function = objective_function_info,
    L_matrix = L,
    criterion = "A",
    optimise_params = optim_params(random_initialisation = TRUE),
    seed = 42,
    quiet = TRUE
  )

  expect_s3_class(result, "design")
  expect_lt(result$score, Inf)

  info <- calc_info_matrix(result$design_df, L_matrix = L)
  expect_equal(info$rank, 5)
  expect_true(is.finite(info$A_value))
})

test_that("speed() optimises a BIBD with the info objective", {
  skip_if_not_installed("speed")

  df <- initialise_design_df(
    items = 5, nrows = 3, ncols = 10,
    block_nrows = 3, block_ncols = 1
  )

  result <- speed(
    df,
    swap = "treatment",
    spatial_factors = ~ block + row + col,
    obj_function = objective_function_info,
    criterion = "A",
    optimise_params = optim_params(random_initialisation = TRUE),
    seed = 42,
    quiet = TRUE
  )

  info <- calc_info_matrix(result$design_df)
  expect_equal(info$rank, 4)
  expect_true(is.finite(info$A_value))
})
