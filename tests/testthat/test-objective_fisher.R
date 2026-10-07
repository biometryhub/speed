# tests/testthat/test-objective_fisher.R

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
      df,
      swap = "treatment", spatial_cols = c("row", "col"),
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
    criterion = "A", Sigma = Sigma
  )$score

  s2 <- objective_function_info(
    df2, "treatment", c("row", "col"),
    criterion = "A", Sigma = Sigma
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
  expect_equal(length(result$eigenvalues), 3) # v - 1
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
  expect_equal(result$A_value, Inf)
  expect_equal(result$D_value, Inf)
})

test_that("information matrix for an RCBD has the expected rank", {
  df <- data.frame(
    row       = rep(1:3, each = 5),
    col       = rep(1:5, times = 3),
    block     = rep(1:3, each = 5),
    treatment = rep(paste0("T", 1:5), 3)
  )

  result <- objective_function_info(df, "treatment", c("row", "col"))

  expect_equal(result$rank, 4)
  expect_equal(result$eigenvalues, rep(3, 4), tolerance = 1e-10)
  expect_equal(dim(result$info_matrix), c(5, 5))
  expect_equal(rownames(result$info_matrix), paste0("T", 1:5))
  expect_equal(colnames(result$info_matrix), paste0("T", 1:5))
})

test_that("a requested estimable contrast does not require full connectedness", {
  df <- data.frame(
    row       = 1:6,
    col       = rep(1:2, 3),
    block     = rep(1:3, each = 2),
    treatment = c("A", "B", "A", "B", "C", "C")
  )
  contrast <- matrix(c(1, -1, 0), nrow = 1)
  colnames(contrast) <- c("A", "B", "C")

  full <- objective_function_info(df, "treatment", c("row", "col"))
  targeted <- objective_function_info(
    df,
    "treatment",
    c("row", "col"),
    contrast_matrix = contrast
  )

  expect_false(full$estimable)
  expect_true(targeted$estimable)
  expect_true(is.finite(targeted$score))
})

test_that("incremental calculation matches full recalculation", {
  df <- data.frame(
    row       = rep(1:3, each = 4),
    col       = rep(1:4, times = 3),
    block     = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:4], 3)
  )
  Sigma <- cor_ar1_ar1(3, 4, rho_row = 0.6, rho_col = 0.3)
  current <- objective_function_info(
    df,
    "treatment",
    c("row", "col"),
    Sigma = Sigma
  )

  proposal <- df
  proposal$treatment[c(1, 2, 7, 8)] <- proposal$treatment[c(2, 1, 8, 7)]
  incremental <- objective_function_info(
    proposal,
    "treatment",
    c("row", "col"),
    Sigma = Sigma,
    current_score_obj = current,
    swapped_items = c("A", "B", "C", "D")
  )
  full <- objective_function_info(
    proposal,
    "treatment",
    c("row", "col"),
    Sigma = Sigma
  )

  expect_equal(incremental$info_matrix, full$info_matrix, tolerance = 1e-12)
  expect_equal(incremental$score, full$score, tolerance = 1e-12)

  # Reuse the original score object as if the first proposal was rejected.
  second_proposal <- df
  second_proposal$treatment[c(3, 4)] <- second_proposal$treatment[c(4, 3)]
  second_incremental <- objective_function_info(
    second_proposal,
    "treatment",
    c("row", "col"),
    Sigma = Sigma,
    current_score_obj = current,
    swapped_items = c("C", "D")
  )
  second_full <- objective_function_info(
    second_proposal,
    "treatment",
    c("row", "col"),
    Sigma = Sigma
  )

  expect_equal(
    second_incremental$info_matrix,
    second_full$info_matrix,
    tolerance = 1e-12
  )
})

test_that("covariance matrices remain aligned when layout rows are reordered", {
  df <- data.frame(
    row       = rep(1:3, each = 4),
    col       = rep(1:4, times = 3),
    block     = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:4], 3)
  )
  Sigma <- cor_ar1_ar1(3, 4, rho_row = 0.6, rho_col = 0.3)
  original <- objective_function_info(
    df,
    "treatment",
    c("row", "col"),
    Sigma = Sigma
  )

  row_order <- c(5:12, 1:4)
  reordered <- objective_function_info(
    df[row_order, ],
    "treatment",
    c("row", "col"),
    Sigma = Sigma[row_order, row_order]
  )

  expect_equal(reordered$info_matrix, original$info_matrix, tolerance = 1e-12)
  expect_equal(reordered$score, original$score, tolerance = 1e-12)
})

test_that("Sigma and L_matrix are mutually exclusive", {
  df <- data.frame(
    row = 1:4,
    col = 1:4,
    treatment = rep(c("A", "B"), 2)
  )

  expect_error(
    objective_function_info(
      df,
      "treatment",
      c("row", "col"),
      Sigma = diag(4),
      L_matrix = diag(4)
    ),
    "Only one"
  )
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

  result <- speed(
    df,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ block + row + col,
    obj_function = objective_function_info,
    Sigma = Sigma,
    criterion = "A",
    optimise_params = optim_params(random_initialisation = TRUE),
    seed = 42,
    quiet = TRUE
  )

  expect_s3_class(result, "design")
  expect_lt(result$score, Inf)

  info <- objective_function_info(
    result$design_df,
    "treatment",
    c("row", "col"),
    Sigma = Sigma
  )
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

  info <- objective_function_info(
    result$design_df,
    "treatment",
    c("row", "col")
  )
  expect_equal(info$rank, 4)
  expect_true(is.finite(info$A_value))
})
