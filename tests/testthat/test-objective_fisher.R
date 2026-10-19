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
test_that("disconnected designs have infinite scores", {
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

  expect_equal(result$score, Inf)
  expect_false(result$estimable)
  expect_equal(result$A_value, Inf)
  expect_equal(result$D_value, Inf)
})

test_that("large finite contrast variances are preferable to non-estimable targets", {
  df <- data.frame(
    row = rep(1:2, each = 4), col = rep(1:4, 2),
    block = rep(1:2, each = 4), treatment = rep(c("A", "B"), 4)
  )
  contrast <- matrix(c(1e6, -1e6), nrow = 1, dimnames = list(NULL, c("A", "B")))
  connected <- objective_function_info(
    df, "treatment", c("row", "col"), contrast_matrix = contrast
  )
  df$treatment <- rep(c("A", "B"), each = 4)
  disconnected <- objective_function_info(
    df, "treatment", c("row", "col"), contrast_matrix = contrast
  )

  expect_equal(connected$score, 5e11, tolerance = 1e-10)
  expect_true(connected$estimable)
  expect_equal(disconnected$score, Inf)
  expect_false(disconnected$estimable)
  expect_lt(connected$score, disconnected$score)
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
  df <- initialise_design_df(
    items = 6, nrows = 4, ncols = 6,
    block_nrows = 1, block_ncols = 6
  )
  Sigma <- cor_ar1_ar1(4, 6, rho_row = 0.6, rho_col = 0.3)
  sorted <- df[order(df$row, df$col), ]
  reordered <- df[rev(seq_len(nrow(df))), ]
  L <- compute_L_projection(df, Sigma)

  for (criterion in c("A", "D")) {
    original <- objective_function_info(
      sorted, "treatment", c("row", "col"),
      criterion = criterion, Sigma = Sigma
    )
    for (layout in list(df, reordered)) {
      result <- objective_function_info(
        layout, "treatment", c("row", "col"),
        criterion = criterion, Sigma = Sigma
      )
      incremental <- objective_function_info(
        layout, "treatment", c("row", "col"),
        criterion = criterion, Sigma = Sigma,
        current_score_obj = original, swapped_items = unique(df$treatment)
      )
      precomputed <- objective_function_info(
        layout, "treatment", c("row", "col"),
        criterion = criterion, L_matrix = L
      )

      expect_equal(result$info_matrix, original$info_matrix, tolerance = 1e-12)
      expect_equal(result$score, original$score, tolerance = 1e-12)
      expect_equal(incremental$info_matrix, original$info_matrix, tolerance = 1e-12)
      expect_equal(precomputed$info_matrix, original$info_matrix, tolerance = 1e-12)
    }
  }
})

test_that("spatial scoring uses numeric coordinates and custom grid columns", {
  df <- initialise_design_df(
    items = 6, nrows = 4, ncols = 6,
    block_nrows = 1, block_ncols = 6
  )
  Sigma <- cor_ar1_ar1(4, 6, rho_row = 0.6, rho_col = 0.3)
  original <- objective_function_info(df, "treatment", c("row", "col"), Sigma = Sigma)
  names(df)[names(df) == "row"] <- "x"
  names(df)[names(df) == "col"] <- "y"
  df$x <- factor(df$x, levels = 4:1)
  df$y <- factor(df$y, levels = 6:1)

  for (index in list(NULL, grid_indices(df, "x", "y"))) {
    result <- objective_function_info(
      df, "treatment", c("x", "y"), Sigma = Sigma,
      row_column = "x", col_column = "y", grid_index = index
    )
    expect_equal(result$info_matrix, original$info_matrix, tolerance = 1e-12)
  }
})

test_that("spatial scoring orders multiple grids consistently", {
  df <- initialise_design_df(
    items = 4, nrows = 3, ncols = 4,
    block_nrows = 1, block_ncols = 4
  )
  df <- rbind(transform(df, site = "A"), transform(df, site = "B"))
  df$block <- interaction(df$site, df$block, drop = TRUE)
  df$treatment[c(13, 16)] <- df$treatment[c(16, 13)]
  Sigma <- kronecker(diag(c(1, 2)), cor_ar1_ar1(3, 4, rho_row = 0.6, rho_col = 0.3))
  original <- objective_function_info(
    df, "treatment", c("row", "col"), Sigma = Sigma, by = "site"
  )
  site_a <- objective_function_info(
    df[1:12, ], "treatment", c("row", "col"), Sigma = Sigma[1:12, 1:12]
  )
  site_b <- objective_function_info(
    df[13:24, ], "treatment", c("row", "col"), Sigma = Sigma[13:24, 13:24]
  )
  expect_equal(original$info_matrix, site_a$info_matrix + site_b$info_matrix, tolerance = 1e-12)
  reordered <- df[rev(seq_len(nrow(df))), ]
  for (index in list(NULL, grid_indices(reordered, by = "site"))) {
    result <- objective_function_info(
      reordered, "treatment", c("row", "col"), Sigma = Sigma,
      by = "site", grid_index = index
    )
    expect_equal(result$info_matrix, original$info_matrix, tolerance = 1e-12)
  }

  result <- speed(
    reordered,
    swap = "treatment",
    swap_within = "block",
    grid_factors = list(dim1 = "row", dim2 = "col", by = "site"),
    obj_function = objective_function_info,
    Sigma = Sigma,
    iterations = 20,
    early_stop_iterations = 100,
    optimise_params = optim_params(random_initialisation = 2),
    seed = 1,
    quiet = TRUE
  )
  final <- objective_function_info(
    result$design_df, "treatment", c("row", "col"), Sigma = Sigma, by = "site"
  )
  expect_equal(result$score, final$score, tolerance = 1e-10)
  expect_lte(result$score, original$score)
})

test_that("spatial scoring accepts covariance for observed plots only", {
  df <- initialise_design_df(
    items = 4, nrows = 3, ncols = 4,
    block_nrows = 1, block_ncols = 4
  )
  df <- df[order(df$row, df$col), ]
  observed <- setdiff(seq_len(nrow(df)), c(2, 7))
  df <- df[observed, ]
  Sigma <- cor_ar1_ar1(3, 4, rho_row = 0.6, rho_col = 0.3)[observed, observed]
  original <- objective_function_info(df, "treatment", c("row", "col"), Sigma = Sigma)
  reordered <- objective_function_info(
    df[rev(seq_len(nrow(df))), ], "treatment", c("row", "col"), Sigma = Sigma
  )

  expect_true(original$estimable)
  expect_equal(reordered$info_matrix, original$info_matrix, tolerance = 1e-12)
  expect_equal(reordered$score, original$score, tolerance = 1e-12)
})

test_that("cached projections are rebuilt when nuisance effects change", {
  df <- initialise_design_df(
    items = 6, nrows = 4, ncols = 6,
    block_nrows = 1, block_ncols = 6
  )
  Sigma <- cor_ar1_ar1(4, 6, rho_row = 0.6, rho_col = 0.3)

  for (covariance in list(NULL, Sigma)) {
    original <- objective_function_info(df, "treatment", c("row", "col"), Sigma = covariance)
    proposal <- df
    proposal$block[c(1, 6)] <- proposal$block[c(6, 1)]
    incremental <- objective_function_info(
      proposal, "treatment", c("row", "col"), Sigma = covariance,
      current_score_obj = original, swapped_items = unique(df$treatment)
    )
    full <- objective_function_info(proposal, "treatment", c("row", "col"), Sigma = covariance)

    expect_false(isTRUE(all.equal(full$info_matrix, original$info_matrix)))
    expect_equal(incremental$info_matrix, full$info_matrix, tolerance = 1e-12)
    expect_equal(incremental$score, full$score, tolerance = 1e-12)
  }
})

test_that("formula expansion and environment changes invalidate the projection", {
  df <- initialise_design_df(
    items = 4, nrows = 3, ncols = 4,
    block_nrows = 1, block_ncols = 4
  )
  df <- df[order(df$row, df$col), ]
  df$block <- factor(df$block)
  Sigma <- cor_ar1_ar1(3, 4, 0.6, 0.3)

  for (covariance in list(NULL, Sigma)) {
    nuisance <- ~ . - treatment - row - col
    original <- objective_function_info(
      df, "treatment", c("row", "col"),
      Sigma = covariance, nuisance_formula = nuisance
    )
    proposal <- df
    proposal$block[c(1, 6)] <- proposal$block[c(6, 1)]
    cached <- objective_function_info(
      proposal, "treatment", c("row", "col"),
      Sigma = covariance, nuisance_formula = nuisance,
      current_score_obj = original, swapped_items = unique(df$treatment)
    )
    full <- objective_function_info(
      proposal, "treatment", c("row", "col"),
      Sigma = covariance, nuisance_formula = nuisance
    )
    expect_false(isTRUE(all.equal(original$info_matrix, full$info_matrix)))
    expect_equal(cached$info_matrix, full$info_matrix, tolerance = 1e-10)
    expect_equal(cached$score, full$score, tolerance = 1e-10)

    adjustment <- rep(c(0, 1, 0, 1), 3)
    nuisance <- ~ block + adjustment
    original <- objective_function_info(
      df, "treatment", c("row", "col"),
      Sigma = covariance, nuisance_formula = nuisance
    )
    adjustment <- rep(c(0, 0, 1, 1), 3)
    cached <- objective_function_info(
      df, "treatment", c("row", "col"),
      Sigma = covariance, nuisance_formula = nuisance,
      current_score_obj = original, swapped_items = unique(df$treatment)
    )
    full <- objective_function_info(
      df, "treatment", c("row", "col"),
      Sigma = covariance, nuisance_formula = nuisance
    )
    expect_false(isTRUE(all.equal(original$info_matrix, full$info_matrix)))
    expect_equal(cached$info_matrix, full$info_matrix, tolerance = 1e-10)
  }
})

test_that("the default nuisance model requires at least two blocks", {
  df <- data.frame(treatment = rep(c("A", "B"), 2))
  expect_error(objective_function_info(df, "treatment", character()), "at least two blocks")
  df$block <- factor(rep(1, 4), levels = 1:2)
  expect_error(objective_function_info(df, "treatment", character()), "at least two blocks")
  df$block <- rep(NA_integer_, 4)
  expect_error(objective_function_info(df, "treatment", character()), "at least two blocks")
  df$block <- c(1, 1, 2, NA)
  expect_error(objective_function_info(df, "treatment", character()), "missing")
})

test_that("explicit nuisance models can replace the block model", {
  df <- initialise_design_df(
    items = 4, nrows = 3, ncols = 4,
    block_nrows = 1, block_ncols = 4
  )
  Sigma <- cor_ar1_ar1(3, 4, rho_row = 0.6, rho_col = 0.3)
  original <- objective_function_info(df, "treatment", c("row", "col"), Sigma = Sigma)
  L <- compute_L_projection(df, Sigma)

  for (blocks in list(NULL, rep(1, nrow(df)))) {
    df$block <- blocks
    custom <- objective_function_info(
      df, "treatment", c("row", "col"), Sigma = Sigma,
      nuisance_formula = ~ factor(row)
    )
    precomputed <- objective_function_info(
      df, "treatment", c("row", "col"), L_matrix = L
    )
    expect_equal(custom$info_matrix, original$info_matrix, tolerance = 1e-12)
    expect_equal(precomputed$info_matrix, original$info_matrix, tolerance = 1e-12)
  }
})

test_that("intercept-only nuisance formulas produce the expected information", {
  df <- data.frame(row = 1:4, col = 1L, treatment = c("A", "B", "A", "B"))
  for (covariance in list(NULL, diag(4))) {
    result <- objective_function_info(
      df, "treatment", c("row", "col"),
      Sigma = covariance, nuisance_formula = ~ 1
    )
    expect_equal(unname(result$info_matrix), matrix(c(1, -1, -1, 1), 2, 2))
    expect_equal(result$A_value, 0.5, tolerance = 1e-10)
  }
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

test_that("speed() does not treat negative D-optimality scores as an optimum", {
  df <- initialise_design_df(
    items = 6, nrows = 4, ncols = 6,
    block_nrows = 1, block_ncols = 6
  )
  Sigma <- cor_ar1_ar1(4, 6, rho_row = 0.6, rho_col = 0.3)

  for (covariance in list(NULL, Sigma)) {
    result <- speed(
      df,
      swap = "treatment",
      swap_within = "block",
      obj_function = objective_function_info,
      criterion = "D",
      Sigma = covariance,
      iterations = 100,
      early_stop_iterations = 200,
      seed = 1,
      quiet = TRUE
    )
    final <- objective_function_info(
      result$design_df, "treatment", c("row", "col"),
      criterion = "D", Sigma = covariance
    )

    expect_lt(result$score, 0)
    expect_equal(result$iterations_run, 100)
    expect_identical(result$metadata$per_level[[1]]$stop_reason, "iterations")
    expect_equal(result$score, final$score, tolerance = 1e-10)
    if (is.null(covariance)) {
      expect_equal(result$scores, rep(-5 * log(4), 100), tolerance = 1e-10)
    }
  }
})

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
