conn_df <- function() {
  df <- initialise_design_df(1:8, nrows = 6, ncols = 4, block_nrows = 2, block_ncols = 4)
  return(df)
}

test_that("connectivity objective requires concurrence_factors", {
  expect_error(
    objective_function_connectivity(conn_df(), "treatment", c("row", "col")),
    "`concurrence_factors` is required"
  )
})

test_that("connectivity objective scores the pooled concurrence variance", {
  df <- conn_df()
  res <- objective_function_connectivity(
    df, "treatment", character(0),
    adj_weight = 0, concurrence_factors = ~row
  )

  # W from rows of 4 plots: each pair sharing a row earns 1/4
  n_mat <- table(df$treatment, df$row)
  w <- n_mat %*% t(n_mat) / 4
  expect_equal(res$score, round(var(w[upper.tri(w)]), 10))
  expect_equal(unname(res$components[["balance"]]), 0)
})

test_that("coarser terms are dropped from the concurrence groups", {
  df <- conn_df()
  groupings <- resolve_connectivity_terms(df, ~ block + row)
  expect_length(groupings, 1)
  expect_error(
    resolve_connectivity_terms(df, ~missing),
    "not found"
  )
})

test_that("adjacency and balance match the base objective", {
  df <- conn_df()
  sc <- c("row", "col", "block")
  res <- objective_function_connectivity(
    df, "treatment", sc, conc_weight = 0, concurrence_factors = ~row
  )
  base <- objective_function(df, "treatment", sc)
  expect_equal(res$score, base$score)
  expect_equal(res$components[c("adjacency", "balance")], base$components)
})

test_that("incremental updates match a full rescore", {
  df <- conn_df()
  df$treatment <- factor(df$treatment)
  first <- objective_function_connectivity(
    df, "treatment", c("row", "col"), concurrence_factors = ~ block + col
  )

  # swap the treatments of two plots in the same block
  swapped <- df
  idx <- c(1, 6)
  swapped$treatment[idx] <- df$treatment[rev(idx)]
  items <- as.character(df$treatment[idx])

  incremental <- objective_function_connectivity(
    swapped, "treatment", c("row", "col"),
    concurrence_factors = ~ block + col,
    current_score_obj = first, swapped_items = items
  )
  full <- objective_function_connectivity(
    swapped, "treatment", c("row", "col"), concurrence_factors = ~ block + col
  )
  expect_equal(incremental$score, full$score)
  expect_equal(incremental$components, full$components)
})

test_that("speed switches to the connectivity objective with concurrence_factors", {
  df <- conn_df()
  res <- speed(
    df, "treatment",
    swap_within = "block",
    concurrence_factors = ~ row + col,
    iterations = 200, early_stop_iterations = 100,
    quiet = TRUE, seed = 1
  )
  meta <- res$metadata$per_level[[1]]
  expect_equal(.objective_name(meta$obj_function), "objective_function_connectivity")
  expect_true("concurrence" %in% names(meta$final_components))
  expect_equal(meta$concurrence_factors, ~ row + col)

  plain <- speed(df, "treatment", iterations = 10, quiet = TRUE, seed = 1)
  expect_identical(plain$metadata$per_level[[1]]$obj_function, objective_function)
})

test_that("speed accepts the connectivity objective by name", {
  df <- conn_df()
  res <- speed(
    df, "treatment",
    concurrence_factors = ~row,
    obj_function = objective_function_connectivity,
    iterations = 10, quiet = TRUE, seed = 1
  )
  meta <- res$metadata$per_level[[1]]
  expect_equal(.objective_name(meta$obj_function), "objective_function_connectivity")
})

test_that("conc_weight travels through optim_params", {
  df <- conn_df()
  run <- function(...) {
    return(speed(
      df, "treatment",
      concurrence_factors = ~row,
      iterations = 1, quiet = TRUE, seed = 1, ...
    ))
  }
  # score the same layout with each run's bound objective
  conc <- function(res) {
    fn <- res$metadata$per_level[[1]]$obj_function
    return(fn(df, "treatment", c("row", "col"))$components[["concurrence"]])
  }

  base <- run()
  heavy <- run(optimise_params = optim_params(conc_weight = 3))
  expect_gt(conc(base), 0)
  expect_equal(conc(heavy), 3 * conc(base))
  expect_error(run(conc_weight = 3), "must be passed via `optim_params\\(\\)`")
})

test_that("speed rejects concurrence_factors with a custom objective or bad formula", {
  df <- conn_df()
  expect_error(
    speed(df, "treatment", concurrence_factors = ~row,
          obj_function = objective_function_piepho, quiet = TRUE),
    "can only be used with"
  )
  expect_error(
    speed(df, "treatment", concurrence_factors = ~nope, quiet = TRUE),
    "nope"
  )
  expect_error(
    speed(df, "treatment", concurrence_factors = "row", quiet = TRUE),
    "one sided formula"
  )
})
