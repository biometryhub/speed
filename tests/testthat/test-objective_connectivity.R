conn_df <- function() initialise_design_df(1:8, 6, 4, 2, 4)

test_that("connectivity objective verifies concurrence_factors", {
  df <- conn_df()
  expect_error(
    objective_function_connectivity(df, "treatment", c("row", "col")),
    "`concurrence_factors` is required"
  )
  expect_error(
    objective_function_connectivity(
      df,
      "treatment",
      c("row", "col"),
      concurrence_factors = ~missing
    ),
    "'missing' not found"
  )
})

test_that("connectivity objective scores correctly", {
  df <- conn_df()
  # uneven groups of 3, 8 and 13 plots, so each needs its own 1/size weight
  df$group <- rep(1:3, c(3, 8, 13))
  res <- objective_function_connectivity(
    df,
    "treatment",
    character(0),
    adj_weight = 0,
    concurrence_factors = ~group
  )

  counts <- table(df$treatment, df$group)
  # weighted pair counts
  w <- counts %*% diag(1 / colSums(counts)) %*% t(counts)
  expect_equal(res$score, round(var(w[upper.tri(w)]), 10))
  expect_equal(res$components[["balance"]], 0)
})

test_that("only the finest terms become concurrence groups", {
  df <- conn_df()
  # rows nested in blocks
  expect_named(resolve_connectivity_terms(df, ~ block + row), "row")
  expect_named(resolve_connectivity_terms(df, ~ row + col), c("row", "col"))
  # single plots ignored
  expect_length(resolve_connectivity_terms(df, ~ row:col), 0)
  expect_named(resolve_connectivity_terms(df, ~ block + col), c("block", "col"))
  expect_named(resolve_connectivity_terms(df, ~ block * col), c("block:col"))
})

test_that("adjacency and balance match the base objective", {
  df <- conn_df()
  spatial_cols <- c("row", "col", "block")
  res <- objective_function_connectivity(
    df,
    "treatment",
    spatial_cols,
    concurrence_factors = ~row
  )
  base <- objective_function(df, "treatment", spatial_cols)
  expect_equal(res$components[c("adjacency", "balance")], base$components)
  expect_equal(res$score, round(sum(res$components), 10))
})

test_that("plots outside any group are left out", {
  df <- conn_df()
  # e.g. buffer plots with no block
  df$block[df$row == 6] <- NA
  res <- objective_function_connectivity(
    df,
    "treatment",
    character(0),
    adj_weight = 0,
    concurrence_factors = ~block
  )

  # `table` drops NA
  counts <- table(df$treatment, df$block)
  w <- counts %*% diag(1 / colSums(counts)) %*% t(counts)
  expect_equal(res$score, round(var(w[upper.tri(w)]), 10))
})

test_that("incremental updates work", {
  set.seed(1)
  df <- conn_df()
  df$treatment <- factor(df$treatment)
  score <- function(design, ...) {
    return(objective_function_connectivity(
      design,
      "treatment",
      c("row", "col"),
      concurrence_factors = ~ block + row + col,
      ...
    ))
  }
  incremental <- score(df)

  # multi-item swaps, about half accepted as in the SA loop
  for (i in seq_len(100)) {
    nb <- generate_neighbour(
      df,
      "treatment",
      "block",
      swap_count = sample(1:3, 1)
    )
    new_incremental <- score(
      nb$design,
      current_score_obj = incremental,
      swapped_items = nb$swapped_items
    )
    if (runif(1) < 0.5) {
      df <- nb$design
      incremental <- new_incremental
    }
  }

  full <- score(df)
  expect_equal(incremental$score, full$score)
  expect_equal(incremental$components, full$components)
})

test_that("speed switches to the connectivity objective with concurrence_factors", {
  df <- conn_df()
  res <- speed(
    df,
    "treatment",
    swap_within = "block",
    concurrence_factors = ~ row + col,
    iterations = 200,
    early_stop_iterations = 100,
    quiet = TRUE,
    seed = 1
  )
  meta <- res$metadata$per_level[[1]]
  expect_equal(
    .objective_name(meta$obj_function),
    "objective_function_connectivity"
  )
  expect_true("concurrence" %in% names(meta$final_components))
  expect_equal(meta$concurrence_factors, ~ row + col)

  rescored <- objective_function_connectivity(
    res$design_df,
    "treatment",
    c("row", "col"),
    concurrence_factors = ~ row + col
  )
  expect_equal(res$score, rescored$score)
})

test_that("random_initialisation scores shuffles with concurrence_factors", {
  df <- conn_df()
  initialise <- function(obj_function, concurrence_factors = NULL) {
    optimise <- list(
      level = list(
        swap = "treatment",
        swap_within = "block",
        spatial_factors = ~ row + col,
        concurrence_factors = concurrence_factors,
        swap_all = FALSE,
        obj_function = obj_function,
        optimise_params = optim_params(
          random_initialisation = 20,
          # no adj and bal to isolate concurrence score
          adj_weight = 0,
          bal_weight = 0
        )
      )
    )
    return(random_initialise(df, optimise, seed = 1))
  }

  score <- function(design) {
    return(objective_function_connectivity(
      design,
      "treatment",
      c("row", "col"),
      concurrence_factors = ~ row + col
    )$components[["concurrence"]])
  }

  expect_lt(
    score(initialise(objective_function_connectivity, ~ row + col)),
    score(initialise(objective_function))
  )
})

# TODO: remove when support
test_that("concurrence_factors is rejected for hierarchical designs", {
  df_split <- data.frame(
    row = rep(1:12, each = 4),
    col = rep(1:4, times = 12),
    block = rep(1:4, each = 12),
    wholeplot = rep(1:12, each = 4),
    wholeplot_treatment = rep(rep(LETTERS[1:3], each = 4), times = 4),
    subplot_treatment = rep(letters[1:4], 12)
  )
  msg <- "not supported for hierarchical designs"
  expect_error(
    speed(
      df_split,
      swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
      swap_within = list(wp = "block", sp = "wholeplot"),
      concurrence_factors = list(sp = ~row),
      iterations = 1,
      quiet = TRUE
    ),
    msg
  )
  expect_error(
    speed(
      df_split,
      optimise = list(
        wp = list(swap = "wholeplot_treatment", swap_within = "block"),
        sp = list(
          swap = "subplot_treatment",
          swap_within = "wholeplot",
          concurrence_factors = ~row
        )
      ),
      iterations = 1,
      quiet = TRUE
    ),
    msg
  )
})

test_that("speed rejects concurrence_factors with a custom objective", {
  df <- conn_df()
  for (fn in list(objective_function_piepho, objective_function_connectivity)) {
    expect_error(
      speed(
        df,
        "treatment",
        concurrence_factors = ~row,
        obj_function = fn,
        quiet = TRUE
      ),
      "can only be used with the default objective function"
    )
  }
  expect_error(
    speed(df, "treatment", concurrence_factors = "row", quiet = TRUE),
    "one sided formula"
  )
})
