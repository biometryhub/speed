test_that("speed works for simple design", {
  nrows <- 5
  ncols <- 5
  nblocks <- 5
  df_initial <- initialize_design_df(1:5, 5, nrows, ncols, nblocks, 1)
  treatments <- sort(as.character(df_initial$treatment))

  iterations <- 40000
  seed <- 112
  start_temp <- 100
  options(
    speed.swap_count = 1,
    speed.swap_all_blocks = FALSE,
    speed.adaptive_swaps = FALSE,
    speed.start_temp = start_temp,
    speed.cooling_rate = 0.99,
    speed.adj_weight = 1,
    speed.bal_weight = 1
  )

  speed_design <- speed(
    df_initial,
    treatment_cols = treatment,
    swap_within = "1",
    spatial_factors = ~ block + row,
    iterations = ,
    early_stop_iterations = 10000,
    quiet = TRUE,
    seed = seed
  )
  design_matrix <- speed_design$design
  design_df <- speed_design$design_df

  # check quality
  expect_equal(speed_design$adjacency_score, 0)
  expect_equal(speed_design$balance_score, 0)
  expect_equal(speed_design$score, 0)
  expect_equal(unique(table(design_df$treatment, design_df$row)), 1)
  expect_equal(unique(table(design_df$treatment, design_df$col)), 1)

  # check simple config
  expect_equal(speed_design$seed, seed)
  expect_lt(speed_design$iterations_run, iterations)
  expect_lte(max(speed_design$temperatures), start_temp)

  expect_equal(sort(speed_design$treatments), unique(treatments))

  # check matrix
  expect_equal(nrow(design_matrix), nrows)
  expect_equal(ncol(design_matrix), ncols)
  expect_equal(sort(as.vector(design_matrix)), treatments)

  # check df
  expect_equal(sort(design_df$treatment), treatments)
})

test_that("speed works for 2d blocking", {
  nrows <- 20
  ncols <- 20
  df_initial <- initialize_design_df(1:40, 10, nrows, ncols, 2, 2)
  treatments <- sort(as.character(df_initial$treatment))

  options(
    speed.swap_count = 5,
    speed.swap_all_blocks = FALSE,
    speed.adaptive_swaps = TRUE,
    speed.start_temp = 100,
    speed.cooling_rate = 0.99,
    speed.adj_weight = 1,
    speed.bal_weight = 1
  )

  speed_design <- speed(
    df_initial,
    treatment_cols = "treatment",
    swap_within = "col_block",
    spatial_factors = ~row_block,
    iterations = 1000000,
    early_stop_iterations = 20000,
    quiet = TRUE,
    seed = 538
  )
  design_matrix <- speed_design$design
  design_df <- speed_design$design_df

  # check quality
  expect_equal(speed_design$adjacency_score, 0)
  expect_equal(speed_design$balance_score, 0)
  expect_equal(speed_design$score, 0)
  expect_equal(max(unique(table(design_df$treatment, design_df$row))), 1)
  expect_equal(max(unique(table(design_df$treatment, design_df$col))), 1)
  expect_equal(max(unique(table(design_df$treatment, design_df$row_block))), 1)
  expect_equal(max(unique(table(design_df$treatment, design_df$col_block))), 1)

  # check matrix
  expect_equal(nrow(design_matrix), nrows)
  expect_equal(ncol(design_matrix), ncols)
  expect_equal(sort(as.vector(design_matrix)), treatments)

  # check df
  expect_equal(sort(design_df$treatment), treatments)
})

# TODO: add tests for more complex designs
