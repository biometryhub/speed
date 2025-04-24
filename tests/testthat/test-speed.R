test_that("speed works for simple design", {
  nrows <- 5
  ncols <- 5
  nblocks <- 5
  treatments <- rep(paste0("T", 1:5), nblocks)
  df_initial <- data.frame(
    row = factor(rep(1:nrows, ncols)),
    col = factor(rep(1:ncols, each = nrows)),
    block = factor(rep(1:nblocks, each = nrows * ncols / nblocks)),
    treatment = factor(sample(treatments, length(treatments)))
  )

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

  speed_design <- speed(df_initial,
    treatment_cols = "treatment",
    swap = ~1,
    spatial_factors = ~ block + row,
    iterations = ,
    early_stop_iterations = 10000,
    quiet = TRUE,
    seed = seed
  )
  design_matrix <- speed_design$design
  design_df <- speed_design$design_df

  expect_equal(speed_design$adjacency_score, 0)
  expect_equal(speed_design$balance_score, 0)
  expect_equal(speed_design$score, 0)
  expect_equal(unique(table(design_df$treatment, design_df$row)), 1)
  expect_equal(unique(table(design_df$treatment, design_df$col)), 1)

  expect_equal(speed_design$seed, seed)
  expect_lt(speed_design$iterations_run, iterations)
  expect_lte(max(speed_design$temperatures), start_temp)

  expect_equal(nrow(design_matrix), nrows)
  expect_equal(ncol(design_matrix), ncols)
})

# TODO: add tests for more complex designs

# # 2d blocking
# row <- factor(rep(1:20, each = 20))
# col <- factor(rep(1:20, 20))
# block <- factor(rep(1:10, each = 40))
# treats <- rep(factor(paste("V", 1:40, sep = "")), 10)
# dat <- data.frame(row = row, col = col, treat = treats, row_block = block)
# dat <- dat[order(dat$col, dat$row), ]
# dat$col_block <- factor(rep(1:10, each = 40))
#
# options(
#   speed.swap_count = 3,
#   speed.swap_all_blocks = FALSE,
#   speed.adaptive_swaps = TRUE,
#   speed.start_temp = 100,
#   speed.cooling_rate = 0.99,
#   speed.adj_weight = 1,
#   speed.bal_weight = 1
# )
# des <- speed(
#   dat,
#   treatment_cols = "treat",
#   swap_within = ~row_block,
#   spatial_factors = ~col_block,
#   iterations = 1000000,
#   early_stop_iterations = 10000
# )
# desd <- des$design_df
# trs <- paste("V", 1:40, sep = "")
#
# ## check unique entries in RowBlock and ColBlock.
#
# lapply(split(desd, desd$row_block), function(el, trs) trs %in% as.character(unique(el$treat)), trs)
# lapply(split(desd, desd$col_block), function(el, trs) trs %in% as.character(unique(el$treat)), trs)
#
# ## already optimal across rows and cols
#
# desd <- des$design_df
# table(desd$treat, desd$row)
# table(desd$treat, desd$col)
