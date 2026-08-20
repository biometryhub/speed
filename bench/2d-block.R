# 100 treatments, 4 reps, 20 rows, 20 columns, blocked in both directions
# row_block: 5 rows x 20 cols (a full rep), col_block: 20 rows x 5 cols
n_treatments_2d <- 100
n_reps_2d <- 4
n_rows_2d <- 20
n_cols_2d <- 20
row_block_nrows_2d <- 5
row_block_ncols_2d <- 20
col_block_nrows_2d <- 20
col_block_ncols_2d <- 5
units_2d <- ~ row_block + col_block + row * col
odw_search <- "tabu+rw"

# speed
df_initial_2d <- speed::initialise_design_df(
  items = n_treatments_2d,
  nrows = n_rows_2d,
  ncols = n_cols_2d,
  block_nrows = row_block_nrows_2d,
  block_ncols = row_block_ncols_2d
) |>
  to_factor(c("treatment", "row", "col"))

# only used for its column-strip blocking
df_dummy_2d <- speed::initialise_design_df(
  items = n_treatments_2d,
  nrows = n_rows_2d,
  ncols = n_cols_2d,
  block_nrows = col_block_nrows_2d,
  block_ncols = col_block_ncols_2d
)
df_initial_2d$row_block <- as.factor(df_initial_2d$block)
df_initial_2d$col_block <- as.factor(df_dummy_2d$block)

bench_speed_2d <- function(seed = 112) {
  speed::speed(
    data = df_initial_2d,
    swap = "treatment",
    swap_within = "row_block",
    spatial_factors = ~col_block,
    iterations = 1000000,
    early_stop_iterations = 200000,
    optimise_params = optim_params(
      random_initialisation = 300,
      adaptive_swaps = TRUE,
      swap_count = 3
    ),
    seed = seed
  )
}

# digger
bench_digger_2d <- function(seed = 112) {
  DiGGer::ibDiGGer(
    numberOfTreatments = n_treatments_2d,
    rowsInDesign = n_rows_2d,
    columnsInDesign = n_cols_2d,
    rowsInReplicate = row_block_nrows_2d,
    columnsInReplicate = row_block_ncols_2d,
    rowsInBlock = col_block_nrows_2d,
    columnsInBlock = col_block_ncols_2d,
    maxInterchanges = 100000,
    rngSeeds = rep(seed, 2)
  )
}

digger_2d_df <- function(seed = 112) {
  df <- df_initial_2d
  df$treatment <- c(DiGGer::getDesign(bench_digger_2d(seed)))

  return(df)
}

# odw
df_initial_odw_2d <- speed:::shuffle_items(
  df_initial_2d,
  "treatment",
  "row_block",
  112
)

initial_param_table_2d <- odw::odw(
  random = ~ treatment + col_block + row_block,
  data = df_initial_2d,
  permute = ~treatment,
  swap = ~row_block,
  search = odw_search,
  start.values = TRUE
)$vparameters.table
initial_param_table_2d[2:3, 2] <- 100

bench_odw_2d <- function() {
  odw::odw(
    random = ~ treatment + col_block + row_block,
    data = df_initial_odw_2d,
    permute = ~treatment,
    swap = ~row_block,
    search = odw_search,
    G.param = initial_param_table_2d,
    R.param = initial_param_table_2d,
    maxit = 3
  )
}

two_d_design <- function() {
  return(list(
    units = units_2d,
    treatment = "treatment",
    is_converged = function(df) TRUE,
    custom_metrics = function(df) get_metrics(df, c("col_block", "row_block")),
    tools = list(
      speed = function(seed) bench_speed_2d(seed)$design_df,
      digger = function(seed) digger_2d_df(seed),
      odw = function(seed) bench_odw_2d()$design
    )
  ))
}

autoplot_2d <- function(df) {
  rows <- speed::autoplot(df, block = "row_block")
  cols <- speed::autoplot(df, block = "col_block")

  return(rows + cols + patchwork::plot_layout(ncol = 2))
}

explore_two_d <- function() {
  png(bench_out("layout-2d-block.png"), height = 720, width = 1440)
  print(autoplot_2d(as_design(df_initial_2d)))
  dev.off()

  speed_result <- bench_speed_2d()
  design_df <- speed_result$design_df
  print(get_metrics(design_df, c("col_block", "row_block")))
  print(efficiency(design_df, "treatment", units_2d))
  # Source.units   df1 Source.treatments df2 aefficiency eefficiency order
  # row_block        3
  # col_block        3
  # row[row_block]  16 treatment          16      0.2215      0.1158    16
  # col[col_block]  16 treatment          16      0.2077      0.1002    16
  # row#col        361 treatment          99      0.8911      0.4824    33
  #                    Residual          262

  png(bench_out("speed-2d-block.png"), height = 720, width = 1440)
  print(autoplot_2d(as_design(design_df)))
  dev.off()

  df_digger <- digger_2d_df()
  print(get_metrics(df_digger, c("col_block", "row_block")))
  print(efficiency(df_digger, "treatment", units_2d))
  # Source.units   df1 Source.treatments df2 aefficiency eefficiency order
  # row_block        3
  # col_block        3
  # row[row_block]  16 treatment          16      0.2126      0.1064    16
  # col[col_block]  16 treatment          16      0.2299      0.1330    16
  # row#col        361 treatment          99      0.8925      0.5016    33
  #                    Residual          262

  png(bench_out("digger-2d-block.png"), height = 720, width = 1440)
  print(autoplot_2d(as_design(df_digger)))
  dev.off()

  # df_odw <- bench_odw_2d()$design
  # print(get_metrics(df_odw, c("col_block", "row_block")))
  # print(efficiency(df_odw, "treatment", units_2d))
  #
  # png(bench_out("odw-2d-block.png"), height = 720, width = 1440)
  # print(autoplot_2d(as_design(df_odw)))
  # dev.off()

  return(invisible(NULL))
}
