# 15 treatments, 5 reps, 25 rows, 3 columns
n_treatments_small <- 15
n_reps_small <- 5
n_rows_small <- 25
n_cols_small <- 3
odw_search <- "tabu+rw"

# speed
df_initial_small <- speed::initialise_design_df(
  items = n_treatments_small,
  nrows = n_rows_small,
  ncols = n_cols_small,
  block_nrows = 5,
  block_ncols = 3
) |>
  to_factor(c("treatment", "block", "row", "col"))

bench_speed_small <- function(seed = 112) {
  speed::speed(
    data = df_initial_small,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~col,
    optimise_params = optim_params(random_initialisation = 10),
    seed = seed
  )
}

# digger
bench_digger_small <- function(seed = 112) {
  DiGGer::corDiGGer(
    numberOfTreatments = n_treatments_small,
    rowsInDesign = n_rows_small,
    columnsInDesign = n_cols_small,
    rowsInReplicate = 5,
    columnsInReplicate = 3,
    treatRepPerRep = 1,
    blockSequence = list(c(12, 1)),
    maxInterchanges = c(5000, 500000),
    rngSeeds = rep(seed, 2)
  )
}

digger_small_df <- function(seed = 112) {
  df <- df_initial_small
  df$treatment <- c(DiGGer::getDesign(bench_digger_small(seed)))

  return(df)
}

# odw
df_initial_odw_small <- speed::initialise_design_df(
  rep(1:n_treatments_small, n_reps_small),
  n_rows_small,
  n_cols_small,
  5,
  3
) |>
  speed:::shuffle_items("treatment", "block", 112) |>
  to_factor(c("treatment", "block", "row", "col"))

initial_param_table_small <- odw::odw(
  random = ~ treatment + block + row + col,
  data = df_initial_odw_small,
  permute = ~treatment,
  swap = ~block,
  search = odw_search,
  start.values = TRUE
)$vparameters.table
initial_param_table_small[4, 2] <- 100

bench_odw_small <- function() {
  odw::odw(
    random = ~ treatment + block + row + col,
    data = df_initial_odw_small,
    permute = ~treatment,
    swap = ~block,
    search = odw_search,
    G.param = initial_param_table_small,
    R.param = initial_param_table_small,
    maxit = 2
  )
}

small_design <- function() {
  return(list(
    units = ~ block + row * col,
    treatment = "treatment",
    is_converged = function(df) TRUE,
    custom_metrics = function(df) get_metrics(df, c("row", "col", "block")),
    tools = list(
      speed = function(seed) bench_speed_small(seed)$design_df,
      digger = function(seed) digger_small_df(seed),
      odw = function(seed) bench_odw_small()$design
    )
  ))
}

explore_small <- function() {
  df_layout <- df_initial_small
  df_layout$plot_in_block <- df_initial_small$treatment
  png(bench_out("layout-small.png"), height = 500, width = 500)
  print(speed::autoplot(as_design(df_layout), treatments = "plot_in_block"))
  dev.off()

  speed_result <- bench_speed_small()
  design_df <- speed_result$design_df
  get_metrics(design_df, c("col", "block"))
  print(efficiency(design_df, "treatment", ~ block + row * col))
  # Source.units df1 Source.treatments df2 aefficiency eefficiency order
  # block          4
  # row[block]    20 treatment          14      0.1034      0.0249    14
  #                  Residual            6
  # col            2 treatment           2      0.0400      0.0400     1
  # row#col       48 treatment          14      0.6160      0.2696    14
  #                  Residual           34

  png(bench_out("speed-small.png"), height = 1080, width = 480)
  print(speed::autoplot(speed_result))
  dev.off()

  df_digger <- digger_small_df()
  get_metrics(df_digger, c("col", "block"))
  print(efficiency(df_digger, "treatment", ~ block + row * col))
  # Source.units df1 Source.treatments df2 aefficiency eefficiency order
  # block          4
  # row[block]    20 treatment          14      0.0833      0.0101    14
  #                  Residual            6
  # col            2 treatment           2      0.0400      0.0400     1
  # row#col       48 treatment          14      0.6488      0.3475    14
  #                  Residual           34

  png(bench_out("digger-small.png"), height = 500, width = 480)
  print(speed::autoplot(as_design(df_digger)))
  dev.off()

  df_odw <- bench_odw_small()$design
  get_metrics(df_odw, c("col", "block"))
  print(efficiency(df_odw, "treatment", ~ block + row * col))
  # Source.units df1 Source.treatments df2 aefficiency eefficiency order
  # block          4
  # row[block]    20 treatment          14      0.1891      0.0477    14
  #                  Residual            6
  # col            2 treatment           2      0.0400      0.0400     1
  # row#col       48 treatment          14      0.6823      0.4618    14
  #                  Residual           34

  png(bench_out("odw-small.png"), height = 500, width = 500)
  print(speed::autoplot(as_design(df_odw)))
  dev.off()

  return(invisible(NULL))
}
