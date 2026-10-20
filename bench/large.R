# 250 treatments, 6 reps, 250 rows, 6 columns
n_treatments_large <- 250
n_reps_large <- 6
n_rows_large <- 250
n_cols_large <- 6
units_large <- ~ block + row * col
odw_search <- "tabu+rw"

# speed
df_initial_large <- speed::initialise_design_df(
  items = n_treatments_large,
  nrows = n_rows_large,
  ncols = n_cols_large,
  block_nrows = 125,
  block_ncols = 2
) |>
  to_factor(c("treatment", "block", "row", "col"))

bench_speed_large <- function(seed = 112) {
  speed::speed(
    data = df_initial_large,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col,
    optimise_params = optim_params(
      random_initialisation = 10,
      target_swaps = 0
    ),
    seed = seed
  )
}

bench_digger_large <- function(seed = 112) {
  DiGGer::corDiGGer(
    numberOfTreatments = n_treatments_large,
    rowsInDesign = n_rows_large,
    columnsInDesign = n_cols_large,
    rowsInReplicate = 125,
    columnsInReplicate = 2,
    treatRepPerRep = 1,
    blockSequence = list(c(250, 1)),
    maxInterchanges = c(100000, 2000000),
    rngSeeds = rep(seed, 2)
  )
}

digger_large_df <- function(seed = 112) {
  df <- df_initial_large
  df$treatment <- c(DiGGer::getDesign(bench_digger_large(seed)))

  return(df)
}

# odw
df_initial_odw_large <- speed::initialise_design_df(
  rep(1:n_treatments_large, n_reps_large),
  n_rows_large,
  n_cols_large,
  125,
  2
) |>
  speed:::shuffle_items("treatment", "block", 112) |>
  to_factor(c("treatment", "block", "row", "col"))

initial_param_table_large <- odw::odw(
  random = ~ treatment + row + col,
  data = df_initial_odw_large,
  permute = ~treatment,
  swap = ~block,
  search = odw_search,
  start.values = TRUE
)$vparameters.table
initial_param_table_large[
  initial_param_table_large[[1]] %in% c("row", "col"),
  2
] <- 100

bench_odw_large <- function() {
  odw::odw(
    random = ~ treatment + row + col,
    data = df_initial_odw_large,
    permute = ~treatment,
    swap = ~block,
    search = odw_search,
    G.param = initial_param_table_large,
    R.param = initial_param_table_large,
    maxit = 9
  )
}

large_design <- function() {
  return(list(
    units = units_large,
    treatment = "treatment",
    is_converged = function(df) TRUE,
    custom_metrics = function(df) get_metrics(df, c("row", "col", "block")),
    tools = list(
      speed = function(seed) bench_speed_large(seed)$design_df,
      digger = function(seed) digger_large_df(seed),
      odw = function(seed) bench_odw_large()$design
    )
  ))
}

explore_large <- function() {
  ggsave(
    bench_out("layout-large.pdf"),
    autoplot_bench(as_design(df_initial_large)),
    width = 6,
    height = 6
  )

  speed_result <- bench_speed_large()
  speed_result$score

  design_df <- speed_result$design_df
  print(get_metrics(design_df, c("row", "col", "block")))
  print(efficiency(design_df, "treatment", units_large))
  # Source.units df1  Source.treatments df2  aefficiency eefficiency order
  # block           5
  # row           248 treatment          248      0.0005      0.0000   248
  # col             3 treatment            1      0.0027      0.0027     1
  #                   Residual             2
  # row#col      1243 treatment          249      0.8046      0.4681   249
  #                   Residual           994

  ggsave(
    bench_out("speed-large.pdf"),
    autoplot_bench(speed_result) + ggtitle("speed"),
    width = 6,
    height = 6
  )

  df_digger <- digger_large_df()
  print(get_metrics(df_digger, c("row", "col", "block")))
  print(efficiency(df_digger, "treatment", units_large))
  # Source.units df1  Source.treatments df2  aefficiency eefficiency order
  # block           5
  # row           248 treatment          248      0.0002      0.0000   248
  # col             3
  # row#col      1243 treatment          249      0.7988      0.4177   249
  #                   Residual           994

  ggsave(
    bench_out("digger-large.pdf"),
    autoplot_bench(as_design(df_digger)) + ggtitle("DiGGer"),
    width = 6,
    height = 6
  )

  df_odw <- bench_odw_large()$design
  print(get_metrics(df_odw, c("row", "col", "block")))
  print(efficiency(df_odw, "treatment", units_large))
  # Source.units   df1 Source.treatments df2 aefficiency eefficiency order
  # row_block        9
  # col_block        9 treatment           1      0.0100      0.0100     1
  #                    Residual            8
  # row[row_block]  10 treatment          10      0.0835      0.0452    10
  # col[col_block]  10 treatment          10      0.0822      0.0440    10
  # row#col        361 treatment          39      0.9433      0.7756    22
  #                    Residual          322

  ggsave(
    bench_out("odw-large.pdf"),
    autoplot_bench(as_design(df_odw)) + ggtitle("ODW"),
    width = 6,
    height = 6
  )

  return(invisible(NULL))
}
