# Split plot: 3 wholeplot treatments x 4 subplot treatments, 12 rows, 4 columns
n_subplot_treatments <- 4
n_subplot_reps <- 12
n_wholeplot_treatments <- 3
n_wholeplot_reps <- 4
block_nrows_split <- 3
block_ncols_split <- 4
wholeplot_nrows <- 1
wholeplot_ncols <- 4
n_rows_split <- 12
n_cols_split <- 4
odw_search <- "tabu+rw"

# Hierarchical split-plot design
df_initial_split <- speed::initialise_split_design_df(
  splits = list(
    subplot = list(items = letters[1:n_subplot_treatments]),
    wholeplot = list(
      items = LETTERS[1:n_wholeplot_treatments],
      nrows = wholeplot_nrows,
      ncols = wholeplot_ncols
    ),
    block = list(nrows = block_nrows_split, ncols = block_ncols_split)
  ),
  rep_dim = c(n_wholeplot_reps, 1)
) |>
  (function(df) df[order(df$row, df$col), ])() |>
  to_factor(
    c(
      "subplot",
      "wholeplot",
      "block",
      "wholeplot_treatment",
      "subplot_treatment",
      "row",
      "col"
    )
  )
rownames(df_initial_split) <- NULL

# speed
bench_speed_split <- function(seed = 112) {
  optimise <- list(
    wp = list(
      swap = "wholeplot_treatment",
      swap_within = "block",
      swap_all = TRUE,
      optimise_params = optim_params(bal_weight = 0),
      early_stop_iterations = 1000
    ),
    sp = list(
      swap = "subplot_treatment",
      swap_within = "wholeplot",
      spatial_factors = ~col,
      early_stop_iterations = 10000
    )
  )

  return(speed::speed(df_initial_split, optimise = optimise, seed = seed))
}

# digger - optimised separately:
# - wholeplot treatments from a one-column design
# - subplot treatments from the full design with each wholeplot as a block
bench_digger_whole_split <- function(seed = 112) {
  DiGGer::ibDiGGer(
    numberOfTreatments = n_wholeplot_treatments,
    rowsInDesign = n_rows_split,
    columnsInDesign = 1,
    rowsInBlock = block_nrows_split,
    columnsInBlock = 1,
    maxInterchanges = 1000,
    rngSeeds = rep(seed, 2)
  )
}

bench_digger_sub_split <- function(seed = 112) {
  DiGGer::ibDiGGer(
    numberOfTreatments = n_subplot_treatments,
    rowsInDesign = n_rows_split,
    columnsInDesign = n_cols_split,
    rowsInBlock = wholeplot_nrows,
    columnsInBlock = wholeplot_ncols,
    rowsInReplicate = n_rows_split,
    columnsInReplicate = 1,
    maxInterchanges = 10000,
    rngSeeds = rep(seed, 2)
  )
}

digger_split_df <- function(seed = 112) {
  whole_design <- DiGGer::getDesign(bench_digger_whole_split(seed))
  sub_design <- DiGGer::getDesign(bench_digger_sub_split(seed))
  df <- df_initial_split
  df$wholeplot_treatment <- rep(LETTERS[c(whole_design)], each = n_cols_split)
  # digger designs are row x col matrices, df_initial_split is row-major
  df$subplot_treatment <- letters[c(t(sub_design))]

  return(df)
}

# odw - like digger
df_dummy_odw <- speed::initialise_design_df(
  as.factor(rep(LETTERS[1:n_wholeplot_treatments], n_wholeplot_reps)),
  n_rows_split,
  1,
  block_nrows_split,
  1
) |>
  to_factor(c("block", "row", "col")) |>
  speed:::shuffle_items("treatment", "block", 112)

initial_param_table_dummy <- odw::odw(
  random = ~ treatment + row + block,
  data = df_dummy_odw,
  permute = ~treatment,
  swap = ~block,
  search = odw_search,
  start.values = TRUE
)$vparameters.table
initial_param_table_dummy[2, 2] <- 100

bench_odw_dummy <- function() {
  odw::odw(
    random = ~ treatment + row + block,
    data = df_dummy_odw,
    permute = ~treatment,
    swap = ~block,
    search = odw_search,
    G.param = initial_param_table_dummy,
    R.param = initial_param_table_dummy,
    maxit = 2
  )
}

df_initial_odw_split <- df_initial_split

initial_param_table_split <- odw::odw(
  random = ~ subplot_treatment + col,
  data = df_initial_odw_split,
  permute = ~subplot_treatment,
  swap = ~row,
  search = odw_search,
  start.values = TRUE
)$vparameters.table
initial_param_table_split[2, 2] <- 100

bench_odw_split <- function() {
  odw::odw(
    random = ~ subplot_treatment + col,
    data = df_initial_odw_split,
    permute = ~subplot_treatment,
    swap = ~row,
    search = odw_search,
    G.param = initial_param_table_split,
    R.param = initial_param_table_split,
    maxit = 3
  )
}

odw_split_df <- function() {
  df <- bench_odw_split()$design
  df$wholeplot_treatment <- rep(
    bench_odw_dummy()$design$treatment,
    each = n_cols_split
  )

  return(df)
}

split_metrics <- function(df) {
  print(unique(table(df$wholeplot_treatment, df$block)))
  print(unique(table(df$subplot_treatment, df$wholeplot)))
  print(speed::calculate_adjacency_score(df, "subplot_treatment"))
  print(efficiency(
    df,
    c("wholeplot_treatment", "subplot_treatment"),
    ~ block / wholeplot / col + col
  ))

  return(invisible(NULL))
}

split_design <- function() {
  return(list(
    units = ~ block / wholeplot / col + col,
    treatment = c("wholeplot_treatment", "subplot_treatment"),
    is_converged = function(df) TRUE,
    custom_metrics = function(df) {
      list(
        sub_adjacency = speed::calculate_adjacency_score(
          df,
          "subplot_treatment"
        ),
        whole_adjacency = speed::calculate_adjacency_score(
          df,
          "wholeplot_treatment"
        )
      )
    },
    tools = list(
      speed = function(seed) bench_speed_split(seed)$design_df,
      digger = function(seed) digger_split_df(seed),
      odw = function(seed) odw_split_df()
    )
  ))
}

autoplot_split <- function(df) {
  whole <- speed::autoplot(df, treatments = "wholeplot_treatment")
  sub <- speed::autoplot(
    df,
    treatments = "subplot_treatment",
    block = "wholeplot"
  )
  return(whole + sub + patchwork::plot_layout(ncol = 2))
}

explore_split <- function() {
  png(bench_out("layout-split.png"), height = 720, width = 480)
  print(autoplot_split(as_design(df_initial_split)))
  dev.off()

  speed_result <- bench_speed_split()
  print(speed_result$score)
  split_metrics(speed_result$design_df)
  # Source.units         df1 Source.treatments   df2 aefficiency eefficiency order
  # block                  3
  # wholeplot[block]       8 wholeplot_treatment   2      1.0000      1.0000     1
  #                          Residual              6
  # col[block:wholeplot]  36 subplot_treatment     3      1.0000      1.0000     1
  #                          Residual             33

  png(bench_out("speed-split.png"), height = 720, width = 480)
  print(autoplot_split(speed_result))
  dev.off()

  df_digger <- digger_split_df()
  split_metrics(df_digger)
  # Source.units         df1 Source.treatments   df2 aefficiency eefficiency order
  # block                  3
  # wholeplot[block]       8 wholeplot_treatment   2      1.0000      1.0000     1
  #                          Residual              6
  # col[block:wholeplot]  36 subplot_treatment     3      1.0000      1.0000     1
  #                          Residual             33

  png(bench_out("digger-split.png"), height = 720, width = 480)
  print(autoplot_split(as_design(df_digger)))
  dev.off()

  df_odw <- odw_split_df()
  split_metrics(df_odw)
  # Source.units         df1 Source.treatments   df2 aefficiency eefficiency order
  # block                  3
  # wholeplot[block]       8 wholeplot_treatment   2      1.0000      1.0000     1
  #                          Residual              6
  # col[block:wholeplot]  36 subplot_treatment     3      1.0000      1.0000     1
  #                          Residual             33

  png(bench_out("odw-split.png"), height = 720, width = 480)
  print(autoplot_split(as_design(df_odw)))
  dev.off()

  return(invisible(NULL))
}
