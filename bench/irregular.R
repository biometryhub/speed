# Irregular, oz barley: 222 treatments, 5 blocks over two sites (sw 20 x 26,
# se 24 x 26) with blank plots. Block 1 spans both sites.
n_treatments_irr <- 222

# adjacency has to be scored per site, the two share (row, col) coordinates
get_metrics_irr <- function(df, cols) {
  metrics <- list(
    adjacency = speed::calculate_adjacency_score(
      df[df$site == "se", ],
      "treatment"
    ) +
      speed::calculate_adjacency_score(df[df$site == "sw", ], "treatment")
  )

  for (col in cols) {
    uniques <- unique(table(df$treatment, df[[col]]))
    name <- paste0(col, "_unique_occurence")
    metrics[[name]] <- paste(uniques, collapse = ",")
  }
  for (name in names(metrics)) {
    cat(sprintf("%s: %s\n", name, metrics[[name]]))
  }

  return(invisible(metrics))
}

initialise_df_irr <- function() {
  df <- initialise_design_df(
    items = 1,
    designs = list(
      sw = list(nrows = 20, ncols = 26),
      se = list(nrows = 24, ncols = 26)
    )
  )

  irr_row <- df$row
  irr_col <- df$col
  df$site_col <- paste(df$site, irr_col, sep = "_")
  df$site_row <- paste(df$site, irr_row, sep = "_")

  is_sw <- df$site == "sw"
  is_blank <- (is_sw &
    ((irr_col == 26 & irr_row %in% c(9:12, 15:18, 20)) |
      (irr_col == 15 & irr_row == 9))) |
    (!is_sw &
      ((irr_col == 26 & irr_row %in% c(15, 9:12)) |
        (irr_col %in% 21:26 & irr_row %in% 16:18) |
        irr_col == 15 & irr_row == 9))

  # assign blocks
  df$block <- 1
  df[is_sw & irr_row <= 18 & irr_col >= 6, ]$block <- 2
  df[is_sw & irr_row <= 15, ]$block <- 2
  df[is_sw & irr_row <= 9 & irr_col <= 14, ]$block <- 3
  df[is_sw & irr_row <= 8, ]$block <- 3
  df[!is_sw & irr_row <= 18, ]$block <- 4
  df[!is_sw & irr_row <= 9 & irr_col <= 14, ]$block <- 5
  df[!is_sw & irr_row <= 8, ]$block <- 5
  df[is_blank, ]$block <- -1

  for (block in 1:5) {
    df[df$block == block, ]$treatment <- 1:n_treatments_irr
  }
  df[is_blank, ]$block <- NA
  df$block <- factor(df$block)

  return(df)
}

df_initial_irr <- initialise_df_irr()

# block 1 spans both sites, shuffle in advance for digger and odw
sample_block_1 <- function(df, seed) {
  set.seed(seed)
  is_block_1 <- !is.na(df$block) & df$block == "1"
  df[is_block_1, ]$treatment <- sample(df[is_block_1, ]$treatment)

  return(df)
}

# speed
bench_speed_irr <- function(seed = 112) {
  speed::speed(
    data = df_initial_irr,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ site_row + site_col,
    optimise_params = optim_params(random_initialisation = 10, adj_weight = 0),
    seed = seed
  )
}

# digger - one site at a time, blanks are 0
digger_site <- function(df_site, seed) {
  planted <- !is.na(df_site$block)
  cells <- cbind(df_site$row[planted], df_site$col[planted])
  treatments <- as.integer(df_site$treatment[planted])

  initial <- matrix(0, nrow = max(df_site$row), ncol = max(df_site$col))
  initial[cells] <- treatments

  # like swap_within
  swap <- matrix(0, nrow = nrow(initial), ncol = ncol(initial))
  swap[cells] <- as.integer(df_site$block[planted])

  block_nrows <- max(as.integer(df_site$row))

  digger <- DiGGer::corDiGGer(
    numberOfTreatments = n_treatments_irr,
    rowsInDesign = nrow(initial),
    columnsInDesign = ncol(initial),
    treatRepPerRep = tabulate(treatments, n_treatments_irr),
    initialDesign = initial,
    initialSwap = swap,
    blockSequence = list(c(block_nrows, 1)),
    maxInterchanges = c(50000, 200000),
    rngSeeds = rep(seed, 2)
  )
  df_site$treatment[planted] <- as.character(
    DiGGer::getDesign(digger)[cells]
  )

  return(df_site)
}

bench_digger_irr <- function(seed = 112) {
  df_digger_irr <- sample_block_1(df_initial_irr, seed)

  is_sw <- df_digger_irr$site == "sw"
  df_digger_irr[is_sw, ] <- digger_site(df_digger_irr[is_sw, ], seed)
  df_digger_irr[!is_sw, ] <- digger_site(df_digger_irr[!is_sw, ], seed)

  return(df_digger_irr)
}

# odw - one site at a time, blanks are dropped
odw_site <- function(df_site, maxit = 3) {
  planted <- !is.na(df_site$block)
  df_fit <- to_factor(
    df_site[planted, ],
    c("treatment", "block", "row", "col")
  ) |>
    speed:::shuffle_items("treatment", "block", 112)

  odw_random_irr <- ~ treatment + row + col
  odw_search <- "tabu+rw"
  param_table <- odw::odw(
    random = odw_random_irr,
    data = df_fit,
    permute = ~treatment,
    swap = ~block,
    search = odw_search,
    start.values = TRUE
  )$vparameters.table

  param_table[2:3, 2] <- 100

  design <- odw::odw(
    random = odw_random_irr,
    data = df_fit,
    permute = ~treatment,
    swap = ~block,
    search = odw_search,
    G.param = param_table,
    R.param = param_table,
    maxit = maxit
  )$design

  # match on plot position in case odw returns its own row order
  df_site$treatment[planted] <- as.character(design$treatment)[match(
    paste(df_site$row[planted], df_site$col[planted]),
    paste(design$row, design$col)
  )]

  return(df_site)
}

bench_odw_irr <- function(seed = 112, maxit = 3) {
  df_odw_irr <- sample_block_1(df_initial_irr, seed)

  is_sw <- df_odw_irr$site == "sw"
  df_odw_irr[is_sw, ] <- odw_site(df_odw_irr[is_sw, ], maxit)
  df_odw_irr[!is_sw, ] <- odw_site(df_odw_irr[!is_sw, ], maxit)

  return(df_odw_irr)
}

irr_design <- function() {
  return(list(
    units = ~ site / (block + row * col),
    treatment = "treatment",
    is_converged = function(df) TRUE,
    custom_metrics = function(df) {
      get_metrics_irr(df, c("site_col", "site_row", "block"))
    },
    tools = list(
      speed = function(seed) bench_speed_irr(seed)$design_df,
      digger = function(seed) bench_digger_irr(seed),
      odw = function(seed) bench_odw_irr()
    )
  ))
}

explore_irr <- function() {
  png(bench_out("layout-irr.png"), height = 1000, width = 1600)
  print(autoplot_irr(df_initial_irr))
  dev.off()

  design_df <- bench_speed_irr()$design_df
  get_metrics_irr(design_df, c("site_col", "site_row", "block"))
  print(efficiency(design_df, "treatment", ~ site / (block + row * col)))
  #  Source.units  df1  Source.treatments df2  aefficiency eefficiency order
  #  site             1 treatment            1      0.0336      0.0336     1
  #  block[site]      4 treatment            1      0.1664      0.1664     1
  #                     Residual             3
  #  row[site]       41 treatment           41      0.1716      0.0843    41
  #  col[site]       50 treatment           50      0.1503      0.0536    50
  #  row#col[site] 1013 treatment          221      0.9003      0.5465    93
  #                     Residual           792

  png(bench_out("speed-irr.png"), height = 1000, width = 1600)
  print(autoplot_irr(design_df))
  dev.off()

  digger_result_irr <- bench_digger_irr()
  get_metrics_irr(digger_result_irr, c("site_col", "site_row", "block"))
  print(efficiency(
    digger_result_irr,
    "treatment",
    ~ site / (block + row * col)
  ))
  # Source.units  df1  Source.treatments df2  aefficiency eefficiency order
  # site             1 treatment            1      0.0336      0.0336     1
  # block[site]      4 treatment            1      0.1664      0.1664     1
  #                    Residual             3
  # row[site]       41 treatment           41      0.1718      0.0790    41
  # col[site]       50 treatment           50      0.1651      0.0762    50
  # row#col[site] 1013 treatment          221      0.9024      0.6024    93
  #                    Residual           792

  png(bench_out("digger-irr.png"), height = 1000, width = 1600)
  print(autoplot_irr(digger_result_irr))
  dev.off()

  odw_result_irr <- bench_odw_irr()
  get_metrics_irr(odw_result_irr, c("site_col", "site_row", "block"))
  print(efficiency(odw_result_irr, "treatment", ~ site / (block + row * col)))
  # not reproducible
  # Source.units  df1  Source.treatments df2  aefficiency eefficiency order
  # site             1 treatment            1      0.0336      0.0336     1
  # block[site]      4 treatment            1      0.1664      0.1664     1
  #                     Residual             3
  # row[site]       41 treatment           41      0.1773      0.0877    41
  # col[site]       50 treatment           50      0.1589      0.0725    50
  # row#col[site] 1013 treatment          221      0.9032      0.5991    93
  #                    Residual           792

  png(bench_out("odw-irr.png"), height = 1000, width = 1600)
  print(autoplot_irr(odw_result_irr))
  dev.off()

  return(invisible(NULL))
}
