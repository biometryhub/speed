random_treatments <- function(df, seed = NULL) {
  # Set seed for reproducibility
  if (is.null(seed)) {
    # dummy_seed <- runif(1)
    seed <- .GlobalEnv$.Random.seed[3]
  }
  set.seed(seed)

  for (i in unique(df$block)) {
    df[df$block == i, ]$treatment <- sample(df[df$block == i, ]$treatment)
  }

  return(df)
}

# 10 treatments, 6 reps, 20 rows, 3 columns, no blocking
n_treatments <- 15
n_reps <- 5
n_rows <- 25
n_cols <- 3

# speed
treatments <- 1:n_treatments
df_initial <- speed::initialise_design_df(rep(treatments, n_reps), n_rows, n_cols, 5, 3)
df_initial <- random_treatments(df_initial, 112)

bench_speed <- function() {
  speed::speed(
    data = df_initial,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~col,
    iterations = 200000,
    early_stop_iterations = 1000,
    seed = 112,
    quiet = FALSE
  )
}
speed_result <- bench_speed()
design_df <- speed_result$design_df

unique(table(design_df$treatment, design_df$row))
unique(table(design_df$treatment, design_df$col))

png("speed-15x5.png")
speed::autoplot(speed_result)
dev.off()

# digger
bench_digger <- function(variables) {
  DiGGer::corDiGGer(
    numberOfTreatments = n_treatments,
    rowsInDesign = n_rows,
    columnsInDesign = n_cols,
    rowsInReplicate = 5,
    columnsInReplicate = 3,
    treatRepPerRep = 1,
    blockSequence = list(c(12, 1)),
    maxInterchanges = 500000,
    rngSeeds = c(112, 112)
  )
}
digger_design <- DiGGer::getDesign(bench_digger())

df_digger <- data.frame(
  row = rep(1:n_rows, n_cols),
  col = rep(1:n_cols, each = n_rows),
  treatment = c(digger_design)
)

unique(table(df_digger$treatment, df_digger$row))
unique(table(df_digger$treatment, df_digger$col))

digger_result <- speed_result
digger_result$design_df <- df_digger

png("digger-15x5.png")
speed::autoplot(digger_result)
dev.off()

bench_result <- bench::mark(
  check = FALSE,
  iterations = 10,
  speed = bench_speed(),
  digger = bench_digger()
)


# odw
df_initial$treatment <- as.factor(df_initial$treatment)
df_initial$block <- as.factor(df_initial$block)
df_initial$row <- as.factor(df_initial$row)
df_initial$col <- as.factor(df_initial$col)

initial_param_table <- odw::odw(
  random = ~ treatment + col + block,
  data = df_initial,
  permute = ~treatment,
  swap = ~block,
  search = "tabu",
  start.values = TRUE
)$vparameters.table
initial_param_table

initial_param_table[2, 2] <- 100
initial_param_table

bench_odw <- function() {
  odw::odw(
    random = ~ treatment + col + block,
    data = df_initial,
    permute = ~treatment,
    swap = ~block,
    search = "tabu",
    G.param = initial_param_table,
    R.param = initial_param_table,
    maxit = 2
  )
}
design_object <- bench_odw()

df_odw <- design_object$design
df_odw$row <- as.numeric(df_odw$row)
df_odw$col <- as.numeric(df_odw$col)
df_odw$block <- as.numeric(df_odw$block)
odw_result <- speed_result
odw_result$design_df <- df_odw

unique(table(df_odw$treatment, df_odw$row))
unique(table(df_odw$treatment, df_odw$col))

png("odw-15x5.png")
speed::autoplot(odw_result)
dev.off()

bench_result <- bench::mark(
  check = FALSE,
  iterations = 10,
  speed = bench_speed(),
  odw = bench_odw()
)
