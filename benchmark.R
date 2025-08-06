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
write.csv(df_digger, "test.csv")


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
write.csv(design_df, "test.csv")

bench_result <- bench::mark(
  check = FALSE,
  iterations = 10,
  speed = bench_speed(),
  digger = bench_digger()
)

# 10 treatments 40 reps, 20 rows, 20 columns, 2x2 blocking
# speed
df_initial <- initialise_design_df(rep(1:10, 40), 20, 20, 2, 2)

speed_design <- speed::speed(
  data = df_initial,
  swap = "treatment",
  spatial_factors = ~ row + col,
  iterations = 200000,
  early_stop_iterations = 30000,
  seed = 112,
  quiet = FALSE
)
design_df <- speed_design$design_df

unique(table(design_df$treatment, design_df$row))
unique(table(design_df$treatment, design_df$col))
unique(table(design_df$treatment, design_df$block))


# # speed
# df_initial <- speed::initialise_design_df(rep(1:n_treatments, n_reps), n_rows, n_cols)
# pair_mapping <- speed::create_pair_mapping(df_initial$treatment)
#
# options(speed.adj_weight = 1)
# bench_speed <- function() {
#   speed::speed(
#     data = df_initial,
#     swap = "treatment",
#     swap_within = "col",
#     spatial_factors = ~row,
#     iterations = 200000,
#     early_stop_iterations = 50000,
#     obj_function = speed::objective_function_piepho,
#     pair_mapping = pair_mapping,
#     seed = 112,
#     quiet = FALSE
#   )
# }
# design_df_piepho <- bench_speed()$design_df
#
# unique(table(design_df_piepho$treatment, design_df_piepho$row))
# unique(table(design_df_piepho$treatment, design_df_piepho$col))

DF15 <- DiGGer::createFactorialDF(c(3, 5))
sp3x5 <- DiGGer::facDiGGer(
  factorNames = c("F1", "F2"),
  rowsInDesign = 15, columnsInDesign = 3,
  rowsInRep = 15, columnsInRep = 1,
  mainPlotSizes = list(c(5, 1), c(1, 1)),
  treatDataFrame = DF15,
  treatRepColumn = "Repeats",
  maxInt = 100000
)

plot(sp3x5, trts = 1:5, col = 5, new = TRUE, label = FALSE)
plot(sp3x5, trts = 6:10, col = 7, new = FALSE, label = FALSE)
plot(sp3x5, trts = 11:15, col = 8, new = FALSE, label = FALSE)
DiGGer::desPlot(matrix(sp3x5$dlist$F2, 15), new = FALSE)
