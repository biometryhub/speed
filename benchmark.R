source("./R/design_utils.R")
source("./R/utils.R")
source("./R/speed.R")
source("./R/metrics.R")
source("./R/verify_utils.R")

# 10 treatments 40 reps, 20 rows, 20 columns, no blocking
# digger
bench_digger <- function(variables) {
  DiGGer::ibDiGGer(
    numberOfTreatments = 10,
    rowsInDesign = 20,
    columnsInDesign = 20,
    rowsInBlock = 1,
    columnsInBlock = 20,
    rowsInReplicate = 20,
    columnsInReplicate = 1,
    treatRepPerRep = 2,
    maxInterchanges = 100000,
    rngSeeds = c(112, 112)
  )
}
digger_design <- DiGGer::getDesign(bench_digger())

desTab(digger_design, 20, 1)
desTab(digger_design, 1, 20)


# speed
df_initial <- initialise_design_df(rep(1:10, 40), 20, 20)

bench_speed <- function() {
  speed(
    data = df_initial,
    swap = "treatment",
    swap_within = "col",
    spatial_factors = ~row,
    iterations = 200000,
    early_stop_iterations = 10000,
    seed = 112,
    quiet = FALSE
  )
}
design_df <- bench_speed()$design_df

max(table(design_df$treatment, design_df$row))
max(table(design_df$treatment, design_df$col))

bench_result <- bench::mark(
  check = FALSE,
  iterations = 10,
  speed = bench_speed(),
  digger = bench_digger()
)

# 10 treatments 40 reps, 20 rows, 20 columns, 2x2 blocking
# speed
df_initial <- initialise_design_df(rep(1:10, 40), 20, 20, 2, 2)

speed_design <- speed(
  data = df_initial,
  swap = "treatment",
  swap_within = "col",
  spatial_factors = ~ row + block,
  iterations = 200000,
  early_stop_iterations = 30000,
  seed = 112,
  quiet = FALSE
)
design_df <- speed_design$design_df

max(table(design_df$treatment, design_df$row))
max(table(design_df$treatment, design_df$col))
max(table(design_df$treatment, design_df$block))
