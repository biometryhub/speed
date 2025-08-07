#######################################################
# 15 treatments, 5 reps, 25 rows, 3 columns
n_treatments <- 15
n_reps <- 5
n_rows <- 25
n_cols <- 3

# speed
df_initial <- speed::initialise_design_df(rep(1:n_treatments, n_reps), n_rows, n_cols, 5, 3)

options(speed.random_initialisation = TRUE)
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

png("speed-15x5.png", height = 1080, width = 720)
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

df_digger <- df_initial
df_digger$treatment <- c(digger_design)
unique(table(df_digger$treatment, df_digger$row))
unique(table(df_digger$treatment, df_digger$col))

digger_result <- speed_result
digger_result$design_df <- df_digger

png("digger-15x5.png", height = 1080, width = 720)
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

png("odw-15x5.png", height = 1080, width = 720)
speed::autoplot(odw_result)
dev.off()

bench_result <- bench::mark(
  check = FALSE,
  iterations = 10,
  speed = bench_speed(),
  odw = bench_odw()
)

#######################################################
# 10 treatments, 40 reps, 50 rows, 8 columns
n_treatments <- 10
n_reps <- 40
n_rows <- 50
n_cols <- 8

# speed
df_initial <- speed::initialise_design_df(rep(1:n_treatments, n_reps), n_rows, n_cols, 2, 4)

options(speed.random_initialisation = FALSE)
bench_speed <- function() {
  speed::speed(
    data = df_initial,
    swap = "treatment",
    swap_within = "col",
    spatial_factors = ~ row + block,
    iterations = 200000,
    early_stop_iterations = 5000,
    seed = 112,
    quiet = FALSE
  )
}
speed_result <- bench_speed()

design_df <- speed_result$design_df
unique(table(design_df$treatment, design_df$row))
unique(table(design_df$treatment, design_df$col))
unique(table(design_df$treatment, design_df$block))

png("speed-50x8.png", height = 1080, width = 720)
speed::autoplot(speed_result)
dev.off()

# digger
bench_digger <- function(variables) {
  DiGGer::ibDiGGer(
    numberOfTreatments = n_treatments,
    rowsInDesign = n_rows,
    columnsInDesign = n_cols,
    rowsInBlock = 2,
    columnsInBlock = 4,
    rowsInRep = 10,
    columnsInRep = 1,
    maxInterchanges = 50000,
    rngSeeds = c(112, 112)
  )
}
digger_design <- DiGGer::getDesign(bench_digger())
df_digger <- df_initial
df_digger$treatment <- c(digger_design)
unique(table(df_digger$treatment, df_digger$row))
unique(table(df_digger$treatment, df_digger$col))
unique(table(df_digger$treatment, df_digger$block))

digger_result <- speed_result
digger_result$design_df <- df_digger
png("digger-50x8.png", height = 1080, width = 720)
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

png("odw-50x8.png", height = 1080, width = 720)
speed::autoplot(odw_result)
dev.off()

bench_result <- bench::mark(
  check = FALSE,
  iterations = 10,
  speed = bench_speed(),
  odw = bench_odw()
)
