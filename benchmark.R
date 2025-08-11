shuffle_items <- function(design, swap, swap_within, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  for (i in unique(design[[swap_within]])) {
    items <- design[design[[swap_within]] == i, ][[swap]]
    design[design[[swap_within]] == i, ][[swap]] <- sample(items)
  }

  return(design)
}

#######################################################
# 15 treatments, 5 reps, 25 rows, 3 columns
n_treatments <- 15
n_reps <- 5
n_rows <- 25
n_cols <- 3

# speed
df_initial <- speed::initialise_design_df(rep(1:n_treatments, n_reps), n_rows, n_cols, 5, 3)
df_initial$treatment <- as.factor(df_initial$treatment)
df_initial$block <- as.factor(df_initial$block)
df_initial$row <- as.factor(df_initial$row)
df_initial$col <- as.factor(df_initial$col)

options(speed.random_initialisation = TRUE)
bench_speed <- function() {
  speed::speed(
    data = df_initial,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~col,
    seed = 112
  )
}
speed_result <- bench_speed()

design_df <- speed_result$design_df
speed_result$design_df$row <- as.numeric(speed_result$design_df$row)
speed_result$design_df$col <- as.numeric(speed_result$design_df$col)
speed_result$design_df$block <- as.numeric(speed_result$design_df$block)
unique(table(design_df$treatment, design_df$row))
unique(table(design_df$treatment, design_df$col))
speed::calculate_adjacency_score(design_df, "treatment")

png("speed-15x5.png", height = 1080, width = 480)
speed::autoplot(speed_result)
dev.off()

df_layout <- df_initial[order(df_initial$block), ]
df_layout$plot_in_block <- df_initial$treatment
df_layout$row <- as.numeric(df_layout$row)
df_layout$col <- as.numeric(df_layout$col)
class(df_layout) <- c(class(df_layout), "design")
png("layout-15x5.png", height = 1080, width = 480)
speed::autoplot(df_layout, treatments = "plot_in_block")
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
df_digger$block <- as.numeric(df_digger$block)
df_digger$row <- as.numeric(df_digger$row)
df_digger$col <- as.numeric(df_digger$col)
unique(table(df_digger$treatment, df_digger$row))
unique(table(df_digger$treatment, df_digger$col))
speed::calculate_adjacency_score(df_digger, "treatment")

digger_result <- speed_result
digger_result$design_df <- df_digger

png("digger-15x5.png", height = 1080, width = 480)
speed::autoplot(digger_result)
dev.off()

# odw
df_initial_odw <- speed::initialise_design_df(rep(1:n_treatments, n_reps), n_rows, n_cols, 5, 3)
df_initial_odw <- shuffle_items(df_initial_odw, "treatment", "block", 112)
df_initial_odw$treatment <- as.factor(df_initial_odw$treatment)
df_initial_odw$block <- as.factor(df_initial_odw$block)
df_initial_odw$row <- as.factor(df_initial_odw$row)
df_initial_odw$col <- as.factor(df_initial_odw$col)

initial_param_table <- odw::odw(
  random = ~ treatment + col + block,
  data = df_initial_odw,
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
    data = df_initial_odw,
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
speed::calculate_adjacency_score(df_odw, "treatment")

unique(table(df_odw$treatment, df_odw$row))
unique(table(df_odw$treatment, df_odw$col))

png("odw-15x5.png", height = 1080, width = 480)
speed::autoplot(odw_result)
dev.off()

bench_result <- bench::mark(
  check = FALSE,
  iterations = 10,
  speed = bench_speed(),
  digger = bench_digger(),
  odw = bench_odw()
)

png("bench-15x5.png", height = 720, width = 720)
ggplot2::autoplot(bench_result, type = "boxplot")
dev.off()

#######################################################
# 40 treatments, 10 reps, 20 rows, 20 columns
n_treatments <- 40
n_reps <- 10
n_rows <- 20
n_cols <- 20

# speed
df_initial <- speed::initialise_design_df(rep(1:n_treatments, n_reps), n_rows, n_cols, 2, 20)
df_dummy <- speed::initialise_design_df(rep(1:n_treatments, n_reps), n_rows, n_cols, 20, 2)
df_initial$row_block <- as.factor(df_initial$block)
df_initial$col_block <- as.factor(df_dummy$block)
df_initial$treatment <- as.factor(df_initial$treatment)
df_initial$row <- as.factor(df_initial$row)
df_initial$col <- as.factor(df_initial$col)

options(
  speed.random_initialisation = FALSE,
  speed.adaptive_swaps = TRUE,
  speed.swap_count = 3
)
bench_speed <- function() {
  speed::speed(
    data = df_initial,
    swap = "treatment",
    swap_within = "col_block",
    spatial_factors = ~row_block,
    iterations = 200000,
    early_stop_iterations = 10000,
    seed = 112
  )
}
speed_result <- bench_speed()
speed_result$score

design_df <- speed_result$design_df
speed_result$design_df$row <- as.numeric(design_df$row)
speed_result$design_df$col <- as.numeric(design_df$col)
speed_result$design_df$block <- as.numeric(design_df$block)
unique(table(design_df$treatment, design_df$row_block))
unique(table(design_df$treatment, design_df$col_block))
speed::calculate_adjacency_score(design_df, "treatment")

png("speed-20x20.png", height = 720, width = 720)
speed::autoplot(speed_result)
dev.off()

df_layout <- df_initial
df_layout$row <- as.numeric(df_layout$row)
df_layout$col <- as.numeric(df_layout$col)
class(df_layout) <- c(class(df_layout), "design")
png("layout-20x20.png", height = 720, width = 720)
speed::autoplot(df_layout, treatments = "col_block")
dev.off()

# digger
bench_digger <- function(variables) {
  DiGGer::ibDiGGer(
    numberOfTreatments = n_treatments,
    rowsInDesign = n_rows,
    columnsInDesign = n_cols,
    rowsInBlock = 20,
    columnsInBlock = 2,
    rowsInRep = 2,
    columnsInRep = 20,
    maxInterchanges = 700000,
    rngSeeds = c(112, 112)
  )
}
digger_design <- DiGGer::getDesign(bench_digger())
df_digger <- df_initial
df_digger$row <- as.numeric(df_digger$row)
df_digger$col <- as.numeric(df_digger$col)
df_digger$block <- as.numeric(df_digger$block)
df_digger$treatment <- c(digger_design)
unique(table(df_digger$treatment, df_digger$row_block))
unique(table(df_digger$treatment, df_digger$col_block))
speed::calculate_adjacency_score(df_digger, "treatment")

digger_result <- speed_result
digger_result$design_df <- df_digger
png("digger-20x20.png", height = 720, width = 720)
speed::autoplot(digger_result)
dev.off()

# odw
# df_initial_odw <- speed::initialise_design_df(as.factor(rep(1:n_treatments, n_reps)), n_rows, n_cols, 2, 2)
# df_initial_odw$treatment <- as.factor(df_initial_odw$treatment)
# df_initial_odw$row_block <- as.factor(df_initial_odw$row_block)
# df_initial_odw$col_block <- as.factor(df_initial_odw$col_block)
# df_initial_odw$block <- as.factor(df_initial_odw$block)
# df_initial_odw$row <- as.factor(df_initial_odw$row)
# df_initial_odw$col <- as.factor(df_initial_odw$col)
df_initial_odw <- df_initial
df_initial_odw <- shuffle_items(df_initial_odw, "treatment", "col_block", 112)

initial_param_table <- odw::odw(
  random = ~ treatment + col_block + row_block,
  data = df_initial,
  permute = ~treatment,
  swap = ~col_block,
  search = "tabu",
  start.values = TRUE
)$vparameters.table
initial_param_table

initial_param_table[2:3, 2] <- 100
initial_param_table

bench_odw <- function() {
  odw::odw(
    random = ~ treatment + col_block + row_block,
    data = df_initial_odw,
    permute = ~treatment,
    swap = ~col_block,
    search = "tabu",
    G.param = initial_param_table,
    R.param = initial_param_table,
    maxit = 8
  )
}
design_object <- bench_odw()

df_odw <- design_object$design
df_odw$row <- as.numeric(df_odw$row)
df_odw$col <- as.numeric(df_odw$col)
df_odw$block <- as.numeric(df_odw$block)
odw_result <- speed_result
odw_result$design_df <- df_odw
unique(table(df_odw$treatment, df_odw$col_block))
unique(table(df_odw$treatment, df_odw$row_block))
speed::calculate_adjacency_score(df_odw, "treatment")

png("odw-20x20.png", height = 720, width = 720)
speed::autoplot(odw_result)
dev.off()

bench_result <- bench::mark(
  check = FALSE,
  iterations = 10,
  speed = bench_speed(),
  digger = bench_digger(),
  odw = bench_odw()
)

png("bench-20x20.png", height = 720, width = 720)
ggplot2::autoplot(bench_result, type = "boxplot")
dev.off()
