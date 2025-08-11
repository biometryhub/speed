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

#######################################################
# split plot
options(
  speed.random_initialisation = FALSE,
  speed.adaptive_swaps = FALSE,
  speed.swap_count = 1
)

n_subplot_treatments <- 4
n_subplot_reps <- 12
n_wholeplot_treatments <- 3
n_wholeplot_reps <- 4
block_nrows <- 3
block_ncols <- 4
wholeplot_nrows <- 1
wholeplot_ncols <- 4
n_rows <- 12
n_cols <- 4

# speed
df_initial <- speed::initialise_design_df(
  rep(letters[1:n_subplot_treatments], each = n_subplot_reps),
  n_rows,
  n_cols,
  wholeplot_nrows,
  wholeplot_ncols
)
df_dummy <- speed::initialise_design_df(
  rep(
    rep(LETTERS[1:n_wholeplot_treatments], times = n_wholeplot_reps),
    times = n_rows * n_cols / n_wholeplot_reps / n_wholeplot_treatments
  ),
  n_rows,
  n_cols,
  block_nrows,
  block_ncols
)
df_initial$wholeplot <- as.factor(df_initial$block)
df_initial$block <- as.factor(df_dummy$block)
df_initial$wholeplot_treatment <- as.factor(df_dummy$treatment)
df_initial$subplot_treatment <- as.factor(df_initial$treatment)
df_initial$row <- as.factor(df_initial$row)
df_initial$col <- as.factor(df_initial$col)

# save layout
df_layout <- df_initial
df_layout$row <- as.numeric(df_layout$row)
df_layout$col <- as.numeric(df_layout$col)
class(df_layout) <- c(class(df_layout), "design")
png("layout-split-whole.png", height = 720, width = 240)
speed::autoplot(df_layout, treatments = "wholeplot_treatment")
dev.off()
png("layout-split-sub.png", height = 720, width = 240)
speed::autoplot(df_layout, treatments = "subplot_treatment", block = "wholeplot")
dev.off()

bench_speed <- function() {
  speed::speed(
    data = df_initial,
    swap = list(wholeplot = "wholeplot_treatment", subplot = "subplot_treatment"),
    swap_within = list(wholeplot = "block", subplot = "wholeplot"),
    spatial_factors = ~col,
    early_stop_iterations = list(wholeplot = 1000, subplot = 10000), ,
    seed = 13
  )
}
speed_result <- bench_speed()
speed_result$score

design_df <- speed_result$design_df
speed_result$design_df$row <- as.numeric(design_df$row)
speed_result$design_df$col <- as.numeric(design_df$col)
speed_result$design_df$block <- as.numeric(design_df$block)
speed_result$design_df$wholeplot <- as.numeric(design_df$wholeplot)
unique(table(design_df$wholeplot_treatment, design_df$block))
unique(table(design_df$subplot_treatment, design_df$wholeplot))
speed::calculate_adjacency_score(design_df, "subplot_treatment")

png("speed-split-whole.png", height = 720, width = 240)
speed::autoplot(speed_result, treatments = "wholeplot_treatment")
dev.off()
png("speed-split-sub.png", height = 720, width = 240)
speed::autoplot(speed_result, treatments = "subplot_treatment", block = "wholeplot")
dev.off()

# for presentation
# df_initial <- speed::initialise_design_df(rep(letters[1:4], each = 12), 12, 4, 3, 4)
# df_dummy <- speed::initialise_design_df(rep(rep(LETTERS[1:3], times = 4), times = 4), 12, 4, 1, 4)
# df_initial$wholeplot <- df_dummy$block
# df_initial$wholeplot_treatment <- df_dummy$treatment
# df_initial$subplot_treatment <- df_initial$treatment
#
# speed_result <- speed::speed(
#   data = df_initial,
#   swap = list(wholeplot = "wholeplot_treatment", subplot = "subplot_treatment"),
#   swap_within = list(wholeplot = "block", subplot = "wholeplot"),
#   spatial_factors = ~col,
#   early_stop_iterations = list(wholeplot = 1000, subplot = 10000),
#   seed = 13
# )

# digger
bench_digger_whole <- function(variables) {
  DiGGer::ibDiGGer(
    numberOfTreatments = n_wholeplot_treatments,
    rowsInDesign = n_rows,
    columnsInDesign = 1,
    rowsInBlock = 3,
    columnsInBlock = 1,
    maxInterchanges = 1000,
    rngSeeds = c(112, 112)
  )
}
bench_digger_sub <- function(variables) {
  DiGGer::ibDiGGer(
    numberOfTreatments = n_subplot_treatments,
    rowsInDesign = n_rows,
    columnsInDesign = n_cols,
    rowsInBlock = wholeplot_nrows,
    columnsInBlock = wholeplot_ncols,
    rowsInRep = 12,
    columnsInRep = 1,
    maxInterchanges = 1000,
    rngSeeds = c(112, 112)
  )
}
digger_design <- DiGGer::getDesign(bench_digger_sub())
digger_dummy <- DiGGer::getDesign(bench_digger_whole())

df_digger <- df_initial
df_digger$row <- as.numeric(df_digger$row)
df_digger$col <- as.numeric(df_digger$col)
df_digger$block <- as.numeric(df_digger$block)
df_digger$wholeplot <- as.numeric(df_digger$wholeplot)
df_digger$subplot_treatment <- letters[c(digger_design)]
df_digger$wholeplot_treatment <- rep(LETTERS[c(digger_dummy)], times = n_cols)
unique(table(df_digger$wholeplot_treatment, df_digger$block))
unique(table(df_digger$subplot_treatment, df_digger$wholeplot))
speed::calculate_adjacency_score(df_digger, "subplot_treatment")

digger_result <- speed_result
digger_result$design_df <- df_digger
png("digger-split-whole.png", height = 720, width = 240)
speed::autoplot(digger_result, treatments = "wholeplot_treatment")
dev.off()
png("digger-split-sub.png", height = 720, width = 240)
speed::autoplot(digger_result, treatments = "subplot_treatment", block = "wholeplot")
dev.off()

# for presentation
# digger_dummy <- DiGGer::ibDiGGer(
#   numberOfTreatments = 3,
#   rowsInDesign = 12,
#   columnsInDesign = 1,
#   rowsInBlock = 3,
#   columnsInBlock = 1,
#   maxInterchanges = 1000,
#   rngSeeds = c(112, 112)
# ) |> DiGGer::getDesign()
#
# digger_design <- DiGGer::ibDiGGer(
#   numberOfTreatments = 4,
#   rowsInDesign = 12,
#   columnsInDesign = 4,
#   rowsInBlock = 1,
#   columnsInBlock = 4,
#   rowsInRep = 12,
#   columnsInRep = 1,
#   maxInterchanges = 1000,
#   rngSeeds = c(112, 112)
# ) |> DiGGer::getDesign()
#
# df_digger <- speed::initialise_design_df(letters[c(digger_design)], 12, 4, 1, 4)
# df_dummy <- speed::initialise_design_df(rep(rep(1:3, times = 4), times = 4), 12, 4, 3, 4)
# df_digger$wholeplot <- df_digger$block
# df_digger$block <- df_dummy$block
# df_digger$subplot_treatment <- df_digger$treatment
# df_digger$wholeplot_treatment <- rep(LETTERS[c(digger_dummy)], times = 4)

# odw
df_initial_odw <- speed::initialise_design_df(
  as.factor(rep(letters[1:n_subplot_treatments], each = n_subplot_reps)),
  n_rows,
  n_cols,
  wholeplot_nrows,
  wholeplot_ncols
)
df_dummy_odw <- speed::initialise_design_df(
  as.factor(rep(LETTERS[1:n_wholeplot_treatments], n_wholeplot_reps)),
  n_rows,
  1,
  block_nrows,
  1
)
df_initial_odw$wholeplot <- as.factor(df_initial_odw$block)
df_initial_odw$row <- as.factor(df_initial_odw$row)
df_initial_odw$col <- as.factor(df_initial_odw$col)
df_dummy_odw$block <- as.factor(df_dummy_odw$block)
df_dummy_odw$row <- as.factor(df_dummy_odw$row)
df_dummy_odw$col <- as.factor(df_dummy_odw$col)
df_dummy_odw <- shuffle_items(df_dummy_odw, "treatment", "block", 112)

initial_param_table_dummy <- odw::odw(
  random = ~ treatment + row + block,
  data = df_dummy_odw,
  permute = ~treatment,
  swap = ~block,
  search = "tabu",
  start.values = TRUE
)$vparameters.table
initial_param_table_dummy

initial_param_table_dummy[2, 2] <- 100
initial_param_table_dummy

bench_odw_dummy <- function() {
  odw::odw(
    random = ~ treatment + row + block,
    data = df_dummy_odw,
    permute = ~treatment,
    swap = ~block,
    search = "tabu",
    G.param = initial_param_table_dummy,
    R.param = initial_param_table_dummy,
    maxit = 2
  )
}
design_object_dummy <- bench_odw_dummy()

initial_param_table <- odw::odw(
  random = ~ treatment + row + col,
  data = df_initial_odw,
  permute = ~treatment,
  swap = ~row,
  search = "tabu",
  start.values = TRUE
)$vparameters.table
initial_param_table

initial_param_table[2:3, 2] <- 100
initial_param_table

bench_odw <- function() {
  odw::odw(
    random = ~ treatment + row + col,
    data = df_initial_odw,
    permute = ~treatment,
    swap = ~row,
    search = "tabu",
    G.param = initial_param_table,
    R.param = initial_param_table,
    maxit = 8
  )
}
design_object <- bench_odw()

df_odw <- design_object$design
df_odw_dummy <- design_object_dummy$design
df_odw$row <- as.numeric(df_odw$row)
df_odw$col <- as.numeric(df_odw$col)
df_odw$subplot_treatment <- df_odw$treatment
df_odw$wholeplot_treatment <- rep(LETTERS[df_odw_dummy$treatment], n_cols)
df_odw$block <- rep(df_odw_dummy$block, n_cols)
odw_result <- speed_result
odw_result$design_df <- df_odw
unique(table(df_odw$wholeplot_treatment, df_odw$block))
unique(table(df_odw$subplot_treatment, df_odw$wholeplot))
speed::calculate_adjacency_score(df_odw, "subplot_treatment")

png("odw-split-whole.png", height = 720, width = 240)
speed::autoplot(odw_result, treatments = "wholeplot_treatment")
dev.off()
png("odw-split-sub.png", height = 720, width = 240)
speed::autoplot(odw_result, treatments = "subplot_treatment", block = "wholeplot")
dev.off()

bench_result <- bench::mark(
  check = FALSE,
  iterations = 10,
  speed = bench_speed(),
  digger = {
    bench_digger_sub()
    bench_digger_whole()
  },
  odw = {
    bench_odw_dummy()
    bench_odw()
  }
)

png("bench-split.png", height = 720, width = 720)
ggplot2::autoplot(bench_result, type = "boxplot")
dev.off()

# for presentation
# df_initial_odw <- speed::initialise_design_df(as.factor(rep(letters[1:4], each = 12)), 12, 4, 1, 4)
# df_dummy_odw <- speed::initialise_design_df(as.factor(rep(LETTERS[1:3], 4)), 12, 1, 3, 1)
# df_initial_odw$wholeplot <- as.factor(df_initial_odw$block)
# df_initial_odw$row <- as.factor(df_initial_odw$row)
# df_initial_odw$col <- as.factor(df_initial_odw$col)
# df_dummy_odw$block <- as.factor(df_dummy_odw$block)
# df_dummy_odw$row <- as.factor(df_dummy_odw$row)
# df_dummy_odw$col <- as.factor(df_dummy_odw$col)
# df_dummy_odw <- shuffle_items(df_dummy_odw, "treatment", "block", 112)
#
# initial_param_table_dummy <- odw::odw(
#   random = ~ treatment + row + block,
#   data = df_dummy_odw,
#   permute = ~treatment,
#   swap = ~block,
#   search = "tabu",
#   start.values = TRUE
# )$vparameters.table
# initial_param_table_dummy
#
# initial_param_table_dummy[2, 2] <- 100
# initial_param_table_dummy
#
# design_object_dummy <- odw::odw(
#   random = ~ treatment + row + block,
#   data = df_dummy_odw,
#   permute = ~treatment,
#   swap = ~block,
#   search = "tabu",
#   G.param = initial_param_table_dummy,
#   R.param = initial_param_table_dummy,
#   maxit = 2
# )
#
# initial_param_table <- odw::odw(
#   random = ~ treatment + row + col,
#   data = df_initial_odw,
#   permute = ~treatment,
#   swap = ~row,
#   search = "tabu",
#   start.values = TRUE
# )$vparameters.table
# initial_param_table
#
# initial_param_table[2:3, 2] <- 100
# initial_param_table
#
# design_object <- odw::odw(
#   random = ~ treatment + row + col,
#   data = df_initial_odw,
#   permute = ~treatment,
#   swap = ~row,
#   search = "tabu",
#   G.param = initial_param_table,
#   R.param = initial_param_table,
#   maxit = 8
# )
#
# df_odw <- design_object$design
# df_odw_dummy <- design_object_dummy$design
# df_odw$subplot_treatment <- df_odw$treatment
# df_odw$wholeplot_treatment <- rep(df_odw_dummy$treatment, 4)
# df_odw$block <- rep(df_odw_dummy$block, 4)
