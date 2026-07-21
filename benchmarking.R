library(bench)
library(dae)
library(dplyr)
library(odw)
library(speed)

design_efficiency <- function(design, treatment, units = ~ row * col) {
  design <- as.data.frame(design)
  cols <- unique(c(all.vars(units), treatment))
  design[cols] <- lapply(design[cols], factor)
  anatomy <- dae::designAnatomy(
    list(units = units, treatments = stats::reformulate(treatment)),
    data = design
  )
  summary(anatomy)
}

# aefficiency / eefficiency of the lowest treatment-bearing stratum.
bottom_stratum_eff <- function(anatomy_summary) {
  d <- anatomy_summary$decomp
  keep <- which(!is.na(d$Source.treatments) & d$Source.treatments != "Residual")
  if (!length(keep)) {
    return(list(stratum = NA_character_, aeff = NA_real_, eeff = NA_real_))
  }

  i <- keep[length(keep)]
  return(list(
    stratum = d$Source.units[i],
    aeff = d$aefficiency[i],
    eeff = d$eefficiency[i]
  ))
}

run_benchmarks <- function(designs, csv = "benchmark-results.csv") {
  rows <- list()
  for (design_name in names(designs)) {
    spec <- designs[[design_name]]
    for (tool_name in names(spec$tools)) {
      tool <- spec$tools[[tool_name]]
      run <- tryCatch(
        {
          elapsed <- system.time(design_df <- tool$run())[["elapsed"]]
          list(elapsed = elapsed, design_df = design_df)
        },
        error = function(e) {
          warning(sprintf(
            "%s/%s failed: %s",
            design_name,
            tool_name,
            conditionMessage(e)
          ))
          NULL
        }
      )
      metrics <- list(conv = NA, aeff = NA_real_, eeff = NA_real_)
      if (!is.null(run)) {
        metrics <- tryCatch(
          {
            summ <- design_efficiency(run$design_df, spec$treatment, spec$units)
            eff <- bottom_stratum_eff(summ)
            list(
              conv = isTRUE(spec$is_converged(run$design_df)),
              aeff = eff$aeff,
              eeff = eff$eeff
            )
          },
          error = function(e) {
            warning(sprintf(
              "%s/%s metrics failed: %s",
              design_name,
              tool_name,
              conditionMessage(e)
            ))
            list(conv = NA, aeff = NA_real_, eeff = NA_real_)
          }
        )
      }
      rows[[length(rows) + 1L]] <- data.frame(
        tool = tool_name,
        design = design_name,
        seed = if (is.null(tool$seed)) NA_integer_ else tool$seed,
        run_time = if (is.null(run)) NA_real_ else run$elapsed,
        is_converged = metrics$conv,
        aefficiency = metrics$aeff,
        eefficiency = metrics$eeff,
        stringsAsFactors = FALSE
      )
    }
  }
  results <- do.call(rbind, rows)
  utils::write.csv(results, csv, row.names = FALSE)
  return(results)
}

designs <- list()

#######################################################
# 15 treatments, 5 reps, 25 rows, 3 columns
n_treatments_15x5 <- 15
n_reps <- 5
n_rows_15x5 <- 25
n_cols_15x5 <- 3

# speed
df_initial_15x5 <- speed::initialise_design_df(
  items = 15,
  nrows = 25,
  ncols = 3,
  block_nrows = 5,
  block_ncols = 3
)
df_initial_15x5$treatment <- as.factor(df_initial_15x5$treatment)
df_initial_15x5$block <- as.factor(df_initial_15x5$block)
df_initial_15x5$row <- as.factor(df_initial_15x5$row)
df_initial_15x5$col <- as.factor(df_initial_15x5$col)

bench_speed_15x5 <- function() {
  speed::speed(
    data = df_initial_15x5,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~col,
    optimise_params = optim_params(random_initialisation = TRUE),
    seed = 1
  )
}
speed_result <- bench_speed_15x5()

design_df <- speed_result$design_df
speed_result$design_df$row <- as.numeric(speed_result$design_df$row)
speed_result$design_df$col <- as.numeric(speed_result$design_df$col)
speed_result$design_df$block <- as.numeric(speed_result$design_df$block)
unique(table(design_df$treatment, design_df$row))
unique(table(design_df$treatment, design_df$col))
speed::calculate_adjacency_score(design_df, "treatment")
design_efficiency(design_df, "treatment", ~ (block / row) * col)
# Source.units   df1 Source.treatments df2 aefficiency eefficiency order
# block            4
# row[block]      20 treatment          14      0.0585      0.0082    14
#                    Residual            6
# col              2 treatment           2      0.0400      0.0400     1
# block#col        8 treatment           8      0.1208      0.0323     8
# row#col[block]  40 treatment          14      0.4157      0.1174    14
#                    Residual           26

png("speed-15x5.png", height = 1080, width = 480)
speed::autoplot(speed_result)
dev.off()

df_layout <- df_initial_15x5[order(df_initial_15x5$block), ]
df_layout$plot_in_block <- df_initial_15x5$treatment
df_layout$row <- as.numeric(df_layout$row)
df_layout$col <- as.numeric(df_layout$col)
class(df_layout) <- c(class(df_layout), "design")
png("layout-15x5.png", height = 500, width = 500)
speed::autoplot(df_layout, treatments = "plot_in_block")
dev.off()

# digger
bench_digger_15x5 <- function(variables) {
  DiGGer::corDiGGer(
    numberOfTreatments = n_treatments_15x5,
    rowsInDesign = n_rows_15x5,
    columnsInDesign = n_cols_15x5,
    rowsInReplicate = 5,
    columnsInReplicate = 3,
    treatRepPerRep = 1,
    blockSequence = list(c(12, 1)),
    maxInterchanges = 500000,
    rngSeeds = c(112, 112)
  )
}
digger_design <- DiGGer::getDesign(bench_digger_15x5())

df_digger <- df_initial_15x5
df_digger$treatment <- c(digger_design)
df_digger$block <- as.numeric(df_digger$block)
df_digger$row <- as.numeric(df_digger$row)
df_digger$col <- as.numeric(df_digger$col)
unique(table(df_digger$treatment, df_digger$row))
unique(table(df_digger$treatment, df_digger$col))
speed::calculate_adjacency_score(df_digger, "treatment")
design_efficiency(df_digger, "treatment", ~ (block / row) * col)
# Source.units   df1 Source.treatments df2 aefficiency eefficiency order
# block            4
# row[block]      20 treatment          14      0.1354      0.0342    14
#                    Residual            6
# col              2 treatment           2      0.0400      0.0400     1
# block#col        8 treatment           8      0.1141      0.0338     8
# row#col[block]  40 treatment          14      0.5300      0.3388    14
#                    Residual           26

digger_result <- speed_result
digger_result$design_df <- df_digger

png("digger-15x5.png", height = 500, width = 480)
speed::autoplot(digger_result)
dev.off()

# odw
df_initial_odw_15x5 <- speed::initialise_design_df(
  rep(1:n_treatments_15x5, n_reps),
  n_rows_15x5,
  n_cols_15x5,
  5,
  3
)
df_initial_odw_15x5 <- speed:::shuffle_items(
  df_initial_odw_15x5,
  "treatment",
  "block",
  112
)
df_initial_odw_15x5$treatment <- as.factor(df_initial_odw_15x5$treatment)
df_initial_odw_15x5$block <- as.factor(df_initial_odw_15x5$block)
df_initial_odw_15x5$row <- as.factor(df_initial_odw_15x5$row)
df_initial_odw_15x5$col <- as.factor(df_initial_odw_15x5$col)

initial_param_table_15x5 <- odw::odw(
  random = ~ treatment + col + block,
  data = df_initial_odw_15x5,
  permute = ~treatment,
  swap = ~block,
  search = "tabu",
  start.values = TRUE
)$vparameters.table
initial_param_table_15x5

initial_param_table_15x5[2, 2] <- 100
initial_param_table_15x5

bench_odw_15x5 <- function() {
  odw::odw(
    random = ~ treatment + col + block,
    data = df_initial_odw_15x5,
    permute = ~treatment,
    swap = ~block,
    search = "tabu",
    G.param = initial_param_table_15x5,
    R.param = initial_param_table_15x5,
    maxit = 2
  )
}
design_object <- bench_odw_15x5()

df_odw <- design_object$design
df_odw$row <- as.numeric(df_odw$row)
df_odw$col <- as.numeric(df_odw$col)
df_odw$block <- as.numeric(df_odw$block)
odw_result <- speed_result
odw_result$design_df <- df_odw
unique(table(df_odw$treatment, df_odw$row))
unique(table(df_odw$treatment, df_odw$col))
speed::calculate_adjacency_score(df_odw, "treatment")
design_efficiency(df_odw, "treatment", ~ (block / row) * col)
# Source.units   df1 Source.treatments df2 aefficiency eefficiency order
# block            4
# row[block]      20 treatment          14      0.1316      0.0297    14
#                    Residual            6
# col              2 treatment           2      0.0400      0.0400     1
# block#col        8 treatment           8      0.1107      0.0351     8
# row#col[block]  40 treatment          14      0.5000      0.2575    14
#                    Residual           26

png("odw-15x5.png", height = 500, width = 500)
speed::autoplot(odw_result)
dev.off()

designs[["15x5"]] <- list(
  units = ~ (block / row) * col,
  treatment = "treatment",
  is_converged = function(df) TRUE,
  tools = list(
    speed = list(seed = 1, run = function() bench_speed_15x5()$design_df),
    digger = list(seed = 112, run = function() {
      d <- df_initial_15x5
      d$treatment <- c(DiGGer::getDesign(bench_digger_15x5()))
      d
    }),
    odw = list(seed = 112, run = function() bench_odw_15x5()$design)
  )
)

bench_result <- bench::mark(
  check = FALSE,
  iterations = 10,
  speed = bench_speed_15x5(),
  digger = bench_digger_15x5(),
  odw = bench_odw_15x5()
)

png("bench-15x5.png", height = 720, width = 720, res = 300)
ggplot2::autoplot(bench_result, type = "violin") + ggplot2::theme_bw()
dev.off()

#######################################################
# 40 treatments, 10 reps, 20 rows, 20 columns
n_treatments_20x20 <- 40
n_reps <- 10
n_rows_20x20 <- 20
n_cols_20x20 <- 20

# speed
df_initial_20x20 <- speed::initialise_design_df(
  n_treatments_20x20,
  n_rows_20x20,
  n_cols_20x20,
  2,
  20
)
df_dummy <- speed::initialise_design_df(
  n_treatments_20x20,
  n_rows_20x20,
  n_cols_20x20,
  20,
  2
)
df_initial_20x20$row_block <- as.factor(df_initial_20x20$block)
df_initial_20x20$col_block <- as.factor(df_dummy$block)
df_initial_20x20$treatment <- as.factor(df_initial_20x20$treatment)
df_initial_20x20$row <- as.factor(df_initial_20x20$row)
df_initial_20x20$col <- as.factor(df_initial_20x20$col)

df_layout <- df_initial_20x20
df_layout$row <- as.numeric(df_layout$row)
df_layout$col <- as.numeric(df_layout$col)
class(df_layout) <- c(class(df_layout), "design")
png("layout-20x20.png", height = 720, width = 720)
speed::autoplot(df_layout)
dev.off()

bench_speed_20x20 <- function() {
  speed::speed(
    data = df_initial_20x20,
    swap = "treatment",
    swap_within = "row_block",
    spatial_factors = ~col_block,
    iterations = 200000,
    early_stop_iterations = 10000,
    optimise_params = optim_params(
      adaptive_swaps = TRUE,
      swap_count = 3
    ),
    seed = 112
  )
}
speed_result <- bench_speed_20x20()
speed_result$score

design_df <- speed_result$design_df
speed_result$design_df$row <- as.numeric(design_df$row)
speed_result$design_df$col <- as.numeric(design_df$col)
speed_result$design_df$block <- as.numeric(design_df$block)
unique(table(design_df$treatment, design_df$row_block))
unique(table(design_df$treatment, design_df$col_block))
speed::calculate_adjacency_score(design_df, "treatment")
design_efficiency(
  design_df,
  "treatment",
  ~ (row_block / row) * (col_block / col)
)

png("speed-20x20.png", height = 720, width = 720)
speed::autoplot(speed_result)
dev.off()

# digger
bench_digger_20x20 <- function(variables) {
  DiGGer::ibDiGGer(
    numberOfTreatments = n_treatments_20x20,
    rowsInDesign = n_rows_20x20,
    columnsInDesign = n_cols_20x20,
    rowsInBlock = 20,
    columnsInBlock = 2,
    rowsInRep = 2,
    columnsInRep = 20,
    maxInterchanges = 700000,
    rngSeeds = c(112, 112)
  )
}
digger_design <- DiGGer::getDesign(bench_digger_20x20())
df_digger <- df_initial_20x20
df_digger$row <- as.numeric(df_digger$row)
df_digger$col <- as.numeric(df_digger$col)
df_digger$block <- as.numeric(df_digger$block)
df_digger$treatment <- c(digger_design)
unique(table(df_digger$treatment, df_digger$row_block))
unique(table(df_digger$treatment, df_digger$col_block))
speed::calculate_adjacency_score(df_digger, "treatment")
design_efficiency(
  df_digger,
  "treatment",
  ~ (row_block / row) * (col_block / col)
)

digger_result <- speed_result
digger_result$design_df <- df_digger
png("digger-20x20.png", height = 720, width = 720)
speed::autoplot(digger_result)
dev.off()

# odw
df_initial_odw_20x20 <- df_initial_20x20
df_initial_odw_20x20 <- speed:::shuffle_items(
  df_initial_odw_20x20,
  "treatment",
  "col_block",
  112
)

initial_param_table_20x20 <- odw::odw(
  random = ~ treatment + col_block + row_block,
  data = df_initial_20x20,
  permute = ~treatment,
  swap = ~col_block,
  search = "tabu",
  start.values = TRUE
)$vparameters.table
initial_param_table_20x20

initial_param_table_20x20[2:3, 2] <- 100
initial_param_table_20x20

bench_odw_20x20 <- function() {
  odw::odw(
    random = ~ treatment + col_block + row_block,
    data = df_initial_odw_20x20,
    permute = ~treatment,
    swap = ~col_block,
    search = "tabu",
    G.param = initial_param_table_20x20,
    R.param = initial_param_table_20x20,
    maxit = 8
  )
}
design_object <- bench_odw_20x20()

df_odw <- design_object$design
df_odw$row <- as.numeric(df_odw$row)
df_odw$col <- as.numeric(df_odw$col)
df_odw$block <- as.numeric(df_odw$block)
odw_result <- speed_result
odw_result$design_df <- df_odw
unique(table(df_odw$treatment, df_odw$col_block))
unique(table(df_odw$treatment, df_odw$row_block))
speed::calculate_adjacency_score(df_odw, "treatment")
design_efficiency(df_odw, "treatment", ~ (row_block / row) * (col_block / col))

png("odw-20x20.png", height = 720, width = 720)
speed::autoplot(odw_result)
dev.off()

designs[["20x20"]] <- list(
  # full 2-way (row_block/row)*(col_block/col) OOMs at 400 plots; use one
  # direction for aeff/eeff (bottom stratum row[row_block]#col).
  units = ~ (row_block / row) * col,
  treatment = "treatment",
  is_converged = function(df) TRUE,
  tools = list(
    speed = list(seed = 112, run = function() bench_speed_20x20()$design_df),
    digger = list(seed = 112, run = function() {
      d <- df_initial_20x20
      d$treatment <- c(DiGGer::getDesign(bench_digger_20x20()))
      d
    }),
    odw = list(seed = 112, run = function() bench_odw_20x20()$design)
  )
)

bench_result <- bench::mark(
  check = FALSE,
  iterations = 10,
  speed = bench_speed_20x20(),
  digger = bench_digger_20x20(),
  odw = bench_odw_20x20()
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
n_rows_split <- 12
n_cols_split <- 4

# Hierarchical split-plot design
df_initial_split <- data.frame(
  row = rep(1:12, each = 4),
  col = rep(1:4, times = 12),
  block = rep(1:4, each = 12),
  wholeplot = rep(1:12, each = 4),
  wholeplot_treatment = rep(rep(LETTERS[1:3], each = 4), times = 4),
  subplot_treatment = rep(letters[1:4], 12)
)

df_initial_split$wholeplot <- as.factor(df_initial_split$wholeplot)
df_initial_split$block <- as.factor(df_initial_split$block)
df_initial_split$wholeplot_treatment <- as.factor(
  df_initial_split$wholeplot_treatment
)
df_initial_split$subplot_treatment <- as.factor(
  df_initial_split$subplot_treatment
)
df_initial_split$row <- as.factor(df_initial_split$row)
df_initial_split$col <- as.factor(df_initial_split$col)

# save layout
df_layout <- df_initial_split
df_layout$row <- as.numeric(df_layout$row)
df_layout$col <- as.numeric(df_layout$col)
class(df_layout) <- c(class(df_layout), "design")
png("layout-split-whole.png", height = 720, width = 240)
speed::autoplot(df_layout, treatments = "wholeplot_treatment")
dev.off()
png("layout-split-sub.png", height = 720, width = 240)
speed::autoplot(
  df_layout,
  treatments = "subplot_treatment",
  block = "wholeplot"
)
dev.off()

bench_speed_split <- function() {
  speed::speed(
    df_initial_split,
    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
    swap_within = list(wp = "block", sp = "wholeplot"),
    early_stop_iterations = list(wp = 1000, sp = 20000),
    iterations = list(wp = 5000, sp = 50000),
    seed = 3
  )
}
speed_result <- bench_speed_split()
speed_result$score

design_df <- speed_result$design_df
speed_result$design_df$row <- as.numeric(design_df$row)
speed_result$design_df$col <- as.numeric(design_df$col)
speed_result$design_df$block <- as.numeric(design_df$block)
speed_result$design_df$wholeplot <- as.numeric(design_df$wholeplot)
unique(table(design_df$wholeplot_treatment, design_df$block))
unique(table(design_df$subplot_treatment, design_df$wholeplot))
speed::calculate_adjacency_score(design_df, "subplot_treatment")
design_efficiency(
  design_df,
  c("wholeplot_treatment", "subplot_treatment"),
  ~ block / wholeplot / col
)

png("speed-split-whole.png", height = 720, width = 240)
speed::autoplot(speed_result, treatments = "wholeplot_treatment")
dev.off()
png("speed-split-sub.png", height = 720, width = 240)
speed::autoplot(
  speed_result,
  treatments = "subplot_treatment",
  block = "wholeplot"
)
dev.off()

# digger
bench_digger_whole_split <- function(variables) {
  DiGGer::facDiGGer(
    factorNames = c("F1", "F2"),
    rowsInDesign = n_rows_split,
    columnsInDesign = n_cols_split,
    rowsInReplicate = 3,
    columnsInReplicate = 4,
    mainPlotSizes = list(c(1, 4), c(1, 1)),
    treatDataFrame = DF15,
    treatRepColumn = "Repeats",
    maxInterchanges = 1000,
    rngSeeds = c(112, 112)
  )
}
digger_design <- DiGGer::getDesign(bench_digger_whole_split())

df_digger <- df_initial_split
df_digger$row <- as.numeric(df_digger$row)
df_digger$col <- as.numeric(df_digger$col)
df_digger$block <- as.numeric(df_digger$block)
df_digger$wholeplot <- as.numeric(df_digger$wholeplot)
df_digger$subplot_treatment <- letters[c(t(digger_design %% 4 + 1))]
df_digger$wholeplot_treatment <- c(t(ifelse(
  digger_design <= 4,
  "A",
  ifelse(digger_design > 8, "C", "B")
)))
unique(table(df_digger$wholeplot_treatment, df_digger$block))
unique(table(df_digger$subplot_treatment, df_digger$wholeplot))
speed::calculate_adjacency_score(df_digger, "subplot_treatment")
design_efficiency(
  df_digger,
  c("wholeplot_treatment", "subplot_treatment"),
  ~ block / wholeplot / col
)

digger_result <- speed_result
digger_result$design_df <- df_digger
png("digger-split-whole.png", height = 720, width = 240)
speed::autoplot(digger_result, treatments = "wholeplot_treatment")
dev.off()
png("digger-split-sub.png", height = 720, width = 240)
speed::autoplot(
  digger_result,
  treatments = "subplot_treatment",
  block = "wholeplot"
)
dev.off()

# odw
df_initial_odw_split <- df_initial_split
df_dummy_odw <- speed::initialise_design_df(
  as.factor(rep(LETTERS[1:n_wholeplot_treatments], n_wholeplot_reps)),
  n_rows_split,
  1,
  block_nrows,
  1
)
df_initial_odw_split$wholeplot <- as.factor(df_initial_odw_split$wholeplot)
df_initial_odw_split$row <- as.factor(df_initial_odw_split$row)
df_initial_odw_split$col <- as.factor(df_initial_odw_split$col)
df_dummy_odw$block <- as.factor(df_dummy_odw$block)
df_dummy_odw$row <- as.factor(df_dummy_odw$row)
df_dummy_odw$col <- as.factor(df_dummy_odw$col)
df_dummy_odw <- speed:::shuffle_items(df_dummy_odw, "treatment", "block", 112)

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

initial_param_table_split <- odw::odw(
  random = ~ subplot_treatment + block:col,
  data = df_initial_odw_split,
  permute = ~subplot_treatment,
  swap = ~row,
  search = "tabu",
  start.values = TRUE
)$vparameters.table
initial_param_table_split

initial_param_table_split[2:3, 2] <- 100
initial_param_table_split

bench_odw_split <- function() {
  odw::odw(
    random = ~ subplot_treatment + block:col,
    data = df_initial_odw_split,
    permute = ~subplot_treatment,
    swap = ~row,
    search = "tabu",
    G.param = initial_param_table_split,
    R.param = initial_param_table_split,
    maxit = 8
  )
}
design_object <- bench_odw_split()

df_odw <- design_object$design
df_odw_dummy <- design_object_dummy$design
df_odw$row <- as.numeric(df_odw$row)
df_odw$col <- as.numeric(df_odw$col)
# df_odw$subplot_treatment <- df_odw$treatment
df_odw$wholeplot_treatment <- rep(df_odw_dummy$treatment, each = n_cols_split)
# df_odw$block <- rep(df_odw_dummy$block, n_cols)
odw_result <- speed_result
odw_result$design_df <- df_odw
unique(table(df_odw$wholeplot_treatment, df_odw$block))
unique(table(df_odw$subplot_treatment, df_odw$wholeplot))
speed::calculate_adjacency_score(df_odw, "subplot_treatment")
design_efficiency(
  df_odw,
  c("wholeplot_treatment", "subplot_treatment"),
  ~ block / wholeplot / col
)

png("odw-split-whole.png", height = 720, width = 240)
speed::autoplot(odw_result, treatments = "wholeplot_treatment")
dev.off()
png("odw-split-sub.png", height = 720, width = 240)
speed::autoplot(
  odw_result,
  treatments = "subplot_treatment",
  block = "wholeplot"
)
dev.off()

designs[["split-plot"]] <- list(
  units = ~ block / wholeplot / col,
  treatment = c("wholeplot_treatment", "subplot_treatment"),
  is_converged = function(df) TRUE,
  tools = list(
    speed = list(seed = 3, run = function() bench_speed_split()$design_df),
    digger = list(seed = 112, run = function() {
      dd <- DiGGer::getDesign(bench_digger_whole_split())
      d <- df_initial_split
      d$subplot_treatment <- letters[c(t(dd %% 4 + 1))]
      d$wholeplot_treatment <- c(t(ifelse(
        dd <= 4,
        "A",
        ifelse(dd > 8, "C", "B")
      )))
      d
    }),
    odw = list(seed = 112, run = function() {
      d <- bench_odw_split()$design
      d$wholeplot_treatment <- rep(
        bench_odw_dummy()$design$treatment,
        each = n_cols_split
      )
      d
    })
  )
)

bench_result <- bench::mark(
  check = FALSE,
  iterations = 10,
  speed = bench_speed_split(),
  digger = {
    bench_digger_sub()
    bench_digger_whole_split()
  },
  odw = {
    bench_odw_dummy()
    bench_odw_split()
  }
)

png("bench-split.png", height = 720, width = 720)
ggplot2::autoplot(bench_result, type = "boxplot")
dev.off()

#######################################################

benchmark_results <- run_benchmarks(designs)
benchmark_results
