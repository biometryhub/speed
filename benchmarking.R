library(dae)
library(dplyr)
library(odw)
library(speed)
library(ggplot2)
library(patchwork)

efficiency <- function(design, treatment, units = ~ row * col) {
  design <- as.data.frame(design)
  cols <- unique(c(all.vars(units), treatment))
  design[cols] <- lapply(design[cols], factor)
  anatomy <- dae::designAnatomy(
    list(units = units, treatments = stats::reformulate(treatment)),
    data = design
  )
  return(summary(anatomy))
}

# odw needs factors
to_factor <- function(df, cols) {
  df[cols] <- lapply(df[cols], as.factor)
  return(df)
}

get_metrics <- function(df, cols) {
  metrics <- list(adjacency = speed::calculate_adjacency_score(df, "treatment"))
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

# Writes one CSV per design (`<csv_prefix>-<design>.csv`) and returns a named
# list of per-design result data frames. A design spec may supply
# `custom_metrics`, a function of the design data frame returning a named list
# of scalars; these are appended as the right-most columns of each bench run.
run_benchmarks <- function(designs, seeds, csv_prefix = "benchmark") {
  results <- list()
  for (design_name in names(designs)) {
    spec <- designs[[design_name]]
    rows <- list()
    for (tool_name in names(spec$tools)) {
      run_tool <- spec$tools[[tool_name]]
      for (seed in seeds) {
        run <- tryCatch(
          {
            elapsed <- system.time(design_df <- run_tool(seed))[["elapsed"]]
            list(elapsed = elapsed, design_df = design_df)
          },
          error = function(e) {
            warning(sprintf(
              "%s/%s/seed=%s failed: %s",
              design_name,
              tool_name,
              seed,
              conditionMessage(e)
            ))
            NULL
          }
        )
        metrics <- list(conv = NA, aeff = NA_real_, eeff = NA_real_)
        if (!is.null(run)) {
          metrics <- tryCatch(
            {
              eff <- efficiency(run$design_df, spec$treatment, spec$units) |>
                bottom_stratum_eff()
              list(
                conv = isTRUE(spec$is_converged(run$design_df)),
                aeff = eff$aeff,
                eeff = eff$eeff
              )
            },
            error = function(e) {
              warning(sprintf(
                "%s/%s/seed=%s metrics failed: %s",
                design_name,
                tool_name,
                seed,
                conditionMessage(e)
              ))
              list(conv = NA, aeff = NA_real_, eeff = NA_real_)
            }
          )
        }
        row <- data.frame(
          tool = tool_name,
          design = design_name,
          seed = seed,
          run_time = if (is.null(run)) NA_real_ else run$elapsed,
          is_converged = metrics$conv,
          aefficiency = metrics$aeff,
          eefficiency = metrics$eeff,
          stringsAsFactors = FALSE
        )

        # Design-specific custom columns, appended to the right
        if (!is.null(run) && is.function(spec$custom_metrics)) {
          custom <- tryCatch(
            as.data.frame(
              as.list(spec$custom_metrics(run$design_df)),
              stringsAsFactors = FALSE
            ),
            error = function(e) {
              warning(sprintf(
                "%s/%s/seed=%s custom metrics failed: %s",
                design_name,
                tool_name,
                seed,
                conditionMessage(e)
              ))
              NULL
            }
          )
          if (!is.null(custom)) {
            row <- cbind(row, custom)
          }
        }

        rows[[length(rows) + 1L]] <- row
      }
    }
    # bind_rows fills NA for runs that produced no custom columns (e.g. failures)
    design_results <- dplyr::bind_rows(rows)
    utils::write.csv(
      design_results,
      sprintf("%s-%s.csv", csv_prefix, design_name),
      row.names = FALSE
    )
    results[[design_name]] <- design_results
  }
  return(results)
}

designs <- list()

#######################################################
# 15 treatments, 5 reps, 25 rows, 3 columns
n_treatments_small <- 15
n_reps <- 5
n_rows_small <- 25
n_cols_small <- 3

# speed
df_initial_small <- speed::initialise_design_df(
  items = n_treatments_small,
  nrows = n_rows_small,
  ncols = n_cols_small,
  block_nrows = 5,
  block_ncols = 3
) |>
  to_factor(c("treatment", "block", "row", "col"))

df_layout <- df_initial_small
df_layout$plot_in_block <- df_initial_small$treatment
class(df_layout) <- c(class(df_layout), "design")
png("layout-small.png", height = 500, width = 500)
speed::autoplot(df_layout, treatments = "plot_in_block")
dev.off()

bench_speed_small <- function(seed = 112) {
  speed::speed(
    data = df_initial_small,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~col,
    optimise_params = optim_params(random_initialisation = 10),
    seed = seed
  )
}
speed_result <- bench_speed_small()

design_df <- speed_result$design_df
get_metrics(design_df, c("col", "block"))
efficiency(design_df, "treatment", ~ block + row * col)
# Source.units df1 Source.treatments df2 aefficiency eefficiency order
# block          4
# row[block]    20 treatment          14      0.1034      0.0249    14
#                  Residual            6
# col            2 treatment           2      0.0400      0.0400     1
# row#col       48 treatment          14      0.6160      0.2696    14
#                  Residual           34

png("speed-small.png", height = 1080, width = 480)
speed::autoplot(speed_result)
dev.off()

# digger
bench_digger_small <- function(seed = 112) {
  DiGGer::corDiGGer(
    numberOfTreatments = n_treatments_small,
    rowsInDesign = n_rows_small,
    columnsInDesign = n_cols_small,
    rowsInReplicate = 5,
    columnsInReplicate = 3,
    treatRepPerRep = 1,
    blockSequence = list(c(12, 1)),
    maxInterchanges = c(5000, 500000),
    rngSeeds = rep(seed, 2)
  )
}
digger_design <- DiGGer::getDesign(bench_digger_small())

df_digger <- df_initial_small
df_digger$treatment <- c(digger_design)
get_metrics(df_digger, c("col", "block"))
efficiency(df_digger, "treatment", ~ block + row * col)
# Source.units df1 Source.treatments df2 aefficiency eefficiency order
# block          4
# row[block]    20 treatment          14      0.0833      0.0101    14
#                  Residual            6
# col            2 treatment           2      0.0400      0.0400     1
# row#col       48 treatment          14      0.6488      0.3475    14
#                  Residual           34

digger_result <- speed_result
digger_result$design_df <- df_digger
png("digger-small.png", height = 500, width = 480)
speed::autoplot(digger_result)
dev.off()

# odw
df_initial_odw_small <- speed::initialise_design_df(
  rep(1:n_treatments_small, n_reps),
  n_rows_small,
  n_cols_small,
  5,
  3
) |>
  speed:::shuffle_items("treatment", "block", 112) |>
  to_factor(c("treatment", "block", "row", "col"))

initial_param_table_small <- odw::odw(
  random = ~ treatment + block + row + col,
  data = df_initial_odw_small,
  permute = ~treatment,
  swap = ~block,
  search = "tabu",
  start.values = TRUE
)$vparameters.table
initial_param_table_small

initial_param_table_small[4, 2] <- 100
initial_param_table_small

bench_odw_small <- function() {
  odw::odw(
    random = ~ treatment + block + row + col,
    data = df_initial_odw_small,
    permute = ~treatment,
    swap = ~block,
    search = "tabu",
    G.param = initial_param_table_small,
    R.param = initial_param_table_small,
    maxit = 2
  )
}
design_object <- bench_odw_small()

df_odw <- design_object$design
odw_result <- speed_result
odw_result$design_df <- df_odw
get_metrics(df_odw, c("col", 'block'))
efficiency(df_odw, "treatment", ~ block + row * col)
# Source.units df1 Source.treatments df2 aefficiency eefficiency order
# block          4
# row[block]    20 treatment          14      0.1891      0.0477    14
#                  Residual            6
# col            2 treatment           2      0.0400      0.0400     1
# row#col       48 treatment          14      0.6823      0.4618    14
#                  Residual           34

png("odw-small.png", height = 500, width = 500)
speed::autoplot(odw_result)
dev.off()

designs[["small"]] <- list(
  units = ~ block + row * col,
  treatment = "treatment",
  is_converged = function(df) TRUE,
  custom_metrics = function(df) get_metrics(df, c("row", "col", "block")),
  tools = list(
    speed = function(seed) bench_speed_small(seed)$design_df,
    digger = function(seed) {
      d <- df_initial_small
      d$treatment <- c(DiGGer::getDesign(bench_digger_small(seed)))
      return(d)
    },
    odw = function(seed) bench_odw_small()$design
  )
)

# bench_result <- bench::mark(
#   check = FALSE,
#   iterations = 10,
#   speed = bench_speed_small(),
#   digger = bench_digger_small(),
#   odw = bench_odw_small()
# )
#
# png("bench-small.png", height = 720, width = 720, res = 300)
# ggplot2::autoplot(bench_result, type = "violin") + ggplot2::theme_bw()
# dev.off()

# run_benchmarks(designs, 1:10)

#######################################################
# # 40 treatments, 10 reps, 20 rows, 20 columns
# n_treatments_large <- 40
# n_reps <- 10
# n_rows_large <- 20
# n_cols_large <- 20
# units_large <- ~ row_block + col_block + row * col
#
# # speed
# df_initial_large <- speed::initialise_design_df(
#   n_treatments_large,
#   n_rows_large,
#   n_cols_large,
#   2,
#   20
# ) |>
#   to_factor(c("treatment", "row", "col"))
#
# df_dummy <- speed::initialise_design_df(
#   n_treatments_large,
#   n_rows_large,
#   n_cols_large,
#   20,
#   2
# )
# df_initial_large$row_block <- as.factor(df_initial_large$block)
# df_initial_large$col_block <- as.factor(df_dummy$block)

# 250 treatments, 6 reps, 250 rows, 6 columns
n_treatments_large <- 250
n_reps <- 6
n_rows_large <- 250
n_cols_large <- 6
units_large <- ~ block + row * col

# speed
df_initial_large <- speed::initialise_design_df(
  items = n_treatments_large,
  nrows = n_rows_large,
  ncols = n_cols_large,
  block_nrows = 125,
  block_ncols = 2
) |>
  to_factor(c("treatment", "block", "row", "col"))

df_layout <- df_initial_large
class(df_layout) <- c(class(df_layout), "design")
png("layout-large.png", height = 720, width = 720)
speed::autoplot(df_layout)
dev.off()

bench_speed_large <- function(seed = 112) {
  speed::speed(
    data = df_initial_large,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col,
    iterations = 200000,
    early_stop_iterations = 100000,
    optimise_params = optim_params(
      random_initialisation = 300,
      adaptive_swaps = TRUE,
      swap_count = 3
    ),
    seed = seed
  )
}
speed_result <- bench_speed_large()
speed_result$score

design_df <- speed_result$design_df
get_metrics(design_df, c("row", "col", "block"))
efficiency(design_df, "treatment", units_large)
# Source.units df1  Source.treatments df2  aefficiency eefficiency order
# block           5
# row           248 treatment          248      0.0005      0.0000   248
# col             3 treatment            1      0.0027      0.0027     1
#                   Residual             2
# row#col      1243 treatment          249      0.8046      0.4681   249
#                   Residual           994

png("speed-large.png", height = 720, width = 720)
speed::autoplot(speed_result)
dev.off()

# digger
# bench_digger_large <- function(seed = 112) {
#   DiGGer::ibDiGGer(
#     numberOfTreatments = n_treatments_large,
#     rowsInDesign = n_rows_large,
#     columnsInDesign = n_cols_large,
#     rowsInBlock = 20,
#     columnsInBlock = 2,
#     rowsInRep = 2,
#     columnsInRep = 20,
#     maxInterchanges = 700000,
#     rngSeeds = rep(seed, 2)
#   )
# }
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
digger_design <- DiGGer::getDesign(bench_digger_large())

df_digger <- df_initial_large
df_digger$treatment <- c(digger_design)
get_metrics(df_digger, c("row", "col", "block"))
efficiency(df_digger, "treatment", units_large)
# Source.units df1  Source.treatments df2  aefficiency eefficiency order
# block           5
# row           248 treatment          248      0.0002      0.0000   248
# col             3
# row#col      1243 treatment          249      0.7988      0.4177   249
#                   Residual           994

digger_result <- speed_result
digger_result$design_df <- df_digger
png("digger-large.png", height = 720, width = 720)
speed::autoplot(digger_result)
dev.off()

# odw
# df_initial_odw_large <- df_initial_large
# df_initial_odw_large <- speed:::shuffle_items(
#   df_initial_odw_large,
#   "treatment",
#   "row_block",
#   112
# )
#
# initial_param_table_large <- odw::odw(
#   random = ~ treatment + col_block + row_block,
#   data = df_initial_large,
#   permute = ~treatment,
#   swap = ~row_block,
#   search = "tabu",
#   start.values = TRUE
# )$vparameters.table
# initial_param_table_large
#
# initial_param_table_large[2:3, 2] <- 100
# initial_param_table_large
#
# bench_odw_large <- function() {
#   odw::odw(
#     random = ~ treatment + col_block + row_block,
#     data = df_initial_odw_large,
#     permute = ~treatment,
#     swap = ~row_block,
#     search = "tabu",
#     G.param = initial_param_table_large,
#     R.param = initial_param_table_large,
#     maxit = 8
#   )
# }
df_initial_odw_large <- speed::initialise_design_df(
  rep(1:n_treatments_large, n_reps),
  n_rows_large,
  n_cols_large,
  125,
  2
) |>
  speed:::shuffle_items("treatment", "block", 112) |>
  to_factor(c("treatment", "block", "row", "col"))

initial_param_table_large <- odw::odw(
  random = ~ treatment + block + row + col,
  data = df_initial_odw_large,
  permute = ~treatment,
  swap = ~block,
  search = "tabu",
  start.values = TRUE
)$vparameters.table
initial_param_table_large

initial_param_table_large[3:4, 2] <- 100
initial_param_table_large

bench_odw_large <- function() {
  odw::odw(
    random = ~ treatment + block + row + col,
    data = df_initial_odw_large,
    permute = ~treatment,
    swap = ~block,
    search = "tabu",
    G.param = initial_param_table_large,
    R.param = initial_param_table_large,
    maxit = 9
  )
}
design_object <- bench_odw_large()

df_odw <- design_object$design
odw_result <- speed_result
odw_result$design_df <- df_odw
get_metrics(df_odw, c("row", "col", "block"))
efficiency(df_odw, "treatment", units_large)
# Source.units   df1 Source.treatments df2 aefficiency eefficiency order
# row_block        9
# col_block        9 treatment           1      0.0100      0.0100     1
#                    Residual            8
# row[row_block]  10 treatment          10      0.0835      0.0452    10
# col[col_block]  10 treatment          10      0.0822      0.0440    10
# row#col        361 treatment          39      0.9433      0.7756    22
#                    Residual          322

png("odw-large.png", height = 720, width = 720)
speed::autoplot(odw_result)
dev.off()

designs[["large"]] <- list(
  units = units_large,
  treatment = "treatment",
  is_converged = function(df) TRUE,
  custom_metrics = function(df) get_metrics(df, c("col_block", "row_block")),
  tools = list(
    speed = function(seed) bench_speed_large(seed)$design_df,
    digger = function(seed) {
      d <- df_initial_large
      d$treatment <- c(DiGGer::getDesign(bench_digger_large(seed)))
      return(d)
    },
    odw = function(seed) bench_odw_large()$design
  )
)

#######################################################
# split plot
autoplot_split <- function(df) {
  whole <- speed::autoplot(df, treatments = "wholeplot_treatment")
  sub <- speed::autoplot(
    df,
    treatments = "subplot_treatment",
    block = "wholeplot"
  )
  return(whole + sub + patchwork::plot_layout(ncol = 2))
}

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
) |>
  to_factor(c(
    "wholeplot",
    "block",
    "wholeplot_treatment",
    "subplot_treatment",
    "row",
    "col"
  ))

# save layout
df_layout <- df_initial_split
class(df_layout) <- c(class(df_layout), "design")

png("layout-split.png", height = 720, width = 480)
autoplot_split(df_layout)
dev.off()

bench_speed_split <- function(seed = 3) {
  speed::speed(
    df_initial_split,
    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
    swap_within = list(wp = "block", sp = "wholeplot"),
    early_stop_iterations = list(wp = 1000, sp = 20000),
    iterations = list(wp = 5000, sp = 50000),
    seed = seed
  )
}
speed_result <- bench_speed_split()
speed_result$score

design_df <- speed_result$design_df
unique(table(design_df$wholeplot_treatment, design_df$block))
unique(table(design_df$subplot_treatment, design_df$wholeplot))
speed::calculate_adjacency_score(design_df, "subplot_treatment")
efficiency(
  design_df,
  c("wholeplot_treatment", "subplot_treatment"),
  ~ block / wholeplot / col
)

png("speed-split.png", height = 720, width = 480)
autoplot_split(speed_result)
dev.off()

# digger
bench_digger_whole_split <- function(seed = 112) {
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
    rngSeeds = rep(seed, 2)
  )
}
digger_design <- DiGGer::getDesign(bench_digger_whole_split())

df_digger <- df_initial_split
df_digger$subplot_treatment <- letters[c(t(digger_design %% 4 + 1))]
df_digger$wholeplot_treatment <- c(t(ifelse(
  digger_design <= 4,
  "A",
  ifelse(digger_design > 8, "C", "B")
)))
unique(table(df_digger$wholeplot_treatment, df_digger$block))
unique(table(df_digger$subplot_treatment, df_digger$wholeplot))
speed::calculate_adjacency_score(df_digger, "subplot_treatment")
efficiency(
  df_digger,
  c("wholeplot_treatment", "subplot_treatment"),
  ~ block / wholeplot / col
)

digger_result <- speed_result
digger_result$design_df <- df_digger
png("digger-split.png", height = 720, width = 480)
autoplot_split(digger_result)
dev.off()

# odw
df_initial_odw_split <- df_initial_split
df_dummy_odw <- speed::initialise_design_df(
  as.factor(rep(LETTERS[1:n_wholeplot_treatments], n_wholeplot_reps)),
  n_rows_split,
  1,
  block_nrows,
  1
) |>
  to_factor(c("block", "row", "col"))
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
# df_odw$subplot_treatment <- df_odw$treatment
df_odw$wholeplot_treatment <- rep(df_odw_dummy$treatment, each = n_cols_split)
# df_odw$block <- rep(df_odw_dummy$block, n_cols)
odw_result <- speed_result
odw_result$design_df <- df_odw
unique(table(df_odw$wholeplot_treatment, df_odw$block))
unique(table(df_odw$subplot_treatment, df_odw$wholeplot))
speed::calculate_adjacency_score(df_odw, "subplot_treatment")
efficiency(
  df_odw,
  c("wholeplot_treatment", "subplot_treatment"),
  ~ block / wholeplot / col
)

png("odw-split.png", height = 720, width = 480)
autoplot_split(odw_result)
dev.off()

designs[["split-plot"]] <- list(
  units = ~ block / wholeplot / col,
  treatment = c("wholeplot_treatment", "subplot_treatment"),
  is_converged = function(df) TRUE,
  custom_metrics = function(df) {
    list(
      sub_adjacency = speed::calculate_adjacency_score(df, "subplot_treatment"),
      whole_adjacency = speed::calculate_adjacency_score(
        df,
        "wholeplot_treatment"
      )
    )
  },
  tools = list(
    speed = function(seed) bench_speed_split(seed)$design_df,
    digger = function(seed) {
      dd <- DiGGer::getDesign(bench_digger_whole_split(seed))
      d <- df_initial_split
      d$subplot_treatment <- letters[c(t(dd %% 4 + 1))]
      d$wholeplot_treatment <- c(t(ifelse(
        dd <= 4,
        "A",
        ifelse(dd > 8, "C", "B")
      )))
      d
    },
    odw = function(seed) {
      d <- bench_odw_split()$design
      d$wholeplot_treatment <- rep(
        bench_odw_dummy()$design$treatment,
        each = n_cols_split
      )
      d
    }
  )
)

#######################################################
benchmark_results <- run_benchmarks(designs, seeds = 1:10)
benchmark_results

# run and result
run_benchmarks(designs, 1:10)

results <- read.csv("benchmark-large.csv")
metrics <- c(
  run_time = "Run time (s) - lower better",
  aefficiency = "A-efficiency - higher better",
  eefficiency = "E-efficiency - higher better",
  adjacency = "Adjacency - lower better"
)
long <- do.call(
  rbind,
  lapply(names(metrics), function(m) {
    data.frame(
      tool = results$tool,
      metric = unname(metrics[m]),
      value = results[[m]]
    )
  })
)
long$tool <- factor(long$tool, levels = c("speed", "digger", "odw"))
long$metric <- factor(long$metric, levels = unname(metrics))

png("bench-large-compare.png", height = 1440, width = 1920)
ggplot(long, aes(tool, value, fill = tool)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.55, width = 0.6) +
  geom_jitter(width = 0.12, height = 0, size = 1.6, alpha = 0.8) +
  facet_wrap(~metric, scales = "free_y", nrow = 2) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(
    title = "large design: tool comparison across metrics",
    subtitle = "10 seeds per tool; points are individual runs",
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_size = 23) +
  theme(strip.text = element_text(face = "bold"))
dev.off()


initial <- matrix(
  c(1:6, 1:6, 1:4, 0, 0, 1:4, 0, 0),
  nrow = 4,
  byrow = TRUE
)

digger_irr <- DiGGer::corDiGGer(
  numberOfTreatments = 6,
  rowsInDesign = 4,
  columnsInDesign = 6,
  treatRepPerRep = c(4, 4, 4, 4, 2, 2),
  initialDesign = initial,
  rngSeeds = rep(112, 2)
)
digger_design <- DiGGer::getDesign(digger_irr)
