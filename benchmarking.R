source("./bench/utils.R")
source("./bench/large.R")
source("./bench/irregular.R")
source("./bench/split-plot.R")
source("./bench/2d-block.R")

designs <- list(
  `split-plot` = split_design(),
  large = large_design(),
  irr = irr_design()
)

run_benchmarks(designs, 1:10)

# library(dae)
# library(dplyr)
# library(odw)
# library(speed)
# library(ggplot2)
# library(patchwork)
#
# efficiency <- function(design, treatment, units = ~ row * col) {
#   design <- as.data.frame(design)
#   cols <- unique(c(all.vars(units), treatment))
#   design[cols] <- lapply(design[cols], factor)
#   anatomy <- dae::designAnatomy(
#     list(units = units, treatments = stats::reformulate(treatment)),
#     data = design
#   )
#   return(summary(anatomy))
# }
#
# # odw needs factors
# to_factor <- function(df, cols) {
#   df[cols] <- lapply(df[cols], as.factor)
#   return(df)
# }
#
# get_metrics <- function(df, cols, skip_adj = FALSE) {
#   metrics <- if (!skip_adj) {
#     list(adjacency = speed::calculate_adjacency_score(df, "treatment"))
#   } else {
#     list()
#   }
#   for (col in cols) {
#     uniques <- unique(table(df$treatment, df[[col]]))
#     name <- paste0(col, "_unique_occurence")
#     metrics[[name]] <- paste(uniques, collapse = ",")
#   }
#   for (name in names(metrics)) {
#     cat(sprintf("%s: %s\n", name, metrics[[name]]))
#   }
#
#   return(invisible(metrics))
# }
#
# # aefficiency / eefficiency of the lowest treatment-bearing stratum.
# bottom_stratum_eff <- function(anatomy_summary) {
#   d <- anatomy_summary$decomp
#   keep <- which(!is.na(d$Source.treatments) & d$Source.treatments != "Residual")
#   if (!length(keep)) {
#     return(list(stratum = NA_character_, aeff = NA_real_, eeff = NA_real_))
#   }
#
#   i <- keep[length(keep)]
#   return(list(
#     stratum = d$Source.units[i],
#     aeff = d$aefficiency[i],
#     eeff = d$eefficiency[i]
#   ))
# }
#
# # Writes one CSV per design (`<csv_prefix>-<design>.csv`) and returns a named
# # list of per-design result data frames. A design spec may supply
# # `custom_metrics`, a function of the design data frame returning a named list
# # of scalars; these are appended as the right-most columns of each bench run.
# run_benchmarks <- function(designs, seeds, csv_prefix = "benchmark") {
#   results <- list()
#   for (design_name in names(designs)) {
#     spec <- designs[[design_name]]
#     csv_path <- sprintf("%s-%s.csv", csv_prefix, design_name)
#     rows <- list()
#     for (tool_name in names(spec$tools)) {
#       run_tool <- spec$tools[[tool_name]]
#       for (seed in seeds) {
#         run <- tryCatch(
#           {
#             elapsed <- system.time(design_df <- run_tool(seed))[["elapsed"]]
#             list(elapsed = elapsed, design_df = design_df)
#           },
#           error = function(e) {
#             warning(sprintf(
#               "%s/%s/seed=%s failed: %s",
#               design_name,
#               tool_name,
#               seed,
#               conditionMessage(e)
#             ))
#             NULL
#           }
#         )
#         metrics <- list(conv = NA, aeff = NA_real_, eeff = NA_real_)
#         if (!is.null(run)) {
#           metrics <- tryCatch(
#             {
#               eff <- efficiency(run$design_df, spec$treatment, spec$units) |>
#                 bottom_stratum_eff()
#               list(
#                 conv = isTRUE(spec$is_converged(run$design_df)),
#                 aeff = eff$aeff,
#                 eeff = eff$eeff
#               )
#             },
#             error = function(e) {
#               warning(sprintf(
#                 "%s/%s/seed=%s metrics failed: %s",
#                 design_name,
#                 tool_name,
#                 seed,
#                 conditionMessage(e)
#               ))
#               list(conv = NA, aeff = NA_real_, eeff = NA_real_)
#             }
#           )
#         }
#         row <- data.frame(
#           tool = tool_name,
#           design = design_name,
#           seed = seed,
#           run_time = if (is.null(run)) NA_real_ else run$elapsed,
#           is_converged = metrics$conv,
#           aefficiency = metrics$aeff,
#           eefficiency = metrics$eeff
#         )
#
#         # Design-specific custom columns, appended to the right
#         if (!is.null(run) && is.function(spec$custom_metrics)) {
#           custom <- tryCatch(
#             as.data.frame(as.list(spec$custom_metrics(run$design_df))),
#             error = function(e) {
#               warning(sprintf(
#                 "%s/%s/seed=%s custom metrics failed: %s",
#                 design_name,
#                 tool_name,
#                 seed,
#                 conditionMessage(e)
#               ))
#               NULL
#             }
#           )
#           if (!is.null(custom)) {
#             row <- cbind(row, custom)
#           }
#         }
#
#         rows[[length(rows) + 1L]] <- row
#         # rewrite every row each run, cheap
#         design_results <- dplyr::bind_rows(rows)
#         utils::write.csv(design_results, csv_path, row.names = FALSE)
#       }
#     }
#     results[[design_name]] <- design_results
#   }
#   return(results)
# }
#
# odw_search <- "tabu+rw"
# designs <- list()
#
# # 250 treatments, 6 reps, 250 rows, 6 columns
# n_treatments_large <- 250
# n_reps <- 6
# n_rows_large <- 250
# n_cols_large <- 6
# units_large <- ~ block + row * col
#
# # speed
# df_initial_large <- speed::initialise_design_df(
#   items = n_treatments_large,
#   nrows = n_rows_large,
#   ncols = n_cols_large,
#   block_nrows = 125,
#   block_ncols = 2
# ) |>
#   to_factor(c("treatment", "block", "row", "col"))
#
# df_layout <- df_initial_large
# class(df_layout) <- c(class(df_layout), "design")
# png("layout-large.png", height = 720, width = 720)
# speed::autoplot(df_layout)
# dev.off()
#
# bench_speed_large <- function(seed = 112) {
#   speed::speed(
#     data = df_initial_large,
#     swap = "treatment",
#     swap_within = "block",
#     spatial_factors = ~ row + col,
#     iterations = 1000000,
#     early_stop_iterations = 200000,
#     optimise_params = optim_params(
#       random_initialisation = 300,
#       adaptive_swaps = TRUE,
#       swap_count = 3
#     ),
#     seed = seed
#   )
# }
# # speed_result <- bench_speed_large()
# # speed_result$score
# #
# # design_df <- speed_result$design_df
# # get_metrics(design_df, c("row", "col", "block"))
# # efficiency(design_df, "treatment", units_large)
# # # Source.units df1  Source.treatments df2  aefficiency eefficiency order
# # # block           5
# # # row           248 treatment          248      0.0005      0.0000   248
# # # col             3 treatment            1      0.0027      0.0027     1
# # #                   Residual             2
# # # row#col      1243 treatment          249      0.8046      0.4681   249
# # #                   Residual           994
# #
# # png("speed-large.png", height = 720, width = 720)
# # speed::autoplot(speed_result)
# # dev.off()
#
# # digger
# bench_digger_large <- function(seed = 112) {
#   DiGGer::corDiGGer(
#     numberOfTreatments = n_treatments_large,
#     rowsInDesign = n_rows_large,
#     columnsInDesign = n_cols_large,
#     rowsInReplicate = 125,
#     columnsInReplicate = 2,
#     treatRepPerRep = 1,
#     blockSequence = list(c(250, 1)),
#     maxInterchanges = c(100000, 2000000),
#     rngSeeds = rep(seed, 2)
#   )
# }
# # digger_design <- DiGGer::getDesign(bench_digger_large())
# #
# # df_digger <- df_initial_large
# # df_digger$treatment <- c(digger_design)
# # get_metrics(df_digger, c("row", "col", "block"))
# # efficiency(df_digger, "treatment", units_large)
# # # Source.units df1  Source.treatments df2  aefficiency eefficiency order
# # # block           5
# # # row           248 treatment          248      0.0002      0.0000   248
# # # col             3
# # # row#col      1243 treatment          249      0.7988      0.4177   249
# # #                   Residual           994
# #
# # digger_result <- speed_result
# # digger_result$design_df <- df_digger
# # png("digger-large.png", height = 720, width = 720)
# # speed::autoplot(digger_result)
# # dev.off()
#
# designs[["large"]] <- list(
#   units = units_large,
#   treatment = "treatment",
#   is_converged = function(df) TRUE,
#   custom_metrics = function(df) get_metrics(df, c("row", "col", "block")),
#   tools = list(
#     speed = function(seed) bench_speed_large(seed)$design_df,
#     digger = function(seed) {
#       d <- df_initial_large
#       d$treatment <- c(DiGGer::getDesign(bench_digger_large(seed)))
#       return(d)
#     }
#   )
# )
# run_benchmarks(designs, 1:10)
#
# # odw
# df_initial_odw_large <- speed::initialise_design_df(
#   rep(1:n_treatments_large, n_reps),
#   n_rows_large,
#   n_cols_large,
#   125,
#   2
# ) |>
#   speed:::shuffle_items("treatment", "block", 112) |>
#   to_factor(c("treatment", "block", "row", "col"))
#
# initial_param_table_large <- odw::odw(
#   random = ~ treatment + block + row + col,
#   data = df_initial_odw_large,
#   permute = ~treatment,
#   swap = ~block,
#   search = odw_search,
#   start.values = TRUE
# )$vparameters.table
# initial_param_table_large
#
# initial_param_table_large[3:4, 2] <- 100
# initial_param_table_large
#
# bench_odw_large <- function() {
#   odw::odw(
#     random = ~ treatment + block + row + col,
#     data = df_initial_odw_large,
#     permute = ~treatment,
#     swap = ~block,
#     search = odw_search,
#     G.param = initial_param_table_large,
#     R.param = initial_param_table_large,
#     maxit = 9
#   )
# }
# design_object <- bench_odw_large()
#
# df_odw <- design_object$design
# odw_result <- speed_result
# odw_result$design_df <- df_odw
# get_metrics(df_odw, c("row", "col", "block"))
# efficiency(df_odw, "treatment", units_large)
# # Source.units   df1 Source.treatments df2 aefficiency eefficiency order
# # row_block        9
# # col_block        9 treatment           1      0.0100      0.0100     1
# #                    Residual            8
# # row[row_block]  10 treatment          10      0.0835      0.0452    10
# # col[col_block]  10 treatment          10      0.0822      0.0440    10
# # row#col        361 treatment          39      0.9433      0.7756    22
# #                    Residual          322
#
# png("odw-large.png", height = 720, width = 720)
# speed::autoplot(odw_result)
# dev.off()
#
# designs[["large"]] <- list(
#   units = units_large,
#   treatment = "treatment",
#   is_converged = function(df) TRUE,
#   custom_metrics = function(df) get_metrics(df, c("col_block", "row_block")),
#   tools = list(
#     speed = function(seed) bench_speed_large(seed)$design_df,
#     digger = function(seed) {
#       d <- df_initial_large
#       d$treatment <- c(DiGGer::getDesign(bench_digger_large(seed)))
#       return(d)
#     },
#     odw = function(seed) bench_odw_large()$design
#   )
# )

results <- read.csv("benchmark-split-plot.csv")
metrics <- c(
  run_time = "Run time (s) - lower better",
  aefficiency = "A-efficiency - higher better",
  eefficiency = "E-efficiency - higher better",
  sub_adjacency = "Subtreatment adjacency - lower better"
  # adjacency = "Adjacency - lower better"
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

# pull the efficiency panels' y axes down to floor(min - 0.1) at 1 decimal
eff_floors <- do.call(
  rbind,
  lapply(unname(metrics[c("aefficiency", "eefficiency")]), function(m) {
    values <- long$value[long$metric == m]
    data.frame(
      tool = long$tool[1],
      metric = factor(m, levels = levels(long$metric)),
      value = floor((min(values, na.rm = TRUE) - 0.1) * 10) / 10 + 0.05
    )
  })
)

png("bench-split-plot-compare.png", height = 1440, width = 1920)
ggplot(long, aes(tool, value, fill = tool)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.55, width = 0.6) +
  geom_jitter(width = 0.12, height = 0, size = 1.6, alpha = 0.8) +
  geom_blank(data = eff_floors) +
  facet_wrap(~metric, scales = "free_y", nrow = 2) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(
    title = "Split plot design: tool comparison across metrics",
    subtitle = "10 seeds per tool; points are individual runs",
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_size = 23) +
  theme(strip.text = element_text(face = "bold"))
dev.off()
