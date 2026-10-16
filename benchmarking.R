source("./bench/utils.R")
source("./bench/large.R")
source("./bench/irregular.R")
source("./bench/split-plot.R")
source("./bench/2d-block.R")

options(scipen = 999)

# designs <- list(
#   `split-plot` = split_design(),
#   large = large_design(),
#   irr = irr_design(),
#   `2d-block` = two_d_design()
# )
#
# # run_benchmarks(designs, 1:10)
# run_benchmarks(designs, 1:10, from_objects = TRUE)

design_types <- list(
  list(
    file = "benchmark-2d-block.csv",
    out = "bench-2d-compare.pdf",
    title = "2d blocking design",
    adjacency = c(adjacency = "Adjacency - lower better")
  ),
  list(
    file = "benchmark-irr.csv",
    out = "bench-irr-compare.pdf",
    title = "Irregular design",
    adjacency = c(adjacency = "Adjacency - lower better")
  ),
  list(
    file = "benchmark-large.csv",
    out = "bench-large-compare.pdf",
    title = "Large design",
    adjacency = c(adjacency = "Adjacency - lower better")
  ),
  list(
    file = "benchmark-split-plot.csv",
    out = "bench-split-plot-compare.pdf",
    title = "Split-plot design",
    adjacency = c(sub_adjacency = "Subtreatment adjacency - lower better")
  )
)
platform_dirs <- c(linux = "./bench-out-linux", windows = "./bench-out-windows")

for (design_type in design_types) {
  # skip platforms that have no results for this design
  paths <- file.path(platform_dirs, design_type$file)
  names(paths) <- names(platform_dirs)
  paths <- paths[file.exists(paths)]
  results <- bind_rows(lapply(names(paths), function(p) {
    df <- read.csv(paths[[p]])
    df$platform <- p
    return(df)
  }))

  metrics <- c(
    run_time = "Run time (s) - lower better",
    aefficiency = "A-efficiency - higher better",
    eefficiency = "E-efficiency - higher better",
    design_type$adjacency
  )
  long <- do.call(
    rbind,
    lapply(names(metrics), function(m) {
      data.frame(
        tool = results$tool,
        platform = results$platform,
        metric = unname(metrics[m]),
        value = results[[m]]
      )
    })
  )
  long$tool <- factor(long$tool, levels = c("speed", "digger", "odw"))
  long$metric <- factor(long$metric, levels = unname(metrics))

  # pull the efficiency panels' y axes down to min - 0.05, snapped to 0.05
  eff_floors <- do.call(
    rbind,
    lapply(unname(metrics[c("aefficiency", "eefficiency")]), function(m) {
      values <- long$value[long$metric == m]
      data.frame(
        tool = long$tool[1],
        metric = factor(m, levels = levels(long$metric)),
        value = floor((min(values, na.rm = TRUE) - 0.05) * 20) / 20
      )
    })
  )

  run_time_metric <- unname(metrics["run_time"])

  boxplot <- function(...) {
    geom_boxplot(
      ...,
      outlier.shape = NA,
      alpha = 0.3,
      linewidth = 0.2,
      width = 0.6
    )
  }

  p <- ggplot(long, aes(tool, value)) +
    boxplot(data = ~ subset(.x, metric != run_time_metric)) +
    geom_jitter(
      data = ~ subset(.x, metric != run_time_metric),
      width = 0.12,
      height = 0,
      alpha = 0.3
    ) +
    # run time
    boxplot(
      data = ~ subset(.x, metric == run_time_metric),
      aes(fill = platform, colour = platform)
    ) +
    geom_point(
      data = ~ subset(.x, metric == run_time_metric),
      aes(colour = platform, group = platform),
      position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.6),
      alpha = 0.3
    ) +
    geom_blank(data = eff_floors) +
    facet_wrap(~metric, scales = "free_y", nrow = 2) +
    ggh4x::facetted_pos_scales(
      y = list(metric == run_time_metric ~ scale_y_log10())
    ) +
    labs(
      title = paste0(design_type$title, ": tool comparison"),
      subtitle = "10 seeds per tool",
      x = NULL,
      y = NULL
    ) +
    # theme_bw(base_size = 31) +
    theme_bw() +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )

  ggsave(design_type$out, p, width = 8, height = 5)
}
