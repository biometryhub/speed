
# Hierarchical split-plot design
df_split <- data.frame(
  row = rep(1:12, each = 4),
  col = rep(1:4, times = 12),
  block = rep(1:4, each = 12),
  wholeplot = rep(1:12, each = 4),
  wholeplot_treatment = rep(rep(LETTERS[1:3], each = 4), times = 4),
  subplot_treatment = rep(letters[1:4], 12)
)

options(speed.adj_weight = 1, speed.adaptive_swaps = TRUE)
result <- speed(df_split,
                swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                swap_within = list(wp = "block", sp = "wholeplot"),
                early_stop_iterations = list(wp = 1000, sp = 10000), seed = 2)

autoplot(result, treatments = "wholeplot_treatment")
autoplot(result, treatments = "subplot_treatment")


new_design <- generate_sequential_neighbour(design, swap, swap_within, level, swap_count, swap_all_blocks)

class(new_design$design) <- c("design", class(new_design$design))
autoplot(new_design$design, treatments = "wholeplot_treatment")
autoplot(new_design$design, treatments = "subplot_treatment")



df_split <- data.frame(
  row = rep(1:18, each = 8),
  col = rep(1:8, times = 18),
  block = rep(1:4, each = 36),
  wholeplot = rep(1:12, each = 12),
  wholeplot_treatment = rep(rep(LETTERS[1:3], each = 12), times = 4),
  subplot = rep(1:48, each = 3),
  subplot_treatment = rep(rep(letters[1:4], each = 3), times = 12),
  subsubplot_treatment = rep(c("x", "y", "z"), 48)
)

result_ss <- speed(df_split_split,
                swap = list(wp = "wholeplot_treatment",
                            sp = "subplot_treatment",
                            ssp = "subsubplot_treatment"),
                swap_within = list(wp = "block",
                                   sp = "wholeplot",
                                   ssp = "subplot"))

autoplot(result_ss, treatments = "wholeplot_treatment")
autoplot(result_ss, treatments = "subplot_treatment")
autoplot(result_ss, treatments = "subsubplot_treatment")
