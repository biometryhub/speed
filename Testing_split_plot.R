
# Hierarchical split-plot design
df_split <- data.frame(
  row = rep(1:12, each = 4),
  col = rep(1:4, times = 12),
  block = rep(1:4, each = 12),
  wholeplot = rep(1:12, each = 4),
  wholeplot_treatment = rep(rep(LETTERS[1:3], each = 4), times = 4),
  subplot_treatment = rep(letters[1:4], 12)
)

options(speed.cooling_rate = 0.9999)
result <- speed(df_split,
                swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                swap_within = list(wp = "block", sp = "wholeplot"),
                early_stop_iterations = list(wp = 1000, sp = 20000),
                iterations = list(wp = 5000, sp = 50000))

autoplot(result, treatments = "wholeplot_treatment")
autoplot(result, treatments = "subplot_treatment")


new_design <- generate_sequential_neighbour(design, swap, swap_within, level, swap_count, swap_all_blocks)

class(new_design$design) <- c("design", class(new_design$design))
autoplot(new_design$design, treatments = "wholeplot_treatment")
autoplot(new_design$design, treatments = "subplot_treatment")


options(speed.cooling_rate = 0.99)
df_split_split <- data.frame(
  row = rep(1:16, each = 9),
  col = rep(1:9, times = 16),
  block = rep(1:4, each = 36),
  # Fixed wholeplot assignment: 3 wholeplots per block, each 4×3
  wholeplot = rep(rep(1:3, each = 3), times = 16) + rep(0:3 * 3, each = 36),
  wholeplot_treatment = rep(rep(LETTERS[1:3], each = 3), times = 16),
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
                                   ssp = "subplot"),
                iterations = list(wp = 2000, sp = 2000, ssp = 50000),
                early_stop_iterations = list(wp = 1000, sp = 3000, ssp = 30000))

autoplot(result_ss, treatments = "wholeplot_treatment")
autoplot(result_ss, treatments = "subplot_treatment")
autoplot(result_ss, treatments = "subsubplot_treatment")




df_strip <- data.frame(
  row = rep(1:12, each = 6),  # 24 rows total (4 rows per block x 6 blocks)
  col = rep(1:6, times = 12),  # 3 columns repeated
  block = rep(rep(1:2, each = 3), times = 4) + rep(0:2*2, each = 24),  # 6 blocks, 12 plots each
  
  # Horizontal strips (3 levels, applied to rows within each block)
  vertical_treatment = rep(rep(LETTERS[1:3], times = 2), times = 12),  # A, B, C
  
  # Vertical strips (4 levels, applied to columns within each block)  
  horizontal_treatment = rep(rep(letters[1:4], each = 6), times = 3),  # a, b, c, d
  
  # Plot identifier within each block
  plot_in_block = rep(1:12, times = 6)
)

class(df_strip) <- c("design", class(df_strip))
autoplot(df_strip, treatments = "block")
autoplot(df_strip, treatments = "horizontal_treatment")
autoplot(df_strip, treatments = "vertical_treatment")

result_strip <- speed(df_strip,
                      swap = list(ht = "horizontal_treatment", vt = "vertical_treatment"),
                      swap_within = list(ht = "block", vt = "block"))

autoplot(result_strip, treatments = "horizontal_treatment")
autoplot(result_strip, treatments = "vertical_treatment")
