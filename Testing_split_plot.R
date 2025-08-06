
# Hierarchical split-plot design
df_split <- data.frame(
  row = rep(1:8, each = 4),
  col = rep(1:4, times = 8),
  block = rep(1:4, each = 8),
  wholeplot = rep(1:8, each = 4),
  wholeplot_treatment = rep(rep(LETTERS[1:2], each = 4), times = 4),
  subplot_treatment = rep(letters[1:4], 8)
)

options(speed.adj_weight = 1)
result <- speed(df_split,
                swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
                swap_within = list(wp = "block", sp = "wholeplot"))

autoplot(result, treatments = "wholeplot_treatment")
autoplot(result, treatments = "subplot_treatment")


generate_sequential_neighbour(design, swap, swap_within, level, swap_count, swap_all_blocks)

class(new_design$design) <- c("design", class(new_design$design))
autoplot(new_design$design, treatments = "wholeplot_treatment")
autoplot(new_design$design, treatments = "subplot_treatment")
