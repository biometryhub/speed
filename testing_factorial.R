df_fac <- data.frame(
  row = rep(1:12, times = 6),  # 12 rows total
  col = rep(1:6, each = 12),  # 6 columns
  block = rep(1:6, each = 12),  # 6 columns
  A = rep(rep(paste0("A", 1:4), each = 3), times = 6),  # A1, A2, A3, A4
  B = rep(paste0("B", 1:3), times = 24)  # B1, B2, B3
)
df_fac$combination <- paste(df_fac$A, df_fac$B, sep = "_")

class(df_fac) <- c("design", class(df_fac))

autoplot(df_fac, treatment = A)
autoplot(df_fac, treatment = B)
autoplot(df_fac, treatment = combination)

options(speed.random_initialisation = F, speed.cooling_rate = 0.999)
fac_result_AB <- speed(df_fac,
                       swap = "combination",
                       swap_within = "block",
                       obj_function = objective_function_factorial,
                       factors = list(A = "A", B = "B"),
                       iterations = 50000,
                       early_stop_iterations = 10000,
                       interaction_weight = 2,
                       quiet = FALSE, seed = 42)

fac_result_AB$design_df$A <- substr(fac_result$design_df$combination, 1, 2)
fac_result_AB$design_df$B <- substr(fac_result$design_df$combination, 4, 5)
autoplot(fac_result_AB, treatment = A)
autoplot(fac_result_AB, treatment = B)
# fac_result$design_df$combination <- paste(fac_result$design_df$A, fac_result$design_df$B, sep = "_")
autoplot(fac_result_AB, treatment = combination)

calculate_adjacency_score(fac_result_AB$design_df, "combination")
calculate_adjacency_score(fac_result_AB$design_df, "A")
calculate_adjacency_score(fac_result_AB$design_df, "B")


options(speed.random_initialisation = F, speed.cooling_rate = 0.99)
fac_result <- speed(df_fac,
                    swap = "combination",
                    swap_within = "block",
                    # obj_function = objective_function_factorial,
                    # factors = list(A = "A", B = "B"),
                    # iterations = 50000,
                    # early_stop_iterations = 10000,
                    # interaction_weight = 0.001
                    quiet = FALSE, seed = 42)

fac_result$design_df$A <- substr(fac_result$design_df$combination, 1, 2)
fac_result$design_df$B <- substr(fac_result$design_df$combination, 4, 5)
autoplot(fac_result, treatment = A)
autoplot(fac_result, treatment = B)
# fac_result$design_df$combination <- paste(fac_result$design_df$A, fac_result$design_df$B, sep = "_")
autoplot(fac_result, treatment = combination)


calculate_adjacency_score(fac_result$design_df, "combination")
calculate_adjacency_score(fac_result$design_df, "A")
calculate_adjacency_score(fac_result$design_df, "B")
